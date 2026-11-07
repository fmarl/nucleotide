;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite wire :in :nucleotide)
(in-suite wire)

(test u32-is-little-endian
  (let ((buf (buffer 4)))
    (setf (w::u32ref buf 0) #x12345678)
    (is (equalp (octets #x78 #x56 #x34 #x12) buf))
    (is (= #x12345678 (w::u32ref buf 0)))))

(test i32-roundtrip
  (let ((buf (buffer 4)))
    (dolist (value '(0 1 -1 2147483647 -2147483648))
      (setf (w::i32ref buf 0) value)
      (is (= value (w::i32ref buf 0))))))

(test pad4
  (is (equal '(0 4 4 4 4 8) (mapcar #'w::pad4 '(0 1 2 3 4 5)))))

(test string-roundtrip
  (let ((buf (buffer 64)))
    (dolist (string '("" "a" "abc" "abcd" "grüße"))
      (let ((end (w::put-wl-string buf 0 string)))
        (is (zerop (mod end 4)))
        (multiple-value-bind (decoded next) (w::get-wl-string buf 0)
          (is (string= string decoded))
          (is (= end next)))))))

(defun dirty-buffer (size)
  "A buffer whose bytes differ from everything the encoders write."
  (make-array size :element-type '(unsigned-byte 8) :initial-element #xff))

(test null-string
  (let ((buf (dirty-buffer 8)))
    (is (= 4 (w::put-wl-string buf 0 nil)))
    (is (equalp (octets 0 0 0 0 #xff #xff #xff #xff) buf))
    (is (equal '(nil 4) (multiple-value-list (w::get-wl-string buf 0))))
    (is (equal '(nil 8) (multiple-value-list (w::get-wl-string (buffer 8) 4))))))

(test array-roundtrip
  (let ((buf (buffer 64))
        (bytes (octets 1 2 3 4 5)))
    (let ((end (w::put-wl-array buf 0 bytes)))
      (is (= 12 end))
      (multiple-value-bind (decoded next) (w::get-wl-array buf 0)
        (is (equalp bytes decoded))
        (is (= end next))))))

(test strings-carry-their-length-with-the-nul-and-pad-with-zeros
  (let ((buf (dirty-buffer 16)))
    (is (= 12 (w::put-wl-string buf 0 "abcd")))
    (is (equalp (octets 5 0 0 0 97 98 99 100 0 0 0 0 #xff #xff #xff #xff) buf))))

(test arrays-carry-their-length-and-pad-with-zeros
  (let ((buf (dirty-buffer 20)))
    (is (= 16 (w::put-wl-array buf 4 (octets 1 2 3 4 5))))
    (is (equalp (octets #xff #xff #xff #xff 5 0 0 0 1 2 3 4 5 0 0 0
                        #xff #xff #xff #xff)
                buf))))

(test arguments-that-just-fit-are-written
  (is (= 4 (w::put-wl-string (buffer 4) 0 nil)))
  (is (= 12 (w::put-wl-string (buffer 12) 0 "abcdefg")))
  (is (= 12 (w::put-wl-array (buffer 12) 0 (buffer 8))))
  (signals w::nucleotide-error (w::put-wl-string (buffer 3) 0 nil))
  (signals w::nucleotide-error (w::put-wl-string (buffer 12) 0 "abcdefgh"))
  (signals w::nucleotide-error (w::put-wl-array (buffer 12) 0 (buffer 9))))

(test negative-ints-are-twos-complement
  (let ((buf (buffer 4)))
    (setf (w::i32ref buf 0) -2)
    (is (equalp (octets #xfe #xff #xff #xff) buf))))

(test overlong-messages-are-refused
  (let ((buffer (buffer 16)))
    (signals w::nucleotide-error (w::put-wl-string buffer 0 (make-string 20 :initial-element #\a)))
    (signals w::nucleotide-error (w::put-wl-array buffer 8 (buffer 8)))
    (signals w::nucleotide-error (w::encode-arg buffer 16 :uint 1))
    (is (= 16 (w::put-wl-string buffer 0 "0123456789")))))

(test invalid-utf-8-becomes-replacement-characters
  (let ((title (w::get-wl-string (octets 5 0 0 0 #x61 #xff #xfe #x62 0 0 0 0) 0)))
    (is (char= #\a (char title 0)))
    (is (char= #\b (char title (1- (length title)))))
    (is (find #\REPLACEMENT_CHARACTER title))))

(defun header (sender opcode size)
  (let ((buf (buffer 8)))
    (setf (w::u32ref buf 0) sender
          (w::u32ref buf 4) (logior (ash size 16) opcode))
    buf))

(defun buffered-connection (&rest chunks)
  "A connection whose read buffer holds 16 consumed bytes, then CHUNKS."
  (let ((conn (w::make-wire-connection nil))
        (bytes (apply #'concatenate '(vector (unsigned-byte 8)) (dirty-buffer 16) chunks)))
    (replace (w::connection-rbuf conn) bytes)
    (setf (w::connection-rstart conn) 16
          (w::connection-rend conn) (length bytes))
    conn))

(test buffered-message-size-waits-for-complete-messages
  (is (null (w::buffered-message-size (buffered-connection (buffer 7)))))
  (is (null (w::buffered-message-size (buffered-connection (header 1 0 12)))))
  (is (= 12 (w::buffered-message-size (buffered-connection (header 1 0 12) (buffer 4)))))
  (is (null (w::buffered-message-size
             (buffered-connection (header 1 0 w::+max-message-size+))))))

(test buffered-message-size-reports-malformed-sizes-at-once
  (is (= 4 (w::buffered-message-size (buffered-connection (header 1 0 4)))))
  (is (= (1+ w::+max-message-size+)
         (w::buffered-message-size
          (buffered-connection (header 1 0 (1+ w::+max-message-size+)))))))

(test peek-message-splits-the-header
  (let ((conn (buffered-connection (header 7 3 16) (octets 1 2 3 4 5 6 7 8))))
    (is (equal '(7 3 24 8) (multiple-value-list (w::peek-message conn))))
    (w::consume-message conn 16)
    (is (= 32 (w::connection-rstart conn)))))

(test peek-message-refuses-malformed-sizes
  (signals w::wl-protocol-error (w::peek-message (buffered-connection (header 1 0 4))))
  (signals w::wl-protocol-error
    (w::peek-message (buffered-connection (header 1 0 (1+ w::+max-message-size+))))))

(defun call-with-socket-pair (function)
  "Call FUNCTION with a writing socket and a connection reading from it."
  (with-private-directory (directory)
    (let ((path (format nil "~A/socket" directory))
          (listener (make-instance 'sb-bsd-sockets:local-socket :type :stream))
          (writer (make-instance 'sb-bsd-sockets:local-socket :type :stream))
          (reader nil))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind listener path)
             (sb-bsd-sockets:socket-listen listener 1)
             (sb-bsd-sockets:socket-connect writer path)
             (setf reader (sb-bsd-sockets:socket-accept listener))
             ;; A broken read path loops instead of failing.
             (sb-ext:with-timeout 10
               (funcall function writer (w::make-wire-connection reader))))
        (sb-bsd-sockets:socket-close listener)
        (sb-bsd-sockets:socket-close writer)
        (when reader (sb-bsd-sockets:socket-close reader))))))

(defun test-message (index)
  "Message INDEX: sender INDEX, opcode INDEX mod 7, INDEX mod 20 body words."
  (let ((words (loop :for k :below (mod index 20)
                     :collect (ldb (byte 32 0) (* (1+ index) (1+ k) 2654435761)))))
    (apply #'concatenate '(vector (unsigned-byte 8))
           (header index (mod index 7) (+ 8 (* 4 (length words))))
           (mapcar (lambda (word)
                     (let ((buf (buffer 4)))
                       (setf (w::u32ref buf 0) word)
                       buf))
                   words))))

(test messages-arrive-whole-however-the-stream-is-split
  ;; Well over the read buffer's 64 KiB, so it is compacted many times.
  (let* ((messages (loop :for i :below 3000 :collect (test-message i)))
         (stream (apply #'concatenate '(vector (unsigned-byte 8)) messages))
         (wrong '()))
    (call-with-socket-pair
     (lambda (writer conn)
       (let ((writing (sb-thread:make-thread
                      (lambda ()
                        (loop :for start = 0 :then end
                              :for i :from 0
                              :for end = (min (length stream) (+ start 1 (mod (* i 37) 701)))
                              :while (< start (length stream))
                              :do (sb-bsd-sockets:socket-send
                                   writer (subseq stream start end) nil))))))
         (loop :for expected :in messages
               :for i :from 0
               :do (multiple-value-bind (sender opcode body-offset body-size)
                       (w::peek-message conn)
                     (unless (and (= sender i)
                                  (= opcode (mod i 7))
                                  (equalp expected
                                          (subseq (w::connection-rbuf conn)
                                                  (- body-offset 8)
                                                  (+ body-offset body-size))))
                       (push i wrong))
                     (w::consume-message conn (+ 8 body-size))))
         (sb-thread:join-thread writing))))
    (is (null wrong) "messages read wrongly: ~S" (reverse wrong))))

(test a-hang-up-mid-message-disconnects
  (call-with-socket-pair
   (lambda (writer conn)
     (sb-bsd-sockets:socket-send writer (subseq (header 1 0 16) 0 6) nil)
     (sb-bsd-sockets:socket-shutdown writer :direction :output)
     (signals w::wl-disconnected (w::peek-message conn)))))
