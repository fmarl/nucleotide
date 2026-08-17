;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +read-buffer-size+ #.(* 64 1024))
(defconstant +max-message-size+ 4096)

(define-condition wl-disconnected (error) ()
  (:report "the Wayland server closed the connection"))

(defstruct (connection (:constructor %make-connection))
  socket
  (rbuf (make-array +read-buffer-size+ :element-type '(unsigned-byte 8)) :type octets)
  (rstart 0 :type fixnum)
  (rend 0 :type fixnum)
  (wbuf (make-array +max-message-size+ :element-type '(unsigned-byte 8)) :type octets)
  (scratch (make-array 8192 :element-type '(unsigned-byte 8)) :type octets))

(defun make-wire-connection (socket)
  (%make-connection :socket socket))

(defun close-connection (connection)
  (sb-bsd-sockets:socket-close (connection-socket connection)))

(declaim (inline u32ref (setf u32ref) i32ref (setf i32ref) pad4))

(defun u32ref (buffer offset)
  (logior (aref buffer offset)
	  (ash (aref buffer (+ offset 1)) 8)
	  (ash (aref buffer (+ offset 2)) 16)
	  (ash (aref buffer (+ offset 3)) 24)))

(defun (setf u32ref) (value buffer offset)
  (setf (aref buffer offset) (ldb (byte 8 0) value)
	(aref buffer (+ offset 1)) (ldb (byte 8 8) value)
	(aref buffer (+ offset 2)) (ldb (byte 8 16) value)
	(aref buffer (+ offset 3)) (ldb (byte 8 24) value))
  value)

(defun i32ref (buffer offset)
  (let ((v (u32ref buffer offset)))
    (if (logbitp 31 v) (- #x100000000) v)))

(defun (setf i32ref) (value buffer offset)
  (setf (u32ref buffer offset) (ldb (byte 32 0) value))
  value)

(defun pad4 (n) (logand (+ n 3) -4))

(defun get-wl-string (buffer offset)
  (let ((len (u32ref buffer offset)))
    (if (zerop len)
	(values nil (+ offset 4))
	(values (sb-ext:octets-to-string buffer :external-format :utf-8
						:start (+ offset 4)
						:end (+ offset 4 (1- len)))
		(+ offset 4 (pad4 len))))))

(defun put-wl-string (buffer offset string)
  (if (null string)
      (progn (setf (u32ref buffer offset) 0)
	     (+ offset 4))
      (let* ((bytes (sb-ext:string-to-octets string :external-format :utf-8))
	     (len (1+ (length bytes))))
	(setf (u32ref buffer offset) len)
	(replace buffer bytes :start1 (+ offset 4))
	(fill buffer 0 :start (+ offset 4 (length bytes)) :end (+ offset 4 (pad4 len)))
	(+ offset 4 (pad4 len)))))

(defun get-wl-array (buffer offset)
  (let ((len (u32ref buffer offset)))
    (values (subseq buffer (+ offset 4) (+ offset 4 len))
	    (+ offset 4 (pad4 len)))))

(defun put-wl-array (buffer offset bytes)
  (let ((len (length bytes)))
    (setf (u32ref buffer offset) len)
    (replace buffer bytes :start1 (+ offset 4))
    (fill buffer 0 :start (+ offset 4 len) :end (+ offset 4 (pad4 len)))
    (+ offset 4 (pad4 len))))

(defun %fill-read-buffer (connection)
  (let ((rbuf (connection-rbuf connection)))
    (when (plusp (connection-rstart connection))
      (replace rbuf rbuf :start2 (connection-rstart connection)
			 :end2 (connection-rend connection))
      (setf (connection-rend connection)
	    (- (connection-rend connection) (connection-rstart connection))
	    (connection-rstart connection) 0))
    (let ((space (- +read-buffer-size+ (connection-rend connection))))
      (when (zerop space)
	(error "wire read buffer overflow"))
      (multiple-value-bind (buffer n)
	  (sb-bsd-sockets:socket-receive
	   (connection-socket connection) (connection-scratch connection)
	   (min space (length (connection-scratch connection))))
	(declare (ignore buffer))
	(when (or (null n) (zerop n))
	  (error 'wayland-disconnected))
	(replace rbuf (connection-scratch connection)
		 :start1 (connection-rend connection) :end2 n)
	(incf (connection-rend connection) n)))))

(defun ensure-bytes (connection n)
  (loop while (< (- (connection-rend connection) (connection-rstart connection)) n)
	do (%fill-read-buffer connection)))

(defun peek-message (connection)
  (ensure-bytes connection 8)
  (let ((size (ash (u32ref (connection-rbuf connection)
			   (+ (connection-rstart connection) 4))
		   -16)))
    (unless (<= 8 size +max-message-size+)
      (error "malformed message: size ~D" size))
    (ensure-bytes connection size)
    (let* ((rbuf (connection-rbuf connection))
	   (start (connection-rstart connection))
	   (word (u32ref rbuf (+ start 4))))
      (values (u32ref rbuf start)
	      (logand word #xffff)
	      (+ start 8)
	      (- size 8)))))

(defun consume-message (connection total-size)
  (incf (connection-rstart connection) total-size))

(defun buffered-message-size (connection)
  (let ((avail (- (connection-rend connection) (connection-rstart connection))))
    (when (>= avail 8)
      (let ((size (ash (u32ref (connection-rbuf connection)
			       (+ (connection-rstart connection) 4))
		       -16)))
	(when (>= avail size) size)))))

(defun send-raw (connection end)
  (let ((socket (connection-socket connection))
	(buffer (connection-wbuf connection))
	(sent 0))
    (loop while (< sent end)
	  do (incf sent (sb-bsd-sockets:socket-send
			 socket (subseq buffer sent end) nil)))))
