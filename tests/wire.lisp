;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite wire :in :nucleotide)
(in-suite wire)

(test u32-is-little-endian
  (let ((buf (buffer 4)))
    (setf (n::u32ref buf 0) #x12345678)
    (is (equalp (octets #x78 #x56 #x34 #x12) buf))
    (is (= #x12345678 (n::u32ref buf 0)))))

(test i32-roundtrip
  (let ((buf (buffer 4)))
    (dolist (value '(0 1 -1 2147483647 -2147483648))
      (setf (n::i32ref buf 0) value)
      (is (= value (n::i32ref buf 0))))))

(test pad4
  (is (equal '(0 4 4 4 4 8) (mapcar #'n::pad4 '(0 1 2 3 4 5)))))

(test string-roundtrip
  (let ((buf (buffer 64)))
    (dolist (string '("" "a" "abc" "abcd" "grüße"))
      (let ((end (n::put-wl-string buf 0 string)))
        (is (zerop (mod end 4)))
        (multiple-value-bind (decoded next) (n::get-wl-string buf 0)
          (is (string= string decoded))
          (is (= end next)))))))

(test null-string
  (let ((buf (buffer 8)))
    (is (= 4 (n::put-wl-string buf 0 nil)))
    (is (null (n::get-wl-string buf 0)))))

(test array-roundtrip
  (let ((buf (buffer 64))
        (bytes (octets 1 2 3 4 5)))
    (let ((end (n::put-wl-array buf 0 bytes)))
      (is (= 12 end))
      (multiple-value-bind (decoded next) (n::get-wl-array buf 0)
        (is (equalp bytes decoded))
        (is (= end next))))))
