;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:n #:nucleotide)
                    (#:xml #:nucleotide.xml)))

(in-package #:nucleotide/tests)

(def-suite :nucleotide)

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun buffer (size)
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
