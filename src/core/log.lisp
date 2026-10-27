;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *log-levels* '(:debug :info :warn :error))

(defvar *log-level* :info
  "The least severe level that is printed: :DEBUG, :INFO, :WARN or :ERROR.")

(defun log-message (level control &rest args)
  (when (>= (position level *log-levels*) (position *log-level* *log-levels*))
    (format *error-output* "~&nucleotide ~(~A~): ~?~%" level control args)))
