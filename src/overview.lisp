;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-command toggle-overview (wm)
  "Open or close the window overview."
  (setf (wm-overview wm) (not (wm-overview wm)))
  (mark-dirty wm))

(defun close-overview (wm)
  (when (wm-overview wm)
    (setf (wm-overview wm) nil)
    (mark-dirty wm)))
