;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *settings* '()
  "Alist from the symbol of each setting to a plist of :DEFAULT, a function
returning its default value, and :CHECK.")

(defmacro define-setting (name default &key documentation check)
  "Define the setting NAME. RESET-SETTINGS gives it DEFAULT again, and CHECK is
called with its value after the init file loaded."
  `(progn
     (defparameter ,name ,default ,@(and documentation (list documentation)))
     (setf *settings* (alist-set ',name
                                 (list :default (lambda () ,default)
                                       :check ,check)
                                 *settings*))
     ',name))

(defun reset-settings ()
  (loop for (name . properties) in *settings*
        do (setf (symbol-value name) (funcall (getf properties :default)))))

(defun check-settings ()
  (loop for (name . properties) in *settings*
        for check = (getf properties :check)
        when check
        do (handler-case (funcall check (symbol-value name))
             (error (c)
               (nucleotide-error "~(~A~): ~A" name c)))))

(defun parse-color (color-code)
  (unless (and (stringp color-code)
               (member (length color-code) '(7 9))
               (char= #\# (char color-code 0)))
    (nucleotide-error "~S is not a color like \"#6f8cab\"" color-code))
  (append (loop for start from 1 below (length color-code) by 2
                collect (parse-integer color-code :start start :end (+ start 2) :radix 16))
          (and (= 7 (length color-code)) '(255))))
