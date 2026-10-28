;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *settings* '()
  "Alist from the symbol of each setting to a plist of :DEFAULT, a function
returning its default value, and :CHECK.")

(defmacro define-setting (name default &key documentation check)
  "Define the setting NAME. Every configuration starts from DEFAULT, and CHECK is
called with its value after the init file loaded."
  `(progn
     (defparameter ,name ,default ,@(and documentation (list documentation)))
     (setf *settings* (alist-set ',name
                                 (list :default (lambda () ,default)
                                       :check ,check)
                                 *settings*))
     ',name))

(defun setting-defaults ()
  (loop for (nil . properties) in *settings*
        collect (funcall (getf properties :default))))

(defun check-settings ()
  (loop for (name . properties) in *settings*
        for check = (getf properties :check)
        when check
        do (handler-case (funcall check (symbol-value name))
             (error (c)
               (nucleotide-error "~(~A~): ~A" name c)))))
