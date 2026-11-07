;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *settings* '()
  "Alist from the symbol of each setting to a plist of :DEFAULT, a function
returning its default value, and :TYPE, the value type of the setting.")

(defmacro define-setting (name default &key documentation (type t))
  "Define the setting NAME. Every configuration starts from DEFAULT, and the value
must be of the value type TYPE once the init file loaded."
  `(progn
     (defparameter ,name ,default ,@(and documentation (list documentation)))
     (setf *settings* (alist-set ',name
                                 (list :default (lambda () ,default)
                                       :type ',type)
                                 *settings*))
     ',name))

(defun setting-defaults ()
  (loop :for (nil . properties) :in *settings*
        :collect (funcall (getf properties :default))))

(defun check-settings ()
  (loop :for (name . properties) :in *settings*
        :do (with-config-context (:setting name)
              (check-value (symbol-value name) (getf properties :type)))))
