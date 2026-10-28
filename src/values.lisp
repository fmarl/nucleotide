;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defmacro define-value-type (name description &key (type nil type-p) check)
  "Define the type NAME of the values of TYPE, or of those that the function named
CHECK accepts. Errors describe its values as DESCRIPTION."
  (assert (if type-p (null check) check) ()
          "~S needs either a type or a check" name)
  (let ((predicate (and check (intern (format nil "~A-P" name)))))
    `(progn
       ,@(when check
           `((defun ,predicate (value)
               (handler-case (progn (,check value) t)
                 (nucleotide-error () nil)))))
       (deftype ,name () ',(if check `(satisfies ,predicate) type))
       (setf (get ',name 'value-description) ,description
             (get ',name 'value-check) ',check)
       ',name)))

(defun type-description (type)
  (or (and (symbolp type) (get type 'value-description))
      (format nil "of type ~S" type)))

(defun check-value (value type &optional (description (type-description type)))
  (let ((check (and (symbolp type) (get type 'value-check))))
    (cond (check (funcall check value))
          ((not (typep value type))
           (nucleotide-error "~S is not ~A" value description)))))

(defun type-check (type &optional (description (type-description type)))
  "A check for settings that rejects values not of TYPE."
  (lambda (value)
    (check-value value type description)))

(defun argv-p (object)
  (and (consp object) (null (cdr (last object))) (every #'stringp object)))

(define-value-type argv "a list of strings like (\"foot\" \"-e\" \"htop\")"
  :type (satisfies argv-p))

(define-value-type program
    "a command line like \"foot -e htop\" or a list of strings like (\"foot\" \"-e\")"
  :type (or string argv))
