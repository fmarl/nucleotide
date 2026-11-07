;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-condition config-error (nucleotide-error)
  ((context :initform '() :reader config-error-context :accessor %config-error-context))
  (:documentation "The configuration asks for something nucleotide does not support.
CONTEXT says where, from the outside in, as a list of (KIND . OBJECT) with KIND
:SETTING, :ACTION, :RULE or :LIBINPUT-SETTING.")
  (:report (lambda (c stream)
             (loop :for (kind . object) :in (config-error-context c)
                   :do (ecase kind
                         (:setting (format stream "~(~A~): " object))
                         (:rule (format stream "rule ~S: " object))
                         ((:action :libinput-setting) (format stream "~S: " object))))
             (report-config-problem c stream))))

(defgeneric report-config-problem (condition stream))

(define-condition simple-config-error (config-error simple-condition) ())

(defmethod report-config-problem ((c simple-config-error) stream)
  (apply #'format stream (simple-condition-format-control c)
         (simple-condition-format-arguments c)))

(defun config-error (control &rest args)
  "Signal a CONFIG-ERROR whose message is CONTROL formatted with ARGS."
  (error 'simple-config-error :format-control control :format-arguments args))

(define-condition invalid-value (config-error)
  ((value :initarg :value :reader invalid-value-value)
   (expected :initarg :expected :reader invalid-value-expected))
  (:documentation "VALUE is not EXPECTED, a description like \"a number of pixels\"."))

(defmethod report-config-problem ((c invalid-value) stream)
  (format stream "~S is not ~A" (invalid-value-value c) (invalid-value-expected c)))

(defun invalid-value (value expected)
  "Signal that VALUE is not EXPECTED, a description like \"a number of pixels\"."
  (error 'invalid-value :value value :expected expected))

(define-condition unknown-name (config-error)
  ((kind :initarg :kind :reader unknown-name-kind)
   (name :initarg :name :reader unknown-name-name)
   (known :initarg :known :initform '() :reader unknown-name-known))
  (:documentation "NAME is not a known KIND, like a key or a command. KNOWN lists the
valid names if there are few."))

(defmethod report-config-problem ((c unknown-name) stream)
  (format stream "unknown ~A ~S~@[, expected one of ~{~S~^, ~}~]"
          (unknown-name-kind c) (unknown-name-name c) (unknown-name-known c)))

(defun unknown-name (kind name &optional known)
  "Signal that NAME is not a known KIND, like \"key\". KNOWN lists the valid names."
  (error 'unknown-name :kind kind :name name :known known))

(defmacro with-config-context ((kind object) &body body)
  "Run BODY, adding (KIND . OBJECT) to the context of the config errors it signals."
  (let ((entry (gensym "ENTRY"))
        (condition (gensym "CONDITION")))
    `(let ((,entry (cons ,kind ,object)))
       (handler-bind ((config-error
                        (lambda (,condition)
                          (push ,entry (%config-error-context ,condition)))))
         ,@body))))

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
                 (config-error () nil)))))
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
           (invalid-value value description)))))

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
