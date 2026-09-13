;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defgeneric handle-event (wm object event &rest args)
  (:documentation "Handle EVENT (a keyword) sent to OBJECT, either a proxy or
the model object standing for it (a WIN, an OUTPUT-STATE). Define methods
with DEFINE-HANDLER; events without a method are ignored.")
  (:method (wm object event &rest args)
    (declare (ignore wm object event args))
    nil))

(defmacro define-handler ((wm (object class) event) lambda-list &body body)
  "Define how WM handles EVENT on OBJECT of CLASS. LAMBDA-LIST receives the
event's arguments."
  (let ((event-var (gensym "EVENT"))
        (args (gensym "ARGS")))
    `(defmethod handle-event (,wm (,object ,class) (,event-var (eql ,event))
                              &rest ,args)
       (declare (ignorable ,wm ,object))
       (destructuring-bind ,lambda-list ,args
         ,@body))))

(defun forward-events (wm proxy &optional (object proxy))
  (push (lambda (event &rest args)
          (apply #'handle-event wm object event args))
        (proxy-hooks proxy)))
