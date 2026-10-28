;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *commands* '())

(defvar *builtin-commands* '())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parameter-variable (parameter)
    (let ((name (if (consp parameter) (first parameter) parameter)))
      (if (consp name) (second name) name)))

  (defun without-defaults (lambda-list)
    (loop with optional = nil
          for parameter in (ldiff lambda-list (member '&aux lambda-list))
          for supplied = (gensym "SUPPLIED")
          if (member parameter lambda-list-keywords)
            do (setf optional (member parameter '(&optional &key)))
            and collect parameter into parameters
          else if optional
                 collect (list (if (consp parameter) (first parameter) parameter) nil supplied)
                   into parameters
                 and collect (cons (parameter-variable parameter) supplied) into given
          else
            collect parameter into parameters
            and collect (cons parameter t) into given
          finally (return (values parameters given))))

  (defun declared-types (body variables)
    (loop for form in body
          while (or (stringp form) (and (consp form) (eq 'declare (first form))))
          when (consp form)
          append (loop for (kind type . declared) in (rest form)
                       when (eq kind 'type)
                       append (loop for variable in declared
                                    when (member variable variables)
                                    collect (list variable type))))))

(defmacro define-command (name (wm &rest lambda-list) &body body)
  "Define NAME as a function of the window manager WM and LAMBDA-LIST that
bindings can run. Binding NAME checks the arguments it gets against LAMBDA-LIST
and the TYPE declarations of BODY."
  (multiple-value-bind (parameters given) (without-defaults lambda-list)
    `(progn
       (defun ,name (,wm ,@lambda-list) ,@body)
       (setf (get ',name 'argument-types)
             (lambda ,parameters
               (declare (ignorable ,@(mapcar #'car given)
                                   ,@(remove t (mapcar #'cdr given))))
               (remove nil (list ,@(loop for (variable type)
                                           in (declared-types body (mapcar #'car given))
                                         collect `(and ,(cdr (assoc variable given))
                                                       (cons ,variable ',type)))))))
       (pushnew ',name *commands*)
       ',name)))

(define-value-type direction "a direction like :left, :right, :up, :down, :prev or :next"
  :type (member :left :right :up :down :prev :next))

(define-value-type horizontal-direction "a direction like :left, :right, :prev or :next"
  :type (member :left :right :prev :next))

(define-value-type workspace-number "a workspace number like 1"
  :type (integer 1))

(defun check-arguments (action)
  (destructuring-bind (name &rest arguments) action
    (unless (member name *commands*)
      (nucleotide-error "unknown command ~S" name))
    (let ((typed (handler-case (apply (get name 'argument-types) arguments)
                   (program-error ()
                     (nucleotide-error "~S has the wrong number of arguments" action)))))
      (handler-case (loop for (value . type) in typed
                          do (check-value value type))
        (nucleotide-error (c)
          (nucleotide-error "~S: ~A" action c))))))

(defun normalize-action (action)
  (let ((action (if (consp action) action (list action))))
    (check-arguments action)
    action))

(defun run-command (wm action)
  (destructuring-bind (name . arguments) (normalize-action action)
    (unless (eq name 'toggle-highlight)
      (end-highlight wm))
    (apply name wm arguments)))

(defun command-names ()
  (sort (copy-list *commands*) #'string<))
