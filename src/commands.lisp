;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *commands* '()
  "The functions that bindings may run.")

(defvar *builtin-commands* '())

(defmacro define-command (name (wm &rest lambda-list) &body body)
  "Define NAME as a function of the window manager WM and LAMBDA-LIST that
bindings can run."
  `(progn
     (defun ,name (,wm ,@lambda-list) ,@body)
     (pushnew ',name *commands*)
     ',name))

(defun normalize-action (action)
  "ACTION, a command name or a list of one and its arguments, as a list."
  (let ((action (if (listp action) action (list action))))
    (unless (member (first action) *commands*)
      (nucleotide-error "unknown command ~S" (first action)))
    action))

(defun run-command (wm action)
  (destructuring-bind (name . arguments) (normalize-action action)
    (unless (eq name 'toggle-highlight)
      (end-highlight wm))
    (apply name wm arguments)))

(defun command-names ()
  (sort (copy-list *commands*) #'string<))
