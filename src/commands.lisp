;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *commands* '())

(defvar *builtin-commands* '())

(defmacro define-command (name (wm &rest lambda-list) &body body)
  "Define NAME as a function of the window manager WM and LAMBDA-LIST that
bindings can run."
  `(progn
     (defun ,name (,wm ,@lambda-list) ,@body)
     (pushnew ',name *commands*)
     ',name))

(defvar *argument-checks* '()
  "Alist from commands to functions that signal an error for arguments the
command cannot take, so that bad bindings fail when the init file loads.")

(defun check-arguments (command function)
  (setf *argument-checks* (alist-set command function *argument-checks*)))

(defun normalize-action (action)
  (let ((action (if (listp action) action (list action))))
    (unless (member (first action) *commands*)
      (nucleotide-error "unknown command ~S" (first action)))
    (let ((check (alist-get (first action) *argument-checks*)))
      (when check
        (apply check (rest action))))
    action))

(defun run-command (wm action)
  (destructuring-bind (name . arguments) (normalize-action action)
    (unless (eq name 'toggle-highlight)
      (end-highlight wm))
    (apply name wm arguments)))

(defun command-names ()
  (sort (copy-list *commands*) #'string<))
