;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *hook-events*
  '(:window-opened :window-closed :window-title :focus-changed :workspace-changed))

(defvar *hooks* '())

(defun on (event function)
  "Call FUNCTION on EVENT: :WINDOW-OPENED and :WINDOW-CLOSED with the window,
:WINDOW-TITLE with the window and its title, :FOCUS-CHANGED with the focused
window or NIL, and :WORKSPACE-CHANGED with the active workspace."
  (unless (member event *hook-events*)
    (nucleotide-error "unknown event ~S, expected one of ~S" event *hook-events*))
  (setf *hooks* (alist-set event
                           (append (remove function (alist-get event *hooks*))
                                   (list function))
                           *hooks*))
  function)

(defvar *failures* '())

(defun call-reporting-failure (key function arguments control &rest control-arguments)
  "Apply FUNCTION to ARGUMENTS, or return NIL if it fails. The first failure for
KEY is a notification about what CONTROL and CONTROL-ARGUMENTS describe, later
ones go to the log."
  (handler-case (apply function arguments)
    (error (c)
      (if (member key *failures* :test #'equal)
          (log-message :error "~? failed: ~A" control control-arguments c)
          (progn (push key *failures*)
                 (notify "~? failed: ~A" control control-arguments c)))
      nil)))

(defun run-event-hooks (event &rest args)
  (dolist (function (alist-get event *hooks*))
    (call-reporting-failure function function args "~(~S~) hook" event)))
