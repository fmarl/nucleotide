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

(defvar *failed-hooks* '())

(defun run-event-hooks (event &rest args)
  (dolist (function (alist-get event *hooks*))
    (handler-case (apply function args)
      (error (c)
        (cond ((member function *failed-hooks*)
               (log-message :error "~(~S~) hook failed: ~A" event c))
              (t (push function *failed-hooks*)
                 (notify "~(~S~) hook failed: ~A" event c)))))))
