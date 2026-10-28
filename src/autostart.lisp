;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun check-programs (programs)
  (unless (and (listp programs) (null (cdr (last programs))))
    (nucleotide-error "~S is not a list of programs like (\"waybar\")" programs))
  (dolist (program programs)
    (check-value program 'program)))

(define-setting *autostart-programs*
    '(("waybar"))
  :documentation "Programs started once at startup, in the format SPAWN takes."
  :check #'check-programs)

(defvar *autostarted* nil)

(defun run-autostart ()
  (unless *autostarted*
    (setf *autostarted* t)
    (dolist (program *autostart-programs*)
      (spawn-program program))))

(define-command spawn (wm &rest command)
  "Start COMMAND, a shell command line or a program and its arguments."
  (declare (ignore wm) (type argv command))
  (spawn-program (if (rest command) command (first command))))
