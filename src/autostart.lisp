;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *autostart-programs*
    '(("waybar"))
  :documentation "Commands started once at startup, in the format SPAWN takes.")

(defvar *autostarted* nil)

(defun run-autostart ()
  (unless *autostarted*
    (setf *autostarted* t)
    (dolist (program *autostart-programs*)
      (spawn-program program))))

(define-command spawn (wm &rest command)
  "Start COMMAND, a shell command line or a program and its arguments."
  (declare (ignore wm))
  (spawn-program (if (rest command) command (first command))))
