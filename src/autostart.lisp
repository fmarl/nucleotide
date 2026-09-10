;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *autostart-programs*
  '(("waybar"))
  "Commands started once, in the format SPAWN takes.")

(defvar *autostarted* nil)

(defun run-autostart ()
  (unless *autostarted*
    (setf *autostarted* t)
    (dolist (program *autostart-programs*)
      (spawn program))))
