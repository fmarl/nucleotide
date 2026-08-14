;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defparameter *autostart-programs*
  '(("waybar")))

(defun run-autostart ()
  (dolist (program *autostart-programs*)
    (uiop:launch-program program)))
