;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun user-config-file ()
  (uiop:xdg-config-home "nucleotide" "init.lisp"))

(defun config-variables ()
  (list* '*commands* '*bindings* '*window-rules* '*hooks* '*failed-hooks*
         (mapcar #'car *settings*)))

(defun default-config ()
  (list* (copy-list *builtin-commands*) '() '() '() '() (setting-defaults)))

(defun call-with-default-config (function)
  "Call FUNCTION with the configuration at its defaults, bound in this thread
only, and return the configuration it leaves as an alist."
  (let ((variables (config-variables)))
    (progv variables (default-config)
      (funcall function)
      (check-settings)
      (mapcar (lambda (variable) (cons variable (symbol-value variable))) variables))))

(defun install-config (config)
  (loop for (variable . value) in config
        do (setf (symbol-value variable) value)))

(defun load-config-file (file)
  (check-file-permissions (uiop:pathname-directory-pathname file) #o022 :allow-root t)
  (check-file-permissions file #o022 :allow-root t)
  (let ((*package* (find-package '#:nucleotide.user)))
    (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
      (load file))))

(defun load-user-config ()
  "Install the init file, or the defaults without one. Returns T, NIL without a
file, or :ERROR, which keeps the current configuration."
  (let ((file (and (probe-file (user-config-file)) (user-config-file))))
    (handler-case
        (progn
          (install-config (call-with-default-config
                           (if file
                               (lambda () (load-config-file file))
                               #'use-defaults)))
          (and file t))
      (error (c)
        (notify "~A not loaded: ~A" file c)
        (unless *bindings*
          (install-config (call-with-default-config #'use-defaults)))
        :error))))

(define-command reload-config (wm)
  "Load the init file again, starting from the defaults."
  (unless (eq (load-user-config) :error)
    (export-cursor-theme)
    (set-cursor-theme wm)
    (when (wm-seat wm)
      (uninstall-all-keybinds wm)
      (install-all-keybinds wm))
    (mark-dirty wm)
    (notify "configuration reloaded")))
