;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun user-config-file ()
  (uiop:xdg-config-home "nucleotide" "init.lisp"))

(defun reset-config ()
  (reset-settings)
  (setf *bindings* '()
        *window-rules* '()
        *hooks* '()))

(defun save-config ()
  (list* (cons '*bindings* *bindings*)
         (cons '*window-rules* *window-rules*)
         (cons '*hooks* *hooks*)
         (loop for (name) in *settings*
               collect (cons name (symbol-value name)))))

(defun restore-config (saved)
  (loop for (name . value) in saved
        do (setf (symbol-value name) value)))

(defun load-config-file (file)
  (let ((*package* (find-package '#:nucleotide)))
    (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
      (load file)))
  (check-settings))

(defun load-user-config ()
  "Load the init file from scratch, or the defaults without one. Returns T
when it loaded, NIL when there is none, and :ERROR when loading failed; then
the configuration stays as it was, or is the defaults at startup."
  (let ((file (user-config-file))
        (saved (save-config)))
    (reset-config)
    (cond ((probe-file file)
           (handler-case
               (progn
                 (check-file-permissions file #o022 :allow-root t)
                 (load-config-file file)
                 t)
             (error (c)
               (notify "~A not loaded: ~A" file c)
               (restore-config saved)
               (unless *bindings*
                 (use-defaults))
               :error)))
          (t
           (use-defaults)
           nil))))

(define-command reload-config (wm)
  "Load the init file from scratch and reinstall all keybindings, in the WM
thread."
  (unless (eq (load-user-config) :error)
    (when (wm-seat wm)
      (uninstall-all-keybinds wm)
      (install-all-keybinds wm))
    (mark-dirty wm)
    (notify "configuration reloaded")))
