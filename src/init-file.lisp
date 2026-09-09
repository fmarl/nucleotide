;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun user-config-file ()
  (uiop:xdg-config-home "nucleotide" "init.lisp"))

(defun load-user-config ()
  "Load the user's init file. Returns T when it loaded, NIL when there is
none, and :ERROR when loading failed (the error is reported)."
  (let ((file (user-config-file)))
    (when (probe-file file)
      (handler-case
          (progn
            (check-file-permissions file #o022)
            (let ((*package* (find-package '#:nucleotide)))
              (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
                (load file)))
            t)
        (error (c)
          (notify "~A not loaded: ~A" file c)
          :error)))))

(defun reload-config (wm)
  "Reload the init file and reinstall all keybindings, in the WM thread."
  (unless (eq (load-user-config) :error)
    (when (wm-seat wm)
      (uninstall-all-keybinds wm)
      (install-all-keybinds wm))
    (configure-keyboards wm)
    (mark-dirty wm)
    (notify "configuration reloaded")))
