;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *keyboard-layout* nil
  "An xkb layout like \"de\" or \"us,de\"; NIL keeps river's default keymap.")

(defparameter *keyboard-variant* nil)
(defparameter *keyboard-options* nil)
(defparameter *keyboard-model* nil)
(defparameter *numlock* nil)

(defparameter *xkbcli* '("xkbcli" "compile-keymap"))

(defun compile-keymap ()
  (uiop:run-program
   (append *xkbcli*
           (loop for (flag value) in `(("--layout" ,*keyboard-layout*)
                                       ("--variant" ,*keyboard-variant*)
                                       ("--options" ,*keyboard-options*)
                                       ("--model" ,*keyboard-model*))
                 when value append (list flag value)))
   :output :string))

(defun keymap-fd (text)
  "An fd of an unlinked file holding TEXT, NUL-terminated as xkb expects."
  (multiple-value-bind (fd path)
      (sb-posix:mkstemp (format nil "~A/nucleotide-keymap-XXXXXX"
                                (or (uiop:getenv "XDG_RUNTIME_DIR") "/tmp")))
    (sb-posix:unlink path)
    (write-all fd (sb-ext:string-to-octets text :external-format :utf-8
                                           :null-terminate t))
    fd))

(defun configure-keyboards (wm)
  (when (and (wm-xkb-config wm) *keyboard-layout*)
    (handler-case
        (let ((fd (keymap-fd (compile-keymap))))
          (unwind-protect
               (forward-events wm (river-xkb-config-v1.create-keymap
                                   (wm-xkb-config wm) fd
                                   +river-xkb-config-v1-keymap-format-text-v1+))
            (sb-posix:close fd)))
      (error (c)
        (notify "keyboard layout ~A not set: ~A" *keyboard-layout* c)))))

(defun configure-keyboard (wm keyboard)
  (when (wm-keymap wm)
    (river-xkb-keyboard-v1.set-keymap keyboard (wm-keymap wm)))
  (when *numlock*
    (river-xkb-keyboard-v1.numlock-enable keyboard)))

(define-handler (wm (keymap river-xkb-keymap-v1) :success) ()
  (when (wm-keymap wm)
    (river-xkb-keymap-v1.destroy (wm-keymap wm)))
  (setf (wm-keymap wm) keymap)
  (dolist (keyboard (wm-keyboards wm))
    (configure-keyboard wm keyboard)))

(define-handler (wm (keymap river-xkb-keymap-v1) :failure) (message)
  (notify "keymap rejected: ~A" message)
  (river-xkb-keymap-v1.destroy keymap))

(define-handler (wm (config river-xkb-config-v1) :xkb-keyboard) (keyboard)
  (push keyboard (wm-keyboards wm))
  (forward-events wm keyboard)
  (configure-keyboard wm keyboard))

(define-handler (wm (keyboard river-xkb-keyboard-v1) :removed) ()
  (setf (wm-keyboards wm) (remove keyboard (wm-keyboards wm)))
  (river-xkb-keyboard-v1.destroy keyboard))
