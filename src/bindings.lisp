;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defstruct submap
  (bindings '()))

(defgeneric enable-binding (binding)
  (:method ((binding river-xkb-binding-v1))
    (river-xkb-binding-v1.enable binding))
  (:method ((binding river-pointer-binding-v1))
    (river-pointer-binding-v1.enable binding)))

(defgeneric disable-binding (binding)
  (:method ((binding river-xkb-binding-v1))
    (river-xkb-binding-v1.disable binding))
  (:method ((binding river-pointer-binding-v1))
    (river-pointer-binding-v1.disable binding)))

(defgeneric destroy-binding (binding)
  (:method ((binding river-xkb-binding-v1))
    (river-xkb-binding-v1.destroy binding))
  (:method ((binding river-pointer-binding-v1))
    (river-pointer-binding-v1.destroy binding)))

(defun on-press (binding action)
  (push (lambda (event &rest args)
          (declare (ignore args))
          (when (eq event :pressed)
            (funcall action)))
        (proxy-hooks binding))
  binding)

(defun add-binding (wm binding action)
  "Bindings outside of submaps are enabled with the next manage sequence."
  (push (on-press binding action) (wm-bindings wm))
  (request wm :enable-binding binding))

(define-setting *bindings-layout* nil
  :documentation "NIL to match key bindings with the active keyboard layout, or the
index of the layout to match them with, like 0. Only keys of that layout can
then be bound."
  :check (type-check '(or null (unsigned-byte 32)) "NIL or a layout index like 0"))

(defun make-key-binding (wm keysym modifiers)
  (let ((binding (river-xkb-bindings-v1.get-xkb-binding (wm-xkb wm) (wm-seat wm)
                                                        keysym modifiers)))
    (request wm :configure-binding binding)
    binding))

(defun bind-key (wm keysym modifiers action)
  (add-binding wm (make-key-binding wm keysym modifiers) action))

(defun bind-pointer (wm button modifiers action)
  (add-binding wm (river-seat-v1.get-pointer-binding (wm-seat wm) button modifiers)
               action))

(defun bind-key-chord (wm submap keysym modifiers action)
  "Bindings of a submap are only enabled while the submap is active."
  (push (on-press (make-key-binding wm keysym modifiers)
                  (lambda ()
                    (leave-submap wm)
                    (funcall action)))
        (submap-bindings submap)))

(defun prefix-submap (wm submaps prefix)
  (or (gethash prefix submaps)
      (let ((submap (make-submap)))
        (bind-key wm (car prefix) (cdr prefix) (lambda () (enter-submap wm submap)))
        (push submap (wm-submaps wm))
        (setf (gethash prefix submaps) submap))))

(defun all-bindings (wm)
  (append (wm-bindings wm) (mappend #'submap-bindings (wm-submaps wm))))

(defun binding-action (wm action)
  (lambda ()
    (call-reporting-failure action #'run-command (list wm action) "~(~S~)" action)))

(defun install-all-keybinds (wm)
  (if (wm-xkb wm)
      (let ((submaps (make-hash-table :test #'equal)))
        (dolist (entry *bindings*)
          (destructuring-bind ((first &optional second) . action) entry
            (let ((run (binding-action wm action)))
              (if second
                  (bind-key-chord wm (prefix-submap wm submaps first)
                                  (car second) (cdr second) run)
                  (bind-key wm (car first) (cdr first) run))))))
      (log-message :warn "river_xkb_bindings_v1 is not available, no keybindings installed"))
  (install-pointer-bindings wm)
  (mark-dirty wm))

(defun uninstall-all-keybinds (wm)
  (setf (wm-active-submap wm) nil)
  (cancel-requests wm '(:configure-binding :enable-binding :enter-submap :leave-submap))
  (mapc #'destroy-binding (all-bindings wm))
  (setf (wm-bindings wm) '()
        (wm-submaps wm) '()))

(defun enter-submap (wm submap)
  "A locked session has no submap."
  (unless (wm-locked wm)
    (setf (wm-active-submap wm) submap)
    (request wm :enter-submap submap)
    (mark-dirty wm)))

(defun leave-submap (wm)
  (let ((submap (wm-active-submap wm)))
    (when submap
      (setf (wm-active-submap wm) nil)
      (request wm :leave-submap submap)
      (mark-dirty wm))))
