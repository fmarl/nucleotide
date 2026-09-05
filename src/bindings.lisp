;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defgeneric enable-binding (binding)
  (:method ((binding river-xkb-binding-v1))
    (river-xkb-binding-v1.enable binding))
  (:method ((binding river-pointer-binding-v1))
    (river-pointer-binding-v1.enable binding)))

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
        (proxy-hooks binding)))

(defun register-binding (wm binding action)
  (on-press binding action)
  (push binding (wm-bindings wm))
  (push binding (wm-pending-bindings wm))
  (mark-dirty wm)
  binding)

(defun bind-key (wm keysym modifiers action)
  (register-binding wm
                    (river-xkb-bindings-v1.get-xkb-binding
                     (wm-xkb wm) (wm-seat wm) keysym modifiers)
                    action))

(defun bind-pointer (wm button modifiers action)
  (register-binding wm
                    (river-seat-v1.get-pointer-binding (wm-seat wm) button modifiers)
                    action))

(defun bind-key-chord (wm submap keysym modifiers action)
  "Bindings of a submap are only enabled while the submap is active."
  (let ((binding (river-xkb-bindings-v1.get-xkb-binding
                  (wm-xkb wm) (wm-seat wm) keysym modifiers)))
    (on-press binding (lambda ()
                        (leave-submap wm)
                        (funcall action)))
    (push binding (wm-bindings wm))
    (push binding (submap-bindings submap))
    binding))

(defun install-submap (wm keysym modifiers chords)
  (let ((submap (make-submap)))
    (dolist (chord (reverse chords))
      (destructuring-bind ((chord-keysym . chord-modifiers) . function) chord
        (bind-key-chord wm submap chord-keysym chord-modifiers
                        (lambda () (funcall function wm)))))
    (bind-key wm keysym modifiers (lambda () (enter-submap wm submap)))))

(defun install-all-keybinds (wm)
  (unless (wm-xkb wm)
    (warn "river_xkb_bindings_v1 is not available, no keybindings installed")
    (return-from install-all-keybinds))
  (dolist (entry (reverse *keybinds*))
    (destructuring-bind ((keysym . modifiers) . function) entry
      (bind-key wm keysym modifiers (lambda () (funcall function wm)))))
  (dolist (entry (reverse *key-chords*))
    (destructuring-bind ((keysym . modifiers) . chords) entry
      (install-submap wm keysym modifiers chords)))
  (install-pointer-bindings wm))

(defun uninstall-all-keybinds (wm)
  (setf (wm-active-submap wm) nil
        (wm-pending-submap-ops wm) '()
        (wm-pending-bindings wm) '())
  (mapc #'destroy-binding (drain (wm-bindings wm))))
