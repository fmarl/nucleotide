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

(defun add-binding (wm binding action)
  (on-press binding action)
  (push binding (wm-bindings wm))
  binding)

(defun bind-key (wm keysym modifiers action)
  (let ((binding (add-binding wm (river-xkb-bindings-v1.get-xkb-binding
                                  (wm-xkb wm) (wm-seat wm) keysym modifiers)
                              action)))
    (request wm :enable-binding binding)))

(defun bind-pointer (wm button modifiers action)
  (let ((binding (add-binding wm (river-seat-v1.get-pointer-binding
                                  (wm-seat wm) button modifiers)
                              action)))
    (request wm :enable-binding binding)))

(defun bind-key-chord (wm submap keysym modifiers action)
  "Bindings of a submap are only enabled while the submap is active."
  (let ((binding (add-binding wm (river-xkb-bindings-v1.get-xkb-binding
                                  (wm-xkb wm) (wm-seat wm) keysym modifiers)
                              (lambda ()
                                (leave-submap wm)
                                (funcall action)))))
    (push binding (submap-bindings submap))))

(defun prefix-submap (wm submaps prefix)
  (or (gethash prefix submaps)
      (let ((submap (make-submap)))
        (bind-key wm (car prefix) (cdr prefix) (lambda () (enter-submap wm submap)))
        (setf (gethash prefix submaps) submap))))

(defun install-all-keybinds (wm)
  (if (wm-xkb wm)
      (let ((submaps (make-hash-table :test #'equal)))
        (dolist (entry *bindings*)
          (destructuring-bind ((first &optional second) . action) entry
            (let ((run (lambda () (run-command wm action))))
              (if second
                  (bind-key-chord wm (prefix-submap wm submaps first)
                                  (car second) (cdr second) run)
                  (bind-key wm (car first) (cdr first) run))))))
      (log-message :warn "river_xkb_bindings_v1 is not available, no keybindings installed"))
  (install-pointer-bindings wm)
  (mark-dirty wm))

(defun uninstall-all-keybinds (wm)
  (setf (wm-active-submap wm) nil)
  (cancel-requests wm '(:enable-binding :enter-submap :leave-submap))
  (mapc #'destroy-binding (drain (wm-bindings wm))))

(defun enter-submap (wm submap)
  (setf (wm-active-submap wm) submap)
  (request wm :enter-submap submap)
  (mark-dirty wm))

(defun leave-submap (wm)
  (let ((submap (wm-active-submap wm)))
    (when submap
      (setf (wm-active-submap wm) nil)
      (request wm :leave-submap submap)
      (mark-dirty wm))))
