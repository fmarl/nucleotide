;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defstruct submap
  (keys "")
  (entries '())
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

(defgeneric binding-pressed (wm)
  (:documentation "Called before the action of a pressed binding.")
  (:method-combination progn)
  (:method progn (wm)
    (declare (ignore wm))))

(defgeneric bindings-installed (wm)
  (:documentation "Called after INSTALL-ALL-KEYBINDS.")
  (:method-combination progn)
  (:method progn (wm)
    (declare (ignore wm))))

(defun on-press (wm binding action)
  (push (lambda (event &rest args)
          (declare (ignore args))
          (when (eq event :pressed)
            (binding-pressed wm)
            (funcall action)))
        (proxy-hooks binding))
  binding)

(defun add-binding (wm binding action)
  "Bindings outside of submaps are enabled with the next manage sequence."
  (push (on-press wm binding action) (wm-bindings wm))
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
  (push (on-press wm (make-key-binding wm keysym modifiers)
                  (lambda ()
                    (leave-submap wm)
                    (funcall action)))
        (submap-bindings submap)))

(defstruct (key-entry (:constructor make-key-entry (key label))
                      (:constructor make-prefix-entry (key &aux (prefix-p t))))
  "A key which-key shows, with the label of its keybind or as a chord prefix."
  key
  label
  prefix-p)

(defun chord-entries (keybinds)
  "Alist from prefixes to the key entries of the chords of KEYBINDS after them."
  (loop for (prefix . chords) in (group-by #'keybind-first-key
                                           (remove-if-not #'keybind-chord-key keybinds))
        collect (cons prefix
                      (loop for chord in chords
                            collect (make-key-entry (key-display-name (keybind-chord-key chord))
                                                    (keybind-label chord))))))

(defun modifier-entries (keybinds)
  "Alist from modifiers to the key entries of KEYBINDS starting with them."
  (loop for (modifiers . group) in (group-by (lambda (keybind) (cdr (keybind-first-key keybind)))
                                             keybinds)
        collect (cons modifiers
                      (remove-duplicates
                       (loop for keybind in group
                             for key = (keysym-display-name (car (keybind-first-key keybind)))
                             collect (if (keybind-chord-key keybind)
                                         (make-prefix-entry key)
                                         (make-key-entry key (keybind-label keybind))))
                       :key #'key-entry-key :test #'equal :from-end t))))

(defun prefix-submap (wm submaps prefix chords)
  "CHORDS are the CHORD-ENTRIES of the keybinds."
  (or (gethash prefix submaps)
      (let ((submap (make-submap :keys (key-display-name prefix)
                                 :entries (alist-get prefix chords))))
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
      (let ((submaps (make-hash-table :test #'equal))
            (chords (chord-entries *bindings*)))
        (dolist (keybind *bindings*)
          (let ((first (keybind-first-key keybind))
                (second (keybind-chord-key keybind))
                (run (binding-action wm (keybind-action keybind))))
            (if second
                (bind-key-chord wm (prefix-submap wm submaps first chords)
                                (car second) (cdr second) run)
                (bind-key wm (car first) (cdr first) run))))
        (setf (wm-modifier-entries wm) (modifier-entries *bindings*)))
      (log-message :warn "river_xkb_bindings_v1 is not available, no keybindings installed"))
  (install-pointer-bindings wm)
  (bindings-installed wm)
  (mark-dirty wm))

(defun uninstall-all-keybinds (wm)
  (setf (wm-active-submap wm) nil)
  (cancel-requests wm '(:configure-binding :enable-binding :enter-submap :leave-submap))
  (mapc #'destroy-binding (all-bindings wm))
  (setf (wm-bindings wm) '()
        (wm-submaps wm) '()
        (wm-modifier-entries wm) '()))

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
