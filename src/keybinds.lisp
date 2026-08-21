;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defconstant +xk-return+ #xff0d)
(defconstant +mod-shift+ 1)
(defconstant +mod-ctrl+ 4)
(defconstant +mod-super+ 64)

(defun spawn (command)
  (uiop:launch-program (list "setsid" "-f" command)))

(defun bind-key (wm keysym modifiers action)
  (let ((binding (river-xkb-bindings-v1.get-xkb-binding
		  (wm-xkb wm) (wm-seat wm) keysym modifiers)))
    (push (lambda (event &rest args)
	    (declare (ignore args))
	    (when (eq event :pressed) (funcall action)))
	  (proxy-hooks binding))
    (push binding (wm-pending-bindings wm))
    (river-window-manager-v1.manage-dirty (wm-river wm))
    binding))

(defun bind-key-chord (wm submap keysym action)
  (let ((binding (river-xkb-bindings-v1.get-xkb-binding (wm-xkb wm) (wm-seat wm) keysym 0)))
    (push (lambda (event &rest args)
	    (declare (ignore args))
	    (when (eq event :pressed)
	      (leave-submap wm)
	      (funcall action)))
	  (proxy-hooks binding))
    (push binding (submap-bindings submap))
    binding))

(defun shiftify (modifier)
  (logior modifier +mod-shift+))

(defmacro define-keybinds (&body bindings)
  `(defun install-keybinds (wm)
     ,@(mapcar
	(lambda (binding)
	  (destructuring-bind (mod key &body body) binding
	    `(bind-key wm ,(if (characterp key)
			       `(char-code ,key)
			       key) ,(if (and (listp mod) (eq (first mod) :shift))
					 `(shiftify ,(second mod))
					 mod) (lambda () ,@body))))
	bindings)))

(defmacro define-key-chords (&body bindings)
  `(defun install-key-chords (wm)
     ,@(mapcar
	(lambda (binding)
	  (destructuring-bind (mod key &body chords) binding
	    (let ((submap (gensym "SUBMAP")))
	      `(let ((,submap (make-submap)))
		 ,@(loop for (ckey . body) in chords
			 collect `(bind-key-chord wm ,submap
						  ,(if (characterp ckey)
						       `(char-code ,ckey)
						       ckey)
						  (lambda () ,@body)))
		 (bind-key wm ,(if (characterp key)
				   `(char-code ,key)
				   key)
			   ,mod (lambda () (enter-submap wm ,submap)))))))
	bindings)))

(defun install-default-keybinds (wm)
  (loop for ws in (wm-workspaces wm)
	for key from (char-code #\1)
	do (let ((ws ws))
	     (bind-key wm key +mod-super+ (lambda () (switch-workspace wm ws)))
	     (bind-key wm key (shiftify +mod-super+) (lambda () (send-to-workspace wm ws))))))

