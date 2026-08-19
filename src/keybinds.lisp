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

(defun shiftify (modifier)
  (logior modifier +mod-shift+))

(defun keybinds (wm)
  (bind-key wm +xk-return+ (shiftify +mod-super+) (lambda () (spawn "alacritty")))
  (bind-key wm (char-code #\p) +mod-super+ (lambda () (spawn "bemenu-run")))
  (bind-key wm (char-code #\k) +mod-super+ (lambda () (cycle-focus wm :prev)))
  (bind-key wm (char-code #\l) +mod-super+ (lambda () (cycle-focus wm :next)))
  (bind-key wm (char-code #\k) (shiftify +mod-super+) (lambda () (move-window wm :prev)))
  (bind-key wm (char-code #\l) (shiftify +mod-super+) (lambda () (move-window wm :next))))
