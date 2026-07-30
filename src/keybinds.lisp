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
    (river-xkb-binding-v1.enable binding)
    binding))

