;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defun wl-debug-info (&optional display-name)
  (with-open-display (display display-name)
    (let ((registry (wl-display.get-registry display))
	  (globals '()))
      (push (lambda (event &rest args)
	      (when (eq event :global)
		(push args globals)))
	    (proxy-hooks registry))
      (wl-display-roundtrip display)
      (loop for (name interface version) in (nreverse globals)
	    do (format t "~4D ~44A v~D~%" name interface version))
      (values))))
