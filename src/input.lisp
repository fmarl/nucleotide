;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defun watch-libinput-result (request result)
  (push (lambda (event &rest args)
	  (declare (ignore args))
	  (unless (eq event :success)
	    (warn "libinput request ~A rejected in ~A" request event)))
	(proxy-hooks result)))

(defun handle-libinput-device-event (device event &rest args)
  (case event
    (:tab-support
     (when (plusp (first args))
       (watch-libinput-result "set_tap" (river-libinput-device-v1.set-tap device *tap-enabled*))))
    (:removed
     (river-libinput-device-v1.destroy device))))


(defun handle-libinput-config-event (wm event &rest args)
  (declare (ignore wm))
  (when (eq event :libinput-device)
    (let ((device (first args)))
      (push (lambda (&rest event)
	      (apply #'handle-libinput-device-event device event))
	    (proxy-hooks device)))))

(defun handle-input-manager-event (wm event &rest args)
  (declare (ignore wm))
  (when (eq event :input-device)
    (let ((device (first args)))
      (push (lambda (event &rest args)
	      (declare (ignore args))
	      (when (eq event :removed)
		(river-input-device-v1.destroy device)))
	    (proxy-hooks device)))))
