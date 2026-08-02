;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defun calc-new-pos (windows pos direction)
  (mod (+ pos (ecase direction (:next 1) (:prev -1)))
       (length windows)))

(defun cycle-focus (wm &optional (direction :next))
  (let ((windows (wm-windows wm)))
    (when windows
      (let ((pos (or (position (wm-focused wm) windows) 0)))
	(setf (wm-focused wm)
	      (nth (calc-new-pos windows pos direction) windows)))
      (river-window-manager-v1.manage-dirty (wm-river wm)))))

(defun move-window (wm &optional (direction :next))
  (let* ((windows (wm-windows wm))
	 (pos (position (wm-focused wm) windows)))
    (when (and pos (> (length windows) 1))
      (let ((other (calc-new-pos windows pos direction)))
	(rotatef (nth pos (wm-windows wm))
		 (nth other (wm-windows wm))))
      (river-window-manager-v1.manage-dirty (wm-river wm)))))
