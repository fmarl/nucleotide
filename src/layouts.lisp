;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defun tiling (n x y width height &key (ratio 6/10))
  (cond ((zerop n) '())
	((= n 1) (list (list x y width height)))
	(t
	 (let* ((master-width (floor (* width ratio)))
		(slave-height (floor height (1- n))))
	   (cons (list x y master-width height)
		 (loop for i below (1- n)
		       for slave-y = (+ y (* i slave-height))
		       collect (list (+ x master-width) slave-y (- width master-width)
				     (if (= i (- n 2))
					 (- (+ y height) slave-y)
					 slave-height))))))))
