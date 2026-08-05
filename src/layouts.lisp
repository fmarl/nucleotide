;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defun tiling (n x y width height &key (ratio 12/20) (gap 8))
  (incf x gap)
  (incf y gap)
  (decf width (* 2 gap))
  (decf height (* 2 gap))
  (cond ((zerop n) '())
	((= n 1) (list (list x y width height)))
	(t
	 (let* ((master-width (floor (* width ratio)))
		(slave-x (+ x master-width gap))
		(slave-width (- width master-width gap))
		(slave-height (floor (- height (* (- n 2) gap)) (1- n))))
	   (cons (list x y master-width height)
		 (loop for i below (1- n)
		       for slave-y = (+ y (* i (+ slave-height gap)))
		       collect (list slave-x slave-y slave-width
				     (if (= i (- n 2))
					 (- (+ y height) slave-y)
					 slave-height))))))))

(defun monocle (n x y width height &key (gap 8))
  (incf x gap)
  (incf y gap)
  (decf width (* 2 gap))
  (decf height (* 2 gap))
  (loop repeat n collect (list x y width height)))
