;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun make-layout (spec)
  "A fresh instance if SPEC names a layout class, else SPEC itself, a function
of (n x y width height)."
  (if (and (symbolp spec) (find-class spec nil))
      (make-instance spec)
      spec))

(defun step-direction (direction)
  (ecase direction
    ((:next :down) 1)
    ((:prev :up) -1)))

(defun swap (list a b)
  (mapcar (lambda (x)
            (cond ((eq x a) b)
                  ((eq x b) a)
                  (t x)))
          list))

(defun adjacent (item list direction)
  "The element next to ITEM in LIST in DIRECTION, or NIL at either end."
  (let ((index (+ (position item list) (step-direction direction))))
    (and (< -1 index (length list)) (nth index list))))

(defgeneric layout-arrange (layout workspace windows x y width height)
  (:documentation "An (x y width height) list for each of WINDOWS, the tiled
windows of WORKSPACE.")
  (:method (layout workspace windows x y width height)
    (declare (ignore workspace))
    (funcall layout (length windows) x y width height)))

(defgeneric layout-neighbor (layout workspace win direction)
  (:documentation "The window next to WIN in DIRECTION, one of :PREV, :NEXT,
:UP and :DOWN, or NIL.")
  (:method (layout workspace win direction)
    (declare (ignore layout))
    (let* ((windows (workspace-windows workspace))
           (pos (position win windows)))
      (when (and pos (rest windows))
        (nth (mod (+ pos (step-direction direction)) (length windows))
             windows)))))

(defgeneric layout-move (layout workspace win direction)
  (:method (layout workspace win direction)
    (let ((other (layout-neighbor layout workspace win direction)))
      (when other
        (setf (workspace-windows workspace) (swap (workspace-windows workspace) win other))))))

(defgeneric layout-successor (layout workspace win)
  (:documentation "The window to focus when WIN leaves WORKSPACE.")
  (:method (layout workspace win)
    (declare (ignore layout))
    (find win (workspace-windows workspace) :test-not #'eq)))

(defun inset (x y width height gap)
  (values (+ x gap) (+ y gap) (- width (* 2 gap)) (- height (* 2 gap))))

(defun stack-rects (n x y width height gap)
  (let ((each (floor (- height (* (1- n) gap)) n)))
    (loop for i below n
          for top = (+ y (* i (+ each gap)))
          collect (list x top width (if (= i (1- n))
                                        (- (+ y height) top)
                                        each)))))

(defun tiling (n x y width height &key (ratio 12/20) (gap 8))
  (multiple-value-bind (x y width height) (inset x y width height gap)
    (if (< n 2)
        (loop repeat n collect (list x y width height))
        (let ((master-width (floor (* width ratio))))
          (cons (list x y master-width height)
                (stack-rects (1- n) (+ x master-width gap) y
                             (- width master-width gap) height gap))))))

(defun monocle (n x y width height &key (gap 8))
  (multiple-value-bind (x y width height) (inset x y width height gap)
    (loop repeat n collect (list x y width height))))
