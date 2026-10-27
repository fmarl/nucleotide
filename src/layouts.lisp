;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *gap* 8
  :documentation "Pixels between windows and around them.")

(defclass layout () ()
  (:documentation "Layouts are subclasses that implement LAYOUT-ARRANGE."))

(defclass tiling (layout)
  ((ratio :initarg :ratio :initform 3/5 :reader tiling-ratio
          :documentation "The part of the width the master window takes.")))

(defclass monocle (layout) ())

(defun make-layout (spec)
  "A layout from SPEC, the name of a layout class or a list of one and its
initargs like (TILING :RATIO 1/2)."
  (destructuring-bind (name &rest initargs) (if (listp spec) spec (list spec))
    (let ((class (and (symbolp name) (find-class name nil))))
      (unless (and class (subtypep class 'layout))
        (nucleotide-error "~S is not a layout" name))
      (apply #'make-instance class initargs))))

(defun step-direction (direction)
  (ecase direction
    ((:next :down :right) 1)
    ((:prev :up :left) -1)))

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

(defgeneric layout-update (layout workspace windows x y width height)
  (:documentation "Update the state of LAYOUT for arranging WINDOWS, the tiled
windows of WORKSPACE, in the area at X Y of WIDTH HEIGHT.")
  (:method (layout workspace windows x y width height)
    (declare (ignore layout workspace windows x y width height))))

(defgeneric layout-arrange (layout workspace windows x y width height)
  (:documentation "An (x y width height) list for each of WINDOWS after
LAYOUT-UPDATE, without changing LAYOUT."))

(defgeneric layout-neighbor (layout workspace win direction)
  (:documentation "The window next to WIN in DIRECTION, one of :LEFT, :RIGHT,
:UP and :DOWN (or :PREV and :NEXT), or NIL.")
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

(defun tiling-rects (n x y width height ratio gap)
  (multiple-value-bind (x y width height) (inset x y width height gap)
    (if (< n 2)
        (loop repeat n collect (list x y width height))
        (let ((master-width (floor (* width ratio))))
          (cons (list x y master-width height)
                (stack-rects (1- n) (+ x master-width gap) y
                             (- width master-width gap) height gap))))))

(defun monocle-rects (n x y width height gap)
  (multiple-value-bind (x y width height) (inset x y width height gap)
    (loop repeat n collect (list x y width height))))

(defmethod layout-arrange ((layout tiling) workspace windows x y width height)
  (declare (ignore workspace))
  (tiling-rects (length windows) x y width height (tiling-ratio layout) *gap*))

(defmethod layout-arrange ((layout monocle) workspace windows x y width height)
  (declare (ignore workspace))
  (monocle-rects (length windows) x y width height *gap*))
