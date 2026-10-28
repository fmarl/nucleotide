;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *gap* 8
  :documentation "Pixels between windows and around them."
  :check (type-check '(and wire-int unsigned-byte) "a number of pixels like 8"))

(defclass layout () ())

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

(defgeneric layout-update (layout workspace windows area)
  (:documentation "Update the state of LAYOUT for WINDOWS, the windows of WORKSPACE
that do not float, in the rect AREA. Fullscreen ones are among them, so that
LAYOUT can keep their place.")
  (:method (layout workspace windows area)
    (declare (ignore layout workspace windows area))))

(defgeneric layout-arrange (layout workspace windows area)
  (:documentation "A rect for each of WINDOWS, those of LAYOUT-UPDATE that are not
fullscreen, without changing LAYOUT."))

(defun cyclic-neighbor (item list direction)
  (let ((pos (position item list)))
    (when (and pos (rest list))
      (nth (mod (+ pos (step-direction direction)) (length list)) list))))

(defgeneric layout-neighbor (layout workspace win direction)
  (:documentation "The window next to WIN in DIRECTION, one of :LEFT, :RIGHT,
:UP and :DOWN (or :PREV and :NEXT), or NIL.")
  (:method (layout workspace win direction)
    (declare (ignore layout))
    (cyclic-neighbor win (workspace-windows workspace) direction)))

(defgeneric layout-move (layout workspace win direction)
  (:documentation "Move WIN one step in DIRECTION within WORKSPACE.")
  (:method (layout workspace win direction)
    (unless (window-floating-p win)
      (let ((other (loop for other = (layout-neighbor layout workspace win direction)
                         then (layout-neighbor layout workspace other direction)
                         repeat (length (workspace-windows workspace))
                         while (and other (not (eq other win)))
                         unless (window-floating-p other)
                         return other)))
        (when other
          (swap-windows workspace win other))))))

(defgeneric layout-successor (layout workspace win)
  (:documentation "The window to focus when WIN leaves WORKSPACE.")
  (:method (layout workspace win)
    (declare (ignore layout))
    (find win (workspace-windows workspace) :test-not #'eq)))

(defun stack-rects-by (fractions area gap)
  "The last rect takes what rounding leaves."
  (loop with room = (- (rect-height area) (* (1- (length fractions)) gap))
        for (fraction . more) on fractions
        for top = (rect-y area) then (+ top each gap)
        for each = (if more (floor (* fraction room)) (- (rect-bottom area) top))
        collect (rect (rect-x area) top (rect-width area) each)))

(defun stack-rects (n area gap)
  (when (plusp n)
    (stack-rects-by (make-list n :initial-element (/ 1 n)) area gap)))

(defun tiling-rects (n area ratio gap)
  (let ((area (inset area gap)))
    (if (< n 2)
        (make-list n :initial-element area)
        (let ((master-width (floor (* (rect-width area) ratio))))
          (cons (rect (rect-x area) (rect-y area) master-width (rect-height area))
                (stack-rects (1- n)
                             (rect (+ (rect-x area) master-width gap) (rect-y area)
                                   (- (rect-width area) master-width gap) (rect-height area))
                             gap))))))

(defun monocle-rects (n area gap)
  (make-list n :initial-element (inset area gap)))

(defmethod layout-arrange ((layout tiling) workspace windows area)
  (declare (ignore workspace))
  (tiling-rects (length windows) area (tiling-ratio layout) *gap*))

(defmethod layout-arrange ((layout monocle) workspace windows area)
  (declare (ignore workspace))
  (monocle-rects (length windows) area *gap*))
