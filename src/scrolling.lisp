;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(deftype fraction ()
  "A part of the width or height of an output or column."
  '(real (0) 1))

(define-setting *default-column-width* 1/2
  :check (type-check 'fraction "a fraction like 1/2"))

(defclass column ()
  ((windows :initarg :windows :reader column-windows)
   (width :initarg :width :reader column-width)
   (active :initarg :active :initform nil :reader column-active)
   (heights :initarg :heights :initform '() :reader column-heights
            :documentation "Alist from the windows with a set height to their
fraction of the column. The others share the rest. A window alone in its
column has none, as it could not show it.")))

(defun make-column (windows width &key active heights)
  (make-instance 'column :windows windows :width width
                 :active (find active windows)
                 :heights (and (rest windows)
                               (remove-if-not (lambda (entry) (member (car entry) windows))
                                              heights))))

(defun copy-column (column &key (windows (column-windows column))
                             (width (column-width column))
                             (active (column-active column))
                             (heights (column-heights column)))
  (make-column windows width :active active :heights heights))

(defclass scrolling (layout)
  ((columns :initform '() :accessor scrolling-columns)
   (offset :initform 0 :accessor scrolling-offset)
   (anchor :initform nil :accessor scrolling-anchor
           :documentation "The focused tiled window as of the last arrangement.")
   (center :initform nil :accessor scrolling-center-p)))

(defmethod scrolling-anchor :around ((layout scrolling))
  (live (call-next-method)))

(defun column-containing (columns win)
  (find-if (lambda (column) (member win (column-windows column))) columns))

(defun column-index (columns win)
  (position (column-containing columns win) columns))

(defun reconcile-columns (columns windows anchor)
  "Drop windows that are gone, and open each window without a column as a new
column right of ANCHOR's."
  (let* ((kept (loop for column in columns
                     for present = (remove-if-not (lambda (win) (member win windows))
                                                  (column-windows column))
                     when present
                     collect (copy-column column :windows present)))
         (placed (mappend #'column-windows kept))
         (new (reverse (remove-if (lambda (win) (member win placed)) windows)))
         (at (1+ (or (column-index kept anchor) (1- (length kept))))))
    (append (subseq kept 0 at)
            (mapcar (lambda (win) (make-column (list win) *default-column-width*))
                    new)
            (subseq kept at))))

(defun mark-active-window (columns win)
  (mapcar (lambda (column)
            (if (member win (column-windows column))
                (copy-column column :active win)
                column))
          columns))

(defun column-pixel-width (column view gap)
  (- (round (* (column-width column) (+ view gap))) gap))

(defun column-lefts (widths gap)
  (loop with left = 0
        for width in widths
        collect left
        do (incf left (+ width gap))))

(defun scroll-into-view (offset left width view)
  (cond ((or (< left offset) (> width view)) left)
        ((> (+ left width) (+ offset view)) (- (+ left width) view))
        (t offset)))

(defun centering-scroll-offset (left width view)
  (- (+ left (floor width 2)) (floor view 2)))

(defun target-offset (layout left width view)
  (cond ((null left) (scrolling-offset layout))
        ((scrolling-center-p layout) (centering-scroll-offset left width view))
        (t (scroll-into-view (scrolling-offset layout) left width view))))

(defun arranged-columns (layout workspace windows)
  (mark-active-window (reconcile-columns (scrolling-columns layout) windows
                                         (scrolling-anchor layout))
                      (workspace-focus workspace)))

(defun column-geometry (columns view gap)
  "The left edges and the widths of COLUMNS."
  (let ((widths (mapcar (lambda (column) (column-pixel-width column view gap)) columns)))
    (values (column-lefts widths gap) widths)))

(defmethod layout-update ((layout scrolling) workspace windows area)
  (let* ((gap *gap*)
         (view (rect-width (inset area gap)))
         (focus (workspace-focus workspace))
         (columns (arranged-columns layout workspace windows))
         (i (column-index columns focus)))
    (multiple-value-bind (lefts widths) (column-geometry columns view gap)
      (setf (scrolling-offset layout) (target-offset layout (and i (nth i lefts))
                                                     (and i (nth i widths)) view)
            (scrolling-columns layout) columns
            (scrolling-anchor layout) (if i focus (scrolling-anchor layout))
            (scrolling-center-p layout) nil))))

(defun height-fractions (column windows)
  "Windows without a set height keep at least a small part, and set heights
are scaled to fill the column when the others cannot."
  (let* ((set (mapcar (lambda (win) (alist-get win (column-heights column))) windows))
         (unset (count nil set))
         (taken (reduce #'+ (remove nil set)))
         (minimum (min 1/10 (/ 1 (* 2 (length windows)))))
         (room (if (zerop unset) 1 (min taken (- 1 (* unset minimum)))))
         (scale (if (plusp taken) (/ room taken) 0))
         (each (if (plusp unset) (/ (- 1 room) unset) 0)))
    (mapcar (lambda (height) (if height (* height scale) each)) set)))

(defun stacked-windows (column)
  (remove-if #'window-fullscreen (column-windows column)))

(defun column-rects (column windows area gap)
  (let ((stacked (remove-if-not (lambda (win) (member win windows))
                                (column-windows column))))
    (when stacked
      (mapcar #'cons stacked (stack-rects-by (height-fractions column stacked) area gap)))))

(defmethod layout-arrange ((layout scrolling) workspace windows area)
  (declare (ignore workspace))
  (let* ((gap *gap*)
         (view (inset area gap))
         (origin (- (rect-x view) (scrolling-offset layout))))
    (multiple-value-bind (lefts widths)
        (column-geometry (scrolling-columns layout) (rect-width view) gap)
      (let ((rects (mapcan (lambda (column left width)
                             (column-rects column windows
                                           (rect (+ origin left) (rect-y view)
                                                 width (rect-height view))
                                           gap))
                           (scrolling-columns layout) lefts widths)))
        (mapcar (lambda (win) (cdr (assoc win rects))) windows)))))

(defun horizontal-p (direction)
  (member direction '(:prev :next :left :right)))

(defun column-focus (column)
  (or (column-active column) (first (column-windows column))))

(defmethod layout-neighbor ((layout scrolling) workspace win direction)
  (declare (ignore workspace))
  (let* ((columns (scrolling-columns layout))
         (column (column-containing columns win)))
    (cond ((null column)
           (let ((last (or (column-containing columns (scrolling-anchor layout))
                           (first columns))))
             (and last (column-focus last))))
          ((horizontal-p direction)
           (let ((next (adjacent column columns direction)))
             (and next (column-focus next))))
          (t (adjacent win (column-windows column) direction)))))

(defun move-in-columns (columns win direction)
  (let* ((column (column-containing columns win))
         (windows (column-windows column)))
    (if (horizontal-p direction)
        (let ((other (adjacent column columns direction)))
          (if other (swap columns column other) columns))
        (let ((other (adjacent win windows direction)))
          (if other
              (substitute (copy-column column :windows (swap windows win other))
                          column columns)
              columns)))))

(defmethod layout-move ((layout scrolling) workspace win direction)
  (declare (ignore workspace))
  (when (column-containing (scrolling-columns layout) win)
    (setf (scrolling-columns layout)
          (move-in-columns (scrolling-columns layout) win direction))))

(defmethod layout-successor ((layout scrolling) workspace win)
  (some (lambda (direction) (layout-neighbor layout workspace win direction))
        '(:up :down :left :right)))

(defun expel (columns column win direction)
  (let ((rest (copy-column column :windows (remove win (column-windows column))))
        (alone (copy-column column :windows (list win) :active win))
        (index (position column columns)))
    (append (subseq columns 0 index)
            (if (minusp (step-direction direction)) (list alone rest) (list rest alone))
            (subseq columns (1+ index)))))

(defun consume (columns column win target)
  (substitute (copy-column target
                           :windows (append (column-windows target) (list win))
                           :active win)
              target
              (remove column columns)))

(defun consume-or-expel-in (columns win direction)
  "Expel WIN from its column into a new one in DIRECTION, or if it is alone,
move it into the neighboring column in DIRECTION."
  (let* ((column (column-containing columns win))
         (target (adjacent column columns direction)))
    (cond ((rest (column-windows column)) (expel columns column win direction))
          (target (consume columns column win target))
          (t columns))))
