;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; niri's scrollable tiling: windows live in columns on an endless
;;; horizontal strip, and the view scrolls to keep the focused column visible.

(defparameter *column-widths* '(1/3 1/2 2/3)
  "Fractions of the output width that CYCLE-COLUMN-WIDTH steps through.")

(defparameter *default-column-width* 1/2)

(defclass column ()
  ((windows :initarg :windows :reader column-windows)
   (width :initarg :width :reader column-width)
   (active :initarg :active :initform nil :reader column-active)))

(defun make-column (windows width &optional active)
  (make-instance 'column :windows windows :width width :active active))

(defun change-column (column &key (windows (column-windows column))
                               (width (column-width column))
                               (active (column-active column)))
  (make-column windows width active))

(defclass scrolling ()
  ((columns :initform '() :accessor scrolling-columns)
   (offset :initform 0 :accessor scrolling-offset)
   (anchor :initform nil :accessor scrolling-anchor
           :documentation "The focused window as of the last arrangement.")
   (center :initform nil :accessor scrolling-center)
   (gap :initarg :gap :initform 8 :reader scrolling-gap)))

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
                     collect (change-column column
                                            :windows present
                                            :active (find (column-active column) present))))
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
                (change-column column :active win)
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
  "The offset that shows the focused column at LEFT of WIDTH, if any."
  (cond ((null left) (scrolling-offset layout))
        ((scrolling-center layout) (centering-scroll-offset left width view))
        (t (scroll-into-view (scrolling-offset layout) left width view))))

(defun arranged-columns (layout workspace)
  (mark-active-window (reconcile-columns (scrolling-columns layout)
                                         (remove-if #'window-floating (workspace-windows workspace))
                                         (scrolling-anchor layout))
                      (workspace-focused workspace)))

(defun update-scroll (layout workspace view)
  "Update LAYOUT's columns and offset for WORKSPACE; return the left edges and
widths of the columns."
  (let* ((gap (scrolling-gap layout))
         (focus (workspace-focused workspace))
         (columns (arranged-columns layout workspace))
         (widths (mapcar (lambda (column) (column-pixel-width column view gap)) columns))
         (lefts (column-lefts widths gap))
         (i (column-index columns focus)))
    (setf (scrolling-offset layout) (target-offset layout (and i (nth i lefts))
                                                   (and i (nth i widths)) view)
          (scrolling-columns layout) columns
          (scrolling-anchor layout) focus
          (scrolling-center layout) nil)
    (values lefts widths)))

(defun column-rects (column windows x y width height gap)
  "Alist from the windows of COLUMN that are among WINDOWS to their rectangles."
  (let ((tiled (remove-if-not (lambda (win) (member win windows))
                              (column-windows column))))
    (when tiled
      (mapcar #'cons tiled (stack-rects (length tiled) x y width height gap)))))

(defmethod layout-arrange ((layout scrolling) workspace windows x y width height)
  (let ((gap (scrolling-gap layout)))
    (multiple-value-bind (x y view height) (inset x y width height gap)
      (multiple-value-bind (lefts widths) (update-scroll layout workspace view)
        (let ((rects (mapcan (lambda (column left column-width)
                               (column-rects column windows
                                             (+ x (- left (scrolling-offset layout))) y
                                             column-width height gap))
                             (scrolling-columns layout) lefts widths)))
          (mapcar (lambda (win) (cdr (assoc win rects))) windows))))))

(defun horizontal-p (direction)
  (member direction '(:prev :next)))

(defun column-focus (column)
  (or (column-active column) (first (column-windows column))))

(defmethod layout-neighbor ((layout scrolling) workspace win direction)
  (declare (ignore workspace))
  (let* ((columns (scrolling-columns layout))
         (column (column-containing columns win)))
    (when column
      (if (horizontal-p direction)
          (let ((next (adjacent column columns direction)))
            (and next (column-focus next)))
          (adjacent win (column-windows column) direction)))))

(defun move-in-columns (columns win direction)
  (let* ((column (column-containing columns win))
         (windows (column-windows column)))
    (if (horizontal-p direction)
        (let ((other (adjacent column columns direction)))
          (if other (swap columns column other) columns))
        (let ((other (adjacent win windows direction)))
          (if other
              (substitute (change-column column :windows (swap windows win other))
                          column columns)
              columns)))))

(defmethod layout-move ((layout scrolling) workspace win direction)
  (declare (ignore workspace))
  (when (column-containing (scrolling-columns layout) win)
    (setf (scrolling-columns layout)
          (move-in-columns (scrolling-columns layout) win direction))))

(defmethod layout-successor ((layout scrolling) workspace win)
  (some (lambda (direction) (layout-neighbor layout workspace win direction))
        '(:up :down :prev :next)))

(defun expel (columns column win direction)
  (let ((rest (change-column column :windows (remove win (column-windows column))))
        (alone (change-column column :windows (list win) :active win))
        (index (position column columns)))
    (append (subseq columns 0 index)
            (if (eq direction :prev) (list alone rest) (list rest alone))
            (subseq columns (1+ index)))))

(defun consume (columns column win target)
  (substitute (change-column target
                             :windows (append (column-windows target) (list win))
                             :active win)
              target
              (remove column columns)))

(defun consume-or-expel (columns win direction)
  "Expel WIN from its column into a new one in DIRECTION, or if it is alone,
move it into the neighboring column in DIRECTION."
  (let* ((column (column-containing columns win))
         (target (adjacent column columns direction)))
    (cond ((rest (column-windows column)) (expel columns column win direction))
          (target (consume columns column win target))
          (t columns))))

(defun next-column-width (width)
  (or (find-if (lambda (preset) (> preset width)) *column-widths*)
      (first *column-widths*)))

;;; Commands; they do nothing unless the active workspace scrolls.

(defun scrolling-focus (wm)
  (let ((layout (wm-active-layout wm))
        (win (wm-focused-window wm)))
    (when (and (typep layout 'scrolling)
               win
               (column-containing (scrolling-columns layout) win))
      (values layout win))))

(defun update-columns (wm function)
  (multiple-value-bind (layout win) (scrolling-focus wm)
    (when layout
      (setf (scrolling-columns layout)
            (funcall function (scrolling-columns layout) win))
      (mark-dirty wm))))

(defun update-focused-column (wm function)
  (update-columns wm (lambda (columns win)
                       (let ((column (column-containing columns win)))
                         (substitute (funcall function column) column columns)))))

(defun consume-or-expel-window (wm direction)
  (update-columns wm (lambda (columns win)
                       (consume-or-expel columns win direction))))

(defun cycle-column-width (wm)
  (update-focused-column wm (lambda (column)
                              (change-column column
                                             :width (next-column-width
                                                     (column-width column))))))

(defun toggle-full-width (wm)
  (update-focused-column wm (lambda (column)
                              (change-column column
                                             :width (if (= 1 (column-width column))
                                                        *default-column-width*
                                                        1)))))

(defun center-column (wm)
  (let ((layout (scrolling-focus wm)))
    (when layout
      (setf (scrolling-center layout) t)
      (mark-dirty wm))))
