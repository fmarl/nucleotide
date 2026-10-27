;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; niri's scrollable tiling: windows live in columns on an endless
;;; horizontal strip, and the view scrolls to keep the focused column visible.

(define-setting *column-widths* '(1/3 1/2 2/3)
  :documentation "Fractions of the output width that CYCLE-COLUMN-WIDTH steps through.")

(define-setting *default-column-width* 1/2)

(define-setting *window-heights* '(1/3 1/2 2/3)
  :documentation "Fractions of the column height that CYCLE-WINDOW-HEIGHT steps through.")

(defclass column ()
  ((windows :initarg :windows :reader column-windows)
   (width :initarg :width :reader column-width)
   (active :initarg :active :initform nil :reader column-active)
   (heights :initarg :heights :initform '() :reader column-heights
            :documentation "Alist from the windows with a set height to their
fraction of the column; the others share the rest. A window alone in its
column has none, as it could not show it.")))

(defun make-column (windows width &optional active heights)
  (make-instance 'column :windows windows :width width :active active
                 :heights (and (rest windows)
                               (remove-if-not (lambda (entry) (member (car entry) windows))
                                              heights))))

(defun change-column (column &key (windows (column-windows column))
                               (width (column-width column))
                               (active (column-active column))
                               (heights (column-heights column)))
  (make-column windows width active heights))

(defclass scrolling (layout)
  ((columns :initform '() :accessor scrolling-columns)
   (offset :initform 0 :accessor scrolling-offset)
   (anchor :initform nil :accessor scrolling-anchor
           :documentation "The focused tiled window as of the last arrangement.")
   (center :initform nil :accessor scrolling-center)))

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
  (cond ((null left) (scrolling-offset layout))
        ((scrolling-center layout) (centering-scroll-offset left width view))
        (t (scroll-into-view (scrolling-offset layout) left width view))))

(defun arranged-columns (layout workspace)
  (mark-active-window (reconcile-columns (scrolling-columns layout)
                                         (remove-if #'window-floating (workspace-windows workspace))
                                         (scrolling-anchor layout))
                      (workspace-focus workspace)))

(defun column-geometry (columns view gap)
  "The left edges and the widths of COLUMNS."
  (let ((widths (mapcar (lambda (column) (column-pixel-width column view gap)) columns)))
    (values (column-lefts widths gap) widths)))

(defmethod layout-update ((layout scrolling) workspace windows x y width height)
  (declare (ignore windows))
  (let* ((gap *gap*)
         (view (nth-value 2 (inset x y width height gap)))
         (focus (workspace-focus workspace))
         (columns (arranged-columns layout workspace))
         (i (column-index columns focus)))
    (multiple-value-bind (lefts widths) (column-geometry columns view gap)
      (setf (scrolling-offset layout) (target-offset layout (and i (nth i lefts))
                                                     (and i (nth i widths)) view)
            (scrolling-columns layout) columns
            (scrolling-anchor layout) (if i focus (scrolling-anchor layout))
            (scrolling-center layout) nil))))

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

(defun column-rects (column windows x y width height gap)
  (let ((stacked (remove-if-not (lambda (win) (member win windows))
                                (stacked-windows column))))
    (when stacked
      (mapcar #'cons stacked (stack-rects-by (height-fractions column stacked)
                                             x y width height gap)))))

(defmethod layout-arrange ((layout scrolling) workspace windows x y width height)
  (declare (ignore workspace))
  (let ((gap *gap*))
    (multiple-value-bind (x y view height) (inset x y width height gap)
      (multiple-value-bind (lefts widths) (column-geometry (scrolling-columns layout) view gap)
        (let ((rects (mapcan (lambda (column left column-width)
                               (column-rects column windows
                                             (+ x (- left (scrolling-offset layout))) y
                                             column-width height gap))
                             (scrolling-columns layout) lefts widths)))
          (mapcar (lambda (win) (cdr (assoc win rects))) windows))))))

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
        '(:up :down :left :right)))

(defun expel (columns column win direction)
  (let ((rest (change-column column :windows (remove win (column-windows column))))
        (alone (change-column column :windows (list win) :active win))
        (index (position column columns)))
    (append (subseq columns 0 index)
            (if (minusp (step-direction direction)) (list alone rest) (list rest alone))
            (subseq columns (1+ index)))))

(defun consume (columns column win target)
  (substitute (change-column target
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

(defun next-preset (presets fraction)
  (or (find-if (lambda (preset) (> preset fraction)) presets)
      (first presets)))

(defun parse-percent (string)
  "The fraction and the sign, #\\+, #\\- or NIL, of a size like \"50%\" or \"+10%\"."
  (let* ((sign (and (plusp (length string)) (find (char string 0) "+-")))
         (start (if sign 1 0))
         (end (1- (length string))))
    (when (and (< start end)
               (char= #\% (char string end))
               (every #'digit-char-p (subseq string start end)))
      (values (/ (parse-integer string :start start :end end) 100) sign))))

(defun size-changer (change)
  "A function from a fraction to the one CHANGE makes of it."
  (multiple-value-bind (amount sign) (and (stringp change) (parse-percent change))
    (cond ((realp change) (constantly change))
          ((null amount)
           (nucleotide-error "~S is not a fraction or a size like \"50%\" or \"+10%\""
                             change))
          ((eql sign #\+) (lambda (fraction) (+ fraction amount)))
          ((eql sign #\-) (lambda (fraction) (- fraction amount)))
          (t (constantly amount)))))

(defun clamp-fraction (fraction)
  (max 1/10 (min 1 fraction)))

;;; Commands; they do nothing unless the active workspace scrolls.

(defun scrolling-focus (wm)
  (let ((layout (wm-active-layout wm))
        (win (workspace-focus (wm-active-workspace wm))))
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

(defun change-window-height (column win function)
  "COLUMN with the height of WIN set to what FUNCTION makes of the fraction it
shows, or unset when FUNCTION returns NIL."
  (let ((stacked (stacked-windows column)))
    (if (and (rest stacked) (member win stacked))
        (let ((height (funcall function (nth (position win stacked)
                                             (height-fractions column stacked)))))
          (change-column column :heights (if height
                                             (alist-set win (clamp-fraction height)
                                                        (column-heights column))
                                             (alist-remove win (column-heights column)))))
        column)))

(defun update-focused-height (wm function)
  (update-columns wm (lambda (columns win)
                       (let ((column (column-containing columns win)))
                         (substitute (change-window-height column win function)
                                     column columns)))))

(define-command consume-or-expel (wm direction)
  (update-columns wm (lambda (columns win)
                       (consume-or-expel-in columns win direction))))

(define-command cycle-column-width (wm)
  (update-focused-column wm (lambda (column)
                              (change-column column
                                             :width (next-preset *column-widths*
                                                                 (column-width column))))))

(define-command set-column-width (wm change)
  "Change the width of the focused column, as a fraction of the output like 1/2,
or as in niri with \"50%\", \"+10%\" or \"-10%\"."
  (let ((changer (size-changer change)))
    (update-focused-column wm (lambda (column)
                                (change-column column
                                               :width (clamp-fraction
                                                       (funcall changer
                                                                (column-width column))))))))

(define-command set-window-height (wm change)
  "Change the height of the focused window, as a fraction of its column like
1/2, or as in niri with \"50%\", \"+10%\" or \"-10%\". The other windows of the
column share the rest."
  (update-focused-height wm (size-changer change)))

(check-arguments 'set-column-width #'size-changer)
(check-arguments 'set-window-height #'size-changer)

(define-command cycle-window-height (wm)
  (update-focused-height wm (lambda (height) (next-preset *window-heights* height))))

(define-command reset-window-height (wm)
  "Let the focused window share the height of its column again."
  (update-focused-height wm (constantly nil)))

(define-command toggle-full-width (wm)
  (update-focused-column wm (lambda (column)
                              (change-column column
                                             :width (if (= 1 (column-width column))
                                                        *default-column-width*
                                                        1)))))

(define-command center-column (wm)
  (let ((layout (scrolling-focus wm)))
    (when layout
      (setf (scrolling-center layout) t)
      (mark-dirty wm))))
