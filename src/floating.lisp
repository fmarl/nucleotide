;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *pointer-modifiers* :super)

(defparameter *move-button* 272
  "Linux input event code of the button that moves windows (BTN_LEFT).")

(defparameter *resize-button* 273
  "Linux input event code of the button that resizes windows (BTN_RIGHT).")

(defun fixed-size-p (win)
  (and (plusp (window-min-width win))
       (plusp (window-min-height win))
       (= (window-min-width win) (window-max-width win))
       (= (window-min-height win) (window-max-height win))))

(defun auto-float-p (win)
  (or (window-parent win) (fixed-size-p win)))

(defun center-in-area (width height area-width area-height)
  (list (floor (- area-width width) 2)
        (floor (- area-height height) 2)))

(defun initial-float-offset (wm win)
  "Centered on the parent if that is visible, else on the output."
  (let ((parent (window-parent win)))
    (multiple-value-bind (x y width height) (usable-area (window-output wm win))
      (if (and parent (window-visible-p wm parent))
          (destructuring-bind (dx dy)
              (center-in-area (window-width win) (window-height win)
                              (window-width parent) (window-height parent))
            (list (- (+ (window-x parent) dx) x)
                  (- (+ (window-y parent) dy) y)))
          (center-in-area (window-width win) (window-height win) width height)))))

(defun place-floating (wm win)
  (let ((output (window-output wm win)))
    (when output
      (unless (window-float-offset win)
        (setf (window-float-offset win) (initial-float-offset wm win)))
      (multiple-value-bind (x y) (usable-area output)
        (destructuring-bind (dx dy) (window-float-offset win)
          (setf (window-x win) (+ x dx)
                (window-y win) (+ y dy)
                (window-clip win) nil))))))

(defun offset-on (output win)
  (multiple-value-bind (x y) (usable-area output)
    (list (- (window-x win) x) (- (window-y win) y))))

(defun float-window (wm win)
  "Float WIN where it currently is, at its current size."
  (let ((output (window-output wm win)))
    (setf (window-floating win) t
          (window-float-offset win) (and output (offset-on output win))
          (window-pending-dimensions win) (list (window-width win) (window-height win))
          (window-configured win) nil)))

(defun tile-window (win)
  (setf (window-floating win) nil
        (window-float-offset win) nil
        (window-pending-dimensions win) nil
        (window-configured win) nil))

(defun toggle-floating (wm)
  (let ((win (effective-focus wm)))
    (when (and win (not (window-fullscreen win)))
      (if (window-floating win)
          (tile-window win)
          (float-window wm win))
      (mark-dirty wm))))

;;; Interactive move and resize with the pointer

(defclass pointer-op ()
  ((kind :initarg :kind :reader op-kind)
   (win :initarg :win :reader op-win)
   (start :initarg :start :reader op-start
          :documentation "The floating offset or the dimensions at the start.")
   (state :initform :start :accessor op-state)))

(defun op-start-value (win kind)
  (ecase kind
    (:move (or (window-float-offset win) (list 0 0)))
    (:resize (list (window-width win) (window-height win)))))

(defun start-pointer-op (wm kind)
  (let ((win (wm-pointer-window wm)))
    (when (and win (not (wm-op wm)) (not (window-fullscreen win)))
      (unless (window-floating win)
        (float-window wm win))
      (focus-window wm win)
      (setf (wm-op wm) (make-instance 'pointer-op :kind kind :win win
                                      :start (op-start-value win kind)))
      (mark-dirty wm))))

(defun update-pointer-op (op dx dy)
  (let ((win (op-win op)))
    (ecase (op-kind op)
      (:move
       (destructuring-bind (x y) (op-start op)
         (setf (window-float-offset win) (list (+ x dx) (+ y dy)))))
      (:resize
       (destructuring-bind (width height) (op-start op)
         (setf (window-pending-dimensions win)
               (list (max 1 (+ width dx)) (max 1 (+ height dy)))))))))

(defun overlap-area (x y width height output)
  (flet ((span (start length other-start other-length)
           (max 0 (- (min (+ start length) (+ other-start other-length))
                     (max start other-start)))))
    (* (span x width (output-x output) (output-width output))
       (span y height (output-y output) (output-height output)))))

(defun output-under (wm x y width height)
  "The output the area at X Y of WIDTH HEIGHT mostly lies on."
  (flet ((area (output) (overlap-area x y width height output)))
    (first (sort (remove-if-not (lambda (output) (plusp (area output)))
                                (copy-list (wm-outputs wm)))
                 #'> :key #'area))))

(defun settle-floating (wm win)
  "Hand a moved floating window to the output it now mostly lies on."
  (let ((output (output-under wm (window-x win) (window-y win)
                              (window-width win) (window-height win))))
    (when (and output
               (output-workspace output)
               (not (eq output (window-output wm win))))
      (setf (window-float-offset win) (offset-on output win))
      (move-to-workspace win (output-workspace output))
      (focus-window wm win))))

(defun finish-pointer-op (wm op)
  (river-seat-v1.op-end (wm-seat wm))
  (when (and (eq (op-kind op) :move)
             (member (op-win op) (all-windows wm)))
    (settle-floating wm (op-win op)))
  (setf (wm-op wm) nil))

(defun manage-pointer-op (wm)
  (let ((op (wm-op wm)))
    (when op
      (ecase (op-state op)
        (:start
         (river-seat-v1.op-start-pointer (wm-seat wm))
         (setf (op-state op) :running))
        (:running)
        (:end (finish-pointer-op wm op))))))

(define-handler (wm (seat river-seat-v1) :pointer-enter) (window)
  (setf (wm-pointer-window wm) (find-window wm window)))

(define-handler (wm (seat river-seat-v1) :pointer-leave) ()
  (setf (wm-pointer-window wm) nil))

(define-handler (wm (seat river-seat-v1) :op-delta) (dx dy)
  (when (wm-op wm)
    (update-pointer-op (wm-op wm) dx dy)))

(define-handler (wm (seat river-seat-v1) :op-release) ()
  (when (wm-op wm)
    (setf (op-state (wm-op wm)) :end)))

(defun install-pointer-bindings (wm)
  (let ((modifiers (parse-modifiers *pointer-modifiers*)))
    (bind-pointer wm *move-button* modifiers
                  (lambda () (start-pointer-op wm :move)))
    (bind-pointer wm *resize-button* modifiers
                  (lambda () (start-pointer-op wm :resize)))))
