;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *pointer-modifiers* "Super"
  :documentation "The modifiers that move and resize windows with the pointer.")

(define-setting *move-button* 272
  :documentation "Linux input event code of the button that moves windows (BTN_LEFT).")

(define-setting *resize-button* 273
  :documentation "Linux input event code of the button that resizes windows (BTN_RIGHT).")

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
  (let ((parent (window-parent win))
        (area (usable-area (window-output wm win))))
    (if (and parent (window-visible-p wm parent))
        (destructuring-bind (dx dy)
            (center-in-area (window-width win) (window-height win)
                            (window-width parent) (window-height parent))
          (list (- (+ (window-x parent) dx) (rect-x area))
                (- (+ (window-y parent) dy) (rect-y area))))
        (center-in-area (window-width win) (window-height win)
                        (rect-width area) (rect-height area)))))

(defun place-floating (wm win)
  (let ((output (window-output wm win)))
    (when output
      (unless (window-float-offset win)
        (setf (%window-float-offset win) (initial-float-offset wm win)))
      (let ((area (usable-area output)))
        (destructuring-bind (dx dy) (window-float-offset win)
          (position-window win (+ (rect-x area) dx) (+ (rect-y area) dy) nil))))))

(defun offset-on (output win)
  (let ((area (usable-area output)))
    (list (- (window-x win) (rect-x area)) (- (window-y win) (rect-y area)))))

(defun float-new-window (win floating)
  "A new floating window picks its own size."
  (setf (%window-floating-p win) floating)
  (when (and floating (not (window-float-offset win)))
    (setf (%window-pending-dimensions win) '(0 0))))

(defun float-window (wm win)
  "Float WIN where it is, at its current size."
  (let ((output (window-output wm win)))
    (setf (%window-floating-p win) t
          (%window-float-offset win) (and output (offset-on output win))
          (%window-pending-dimensions win) (list (window-width win) (window-height win)))
    (request wm :configure win)))

(defun tile-window (wm win)
  (setf (%window-floating-p win) nil
        (%window-float-offset win) nil
        (%window-pending-dimensions win) nil)
  (request wm :configure win))

(defun manage-pending-dimensions (wm)
  (dolist (win (all-windows wm))
    (let ((dimensions (window-pending-dimensions win)))
      (when dimensions
        (setf (%window-pending-dimensions win) nil)
        (when (window-floating-p win)
          (apply #'propose-dimensions win dimensions))))))

(define-command toggle-floating (wm)
  "Float the focused window, or tile it again."
  (let ((win (wm-focus wm)))
    (when (and win (not (window-fullscreen win)))
      (if (window-floating-p win)
          (tile-window wm win)
          (float-window wm win))
      (mark-dirty wm))))

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
      (unless (window-floating-p win)
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
         (setf (%window-float-offset win) (list (+ x dx) (+ y dy)))))
      (:resize
       (destructuring-bind (width height) (op-start op)
         (setf (%window-pending-dimensions win)
               (list (max 1 (+ width dx)) (max 1 (+ height dy)))))))))

(defun output-under (wm rect)
  "The output RECT mostly lies on."
  (flet ((area (output) (overlap-area rect (output-rect output))))
    (first (sort (remove-if-not (lambda (output) (plusp (area output)))
                                (copy-list (wm-outputs wm)))
                 #'> :key #'area))))

(defun settle-floating (wm win)
  "Hand a moved floating window to the output it now mostly lies on."
  (let ((output (output-under wm (window-rect win))))
    (when (and output
               (output-workspace output)
               (not (eq output (window-output wm win))))
      (setf (%window-float-offset win) (offset-on output win))
      (move-to-workspace win (output-workspace output))
      (focus-window wm win))))

(defun finish-pointer-op (wm op)
  (river-seat-v1.op-end (wm-seat wm))
  (when (and (eq (op-kind op) :move)
             (live (op-win op)))
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
