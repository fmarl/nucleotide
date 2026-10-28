;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *pointer-modifiers* "Super"
  :documentation "The modifiers that move and resize windows with the pointer."
  :check #'parse-modifiers)

(define-setting *move-button* 272
  :documentation "Linux input event code of the button that moves windows (BTN_LEFT)."
  :check (type-check '(unsigned-byte 32) "a button code like 272"))

(define-setting *resize-button* 273
  :documentation "Linux input event code of the button that resizes windows (BTN_RIGHT)."
  :check (type-check '(unsigned-byte 32) "a button code like 273"))

(defun fixed-size-p (win)
  (and (plusp (window-min-width win))
       (plusp (window-min-height win))
       (= (window-min-width win) (window-max-width win))
       (= (window-min-height win) (window-max-height win))))

(defun auto-float-p (win)
  (or (window-parent win) (fixed-size-p win)))

(defun initial-float-offset (wm win)
  "Centered on the parent if that is visible, else on the output."
  (let ((parent (window-parent win))
        (area (usable-area (window-output win))))
    (if (and parent (window-visible-p wm parent))
        (point- (point+ (window-origin parent)
                        (centered-offset (window-size win) (window-size parent)))
                (rect-origin area))
        (centered-offset (window-size win) (rect-size area)))))

(defun offset-on (output win)
  (point- (window-origin win) (rect-origin (usable-area output))))

(defstruct (anchor (:constructor anchor (rect edges)))
  "Where a pointer op took hold of a floating window: its offset and size as a
rect, and the edges a resize moves."
  (rect nil :type rect :read-only t)
  (edges 0 :type integer :read-only t))

(defun anchored-offset (anchor size)
  "The offset at which a window of SIZE keeps the edges that the resize of ANCHOR
does not move in place."
  (let ((start (anchor-rect anchor))
        (edges (anchor-edges anchor)))
    (point (if (logtest edges +river-window-v1-edges-left+)
               (- (rect-right start) (size-width size))
               (rect-x start))
           (if (logtest edges +river-window-v1-edges-top+)
               (- (rect-bottom start) (size-height size))
               (rect-y start)))))

(defun resized-size (anchor dx dy)
  "The size of the resize of ANCHOR after the pointer moved by DX and DY."
  (let ((start (anchor-rect anchor))
        (edges (anchor-edges anchor)))
    (flet ((span (length delta low high)
             (max 1 (cond ((logtest edges high) (+ length delta))
                          ((logtest edges low) (- length delta))
                          (t length)))))
      (size (span (rect-width start) dx
                  +river-window-v1-edges-left+ +river-window-v1-edges-right+)
            (span (rect-height start) dy
                  +river-window-v1-edges-top+ +river-window-v1-edges-bottom+)))))

(defun place-floating (wm win)
  (let ((output (window-output win))
        (anchor (window-anchor win)))
    (when output
      (setf (%window-float-offset win)
            (cond (anchor (anchored-offset anchor (window-size win)))
                  ((window-float-offset win))
                  (t (initial-float-offset wm win))))
      (position-window win
                       (point+ (rect-origin (usable-area output)) (window-float-offset win))
                       nil))))

(defun float-new-window (win floating)
  "A new floating window picks its own size."
  (setf (%window-floating-p win) floating)
  (when (and floating (not (window-float-offset win)))
    (setf (%window-pending-size win) (size 0 0))))

(defun float-window (wm win &optional (size (window-size win)))
  "Float WIN where it is, at SIZE."
  (let ((output (window-output win)))
    (setf (%window-floating-p win) t
          (%window-float-offset win) (and output (offset-on output win))
          (%window-anchor win) nil
          (%window-pending-size win) size)
    (request wm :configure win)))

(defun tile-window (wm win)
  (setf (%window-floating-p win) nil
        (%window-float-offset win) nil
        (%window-anchor win) nil
        (%window-pending-size win) nil)
  (request wm :configure win))

(defun manage-pending-sizes (wm)
  (dolist (win (all-windows wm))
    (let ((pending (window-pending-size win)))
      (when pending
        (setf (%window-pending-size win) nil)
        (when (eq (window-placement win) :floating)
          (propose-size win pending))))))

(define-command toggle-floating (wm)
  "Float the focused window, or tile it again. A maximized window stops being
maximized."
  (let ((win (wm-focus wm)))
    (when (and win (not (window-fullscreen win)))
      (let ((size (or (unmaximize wm win) (window-size win))))
        (if (window-floating-p win)
            (tile-window wm win)
            (float-window wm win size)))
      (mark-dirty wm))))

(defconstant +bottom-right+
  (logior +river-window-v1-edges-bottom+ +river-window-v1-edges-right+))

(deftype op-state ()
  "A pointer op waits for the next manage sequence to :START, is :RUNNING, or
waits for the next one to :END."
  '(member :start :running :end))

(defclass pointer-op ()
  ((kind :initarg :kind :reader op-kind :type (member :move :resize))
   (win :initarg :win :reader op-win)
   (anchor :initarg :anchor :reader op-anchor :type anchor)
   (state :initform :start :accessor op-state :type op-state)))

(defun take-hold (wm win)
  "Prepare WIN for a pointer op, which needs it floating at a known size, and
return the rect of its offset and size."
  (let ((size (or (unmaximize wm win) (window-size win))))
    (if (window-floating-p win)
        (setf (%window-pending-size win) size)
        (float-window wm win size))
    (rect-at (or (window-float-offset win) (point 0 0)) size)))

(defun start-pointer-op (wm win kind &optional (edges +bottom-right+))
  (when (and win (not (wm-op wm)) (not (window-fullscreen win)))
    (let ((anchor (anchor (take-hold wm win) edges)))
      (setf (wm-op wm) (make-instance 'pointer-op :kind kind :win win :anchor anchor)
            (%window-anchor win) (and (eq kind :resize) anchor)))
    (focus-window wm win)
    (mark-dirty wm)))

(defun end-pointer-op (wm win)
  "End the pointer op on WIN, at once if river has not started it yet."
  (let ((op (wm-op wm)))
    (when (and op (eq (op-win op) win))
      (if (eq (op-state op) :start)
          (setf (wm-op wm) nil)
          (setf (op-state op) :end)))))

(defun update-pointer-op (op dx dy)
  (let ((win (op-win op))
        (anchor (op-anchor op)))
    (ecase (op-kind op)
      (:move
       (setf (%window-float-offset win)
             (point+ (rect-origin (anchor-rect anchor)) (point dx dy))))
      (:resize
       (setf (%window-pending-size win) (resized-size anchor dx dy))))))

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
               (not (eq output (window-output win))))
      (setf (%window-float-offset win) (offset-on output win))
      (move-to-workspace win (output-workspace output))
      (focus-window wm win))))

(defun finish-pointer-op (wm op)
  (river-seat-v1.op-end (wm-seat wm))
  (let ((win (live (op-win op))))
    (when win
      (ecase (op-kind op)
        (:move (settle-floating wm win))
        (:resize (river-window-v1.inform-resize-end (window-proxy win))))))
  (setf (wm-op wm) nil))

(defun manage-pointer-op (wm)
  (let ((op (wm-op wm)))
    (when op
      (ecase (op-state op)
        (:start
         (river-seat-v1.op-start-pointer (wm-seat wm))
         (when (eq (op-kind op) :resize)
           (river-window-v1.inform-resize-start (window-proxy (op-win op))))
         (setf (op-state op) :running))
        (:running)
        (:end (finish-pointer-op wm op))))))

(define-handler (wm (seat river-seat-v1) :pointer-enter) (window)
  (setf (wm-pointer-window wm) (find-window wm window)))

(define-handler (wm (seat river-seat-v1) :pointer-leave) ()
  (setf (wm-pointer-window wm) nil))

(defun running-op (wm)
  (let ((op (wm-op wm)))
    (and op (eq (op-state op) :running) op)))

(define-handler (wm (seat river-seat-v1) :op-delta) (dx dy)
  (let ((op (running-op wm)))
    (when op
      (update-pointer-op op dx dy))))

(define-handler (wm (seat river-seat-v1) :op-release) ()
  (let ((op (running-op wm)))
    (when op
      (setf (op-state op) :end))))

(defun install-pointer-bindings (wm)
  (let ((modifiers (parse-modifiers *pointer-modifiers*)))
    (bind-pointer wm *move-button* modifiers
                  (lambda () (start-pointer-op wm (wm-pointer-window wm) :move)))
    (bind-pointer wm *resize-button* modifiers
                  (lambda () (start-pointer-op wm (wm-pointer-window wm) :resize)))))
