;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun focus-window (wm win)
  "Focus WIN and show its workspace."
  (show-workspace wm (window-workspace win))
  (setf (workspace-focus (window-workspace win)) win))

(define-command focus (wm &optional (direction :next))
  "Focus the window in DIRECTION: :LEFT, :RIGHT, :UP or :DOWN."
  (let* ((ws (wm-active-workspace wm))
         (focused (workspace-focus ws))
         (target (if focused
                     (layout-neighbor (workspace-layout ws) ws focused direction)
                     (first (workspace-windows ws)))))
    (when (member target (workspace-windows ws))
      (focus-window wm target)
      (mark-dirty wm))))

(define-command move (wm &optional (direction :next))
  "Move the focused window in DIRECTION: :LEFT, :RIGHT, :UP or :DOWN."
  (let* ((ws (wm-active-workspace wm))
         (focused (workspace-focus ws)))
    (when focused
      (layout-move (workspace-layout ws) ws focused direction)
      (mark-dirty wm))))

(define-command close-window (wm)
  "Ask the focused window to close."
  (let ((win (wm-focus wm)))
    (when win
      (request wm :close win)
      (mark-dirty wm))))

(defun send-focused-to (wm ws)
  (let ((win (workspace-focus (wm-active-workspace wm))))
    (when (and win ws (not (eq ws (window-workspace win))))
      (move-to-workspace win ws)
      (mark-dirty wm))))

(define-command set-layout (wm spec)
  "Give the active workspace the layout SPEC, like TILING or (TILING :RATIO 1/2).
Naming the current layout keeps its state."
  (unless (and (symbolp spec)
               (find-class spec nil)
               (typep (wm-active-layout wm) spec))
    (setf (wm-active-layout wm) (make-layout spec)))
  (mark-dirty wm))

(define-command toggle-fullscreen (wm)
  "Make the focused window fullscreen, or end its fullscreen."
  (let ((win (wm-focus wm)))
    (when win
      (cond ((cancel-requests wm '(:fullscreen :exit-fullscreen) :about win))
            ((window-fullscreen win) (request wm :exit-fullscreen win))
            (t (request wm :fullscreen win nil)))
      (mark-dirty wm))))

(defun inform-maximized (wm win)
  "Tell WIN in the next manage sequence whether it ends up maximized."
  (cancel-requests wm '(:inform-maximized) :about win)
  (request wm :inform-maximized win))

(defun maximize (wm win)
  "Let WIN cover the usable area of its output. This ends a pointer op on it."
  (unless (window-maximized-p win)
    (end-pointer-op wm win)
    (setf (%window-maximized-p win) t
          (%window-restore-size win) (and (not (window-fullscreen win))
                                          (plusp (window-width win))
                                          (plusp (window-height win))
                                          (window-size win))
          (%window-pending-size win) nil)
    (inform-maximized wm win)))

(defun unmaximize (wm win)
  "End the maximized state of WIN and return the size it had, if known. A floating
window gets it back, or picks its own."
  (when (window-maximized-p win)
    (let ((restored (window-restore-size win)))
      (setf (%window-maximized-p win) nil
            (%window-restore-size win) nil)
      (when (window-floating-p win)
        (setf (%window-pending-size win) (or restored (size 0 0))))
      (inform-maximized wm win)
      restored)))

(define-command toggle-maximize (wm)
  "Let the focused window cover the usable area of its output, or end that."
  (let ((win (wm-focus wm)))
    (when win
      (if (window-maximized-p win)
          (unmaximize wm win)
          (maximize wm win))
      (mark-dirty wm))))

(define-command send-to-workspace (wm number)
  "Send the focused window to workspace NUMBER of the focused output."
  (send-focused-to wm (nth-workspace wm (1- number))))

(defun attach-window (wm proxy)
  (let ((win (make-instance 'window :proxy proxy :node (river-window-v1.get-node proxy))))
    (add-window (wm-active-workspace wm) win)
    (focus-window wm win)
    (forward-events wm proxy win)))

(defun detach-window (wm win)
  (remove-window win)
  (setf (%window-closed-p win) t)
  (forget-window-requests wm win)
  (end-pointer-op wm win)
  (river-node-v1.destroy (window-node win))
  (river-window-v1.destroy (window-proxy win))
  (run-event-hooks :window-closed win))

(define-handler (wm (win window) :closed) ()
  (detach-window wm win))

(define-handler (wm (win window) :title) (title)
  (setf (%window-title win) title)
  (when (window-rules-applied-p win)
    (run-event-hooks :window-title win title)))

(define-handler (wm (win window) :app-id) (app-id)
  (setf (%window-app-id win) app-id))

(define-handler (wm (win window) :parent) (parent)
  (setf (%window-parent win) (and parent (find-window wm parent))))

(define-handler (wm (win window) :dimensions) (width height)
  (setf (%window-width win) width
        (%window-height win) height))

(define-handler (wm (win window) :dimensions-hint) (min-width min-height max-width max-height)
  (setf (%window-min-width win) min-width
        (%window-min-height win) min-height
        (%window-max-width win) max-width
        (%window-max-height win) max-height))

(define-handler (wm (win window) :fullscreen-requested) (output)
  (request wm :fullscreen win output))

(define-handler (wm (win window) :exit-fullscreen-requested) ()
  (request wm :exit-fullscreen win))

(define-handler (wm (win window) :maximize-requested) ()
  (maximize wm win))

(define-handler (wm (win window) :unmaximize-requested) ()
  (unmaximize wm win))

(define-handler (wm (win window) :decoration-hint) (hint)
  (setf (%window-decoration-hint win) hint)
  (when (window-rules-applied-p win)
    (request wm :configure win)))

(define-handler (wm (win window) :pointer-move-requested) (seat)
  (when (and (eq seat (wm-seat wm)) (window-visible-p wm win))
    (start-pointer-op wm win :move)))

(define-handler (wm (win window) :pointer-resize-requested) (seat edges)
  (when (and (eq seat (wm-seat wm)) (window-visible-p wm win))
    (start-pointer-op wm win :resize edges)))
