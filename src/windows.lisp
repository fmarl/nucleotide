;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun focus-window (wm win)
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
  (let* ((ws (wm-active-workspace wm))
         (focused (workspace-focus ws)))
    (when focused
      (layout-move (workspace-layout ws) ws focused direction)
      (mark-dirty wm))))

(defun focus-successor (ws win remaining)
  (let ((candidate (or (window-parent win)
                       (layout-successor (workspace-layout ws) ws win))))
    (if (member candidate remaining)
        candidate
        (first remaining))))

(defun remove-window (ws win)
  (let ((remaining (remove win (workspace-windows ws))))
    (when (eq (workspace-focus ws) win)
      (setf (workspace-focus ws) (focus-successor ws win remaining)))
    (setf (workspace-windows ws) remaining)))

(defun move-to-workspace (win ws)
  (remove-window (window-workspace win) win)
  (setf (window-workspace win) ws
        (window-home win) nil
        (workspace-focus ws) win)
  (push win (workspace-windows ws)))

(define-command close-window (wm)
  (let ((win (wm-focus wm)))
    (when win
      (push win (wm-pending-closes wm))
      (mark-dirty wm))))

(defun send-focused-to (wm ws)
  (let ((win (workspace-focus (wm-active-workspace wm))))
    (when (and win ws (not (eq ws (window-workspace win))))
      (move-to-workspace win ws)
      (mark-dirty wm))))

(define-command set-layout (wm spec)
  "SPEC is in the format MAKE-LAYOUT takes. The bare name of the class the
workspace already uses keeps its state."
  (unless (and (symbolp spec)
               (find-class spec nil)
               (typep (wm-active-layout wm) spec))
    (setf (wm-active-layout wm) (make-layout spec)))
  (mark-dirty wm))

(define-command toggle-fullscreen (wm)
  (let ((win (wm-focus wm)))
    (when win
      ;; Fullscreen state only changes in the next manage sequence, so a
      ;; second toggle before that cancels the pending one.
      (let ((pending (assoc win (wm-pending-fullscreens wm))))
        (if pending
            (setf (wm-pending-fullscreens wm)
                  (remove pending (wm-pending-fullscreens wm)))
            (push (cons win (if (window-fullscreen win) :exit nil))
                  (wm-pending-fullscreens wm))))
      (mark-dirty wm))))

(define-command send-to-workspace (wm number)
  (send-focused-to wm (nth-workspace wm (1- number))))

(defun attach-window (wm proxy)
  (let ((win (make-instance 'window :proxy proxy
                            :node (river-window-v1.get-node proxy)
                            :workspace (wm-active-workspace wm))))
    (push win (active-windows wm))
    (focus-window wm win)
    (forward-events wm proxy win)))

(defun detach-window (wm win)
  (remove-window (window-workspace win) win)
  (when (eq (wm-highlight wm) win)
    (setf (wm-highlight wm) nil))
  (when (eq (wm-pointer-window wm) win)
    (setf (wm-pointer-window wm) nil))
  (when (and (wm-op wm) (eq (op-win (wm-op wm)) win))
    (setf (op-state (wm-op wm)) :end))
  (setf (wm-pending-closes wm) (remove win (wm-pending-closes wm))
        (wm-pending-fullscreens wm) (remove win (wm-pending-fullscreens wm)
                                            :key #'car)
        (wm-focus-history wm) (remove win (wm-focus-history wm)))
  (when (eq (wm-last-focus wm) win)
    (setf (wm-last-focus wm) nil))
  (river-node-v1.destroy (window-node win))
  (river-window-v1.destroy (window-proxy win))
  (run-event-hooks :window-closed win))

(define-handler (wm (win window) :closed) ()
  (detach-window wm win))

(define-handler (wm (win window) :title) (title)
  (setf (window-title win) title)
  (when (window-rules-applied win)
    (run-event-hooks :window-title win title)))

(define-handler (wm (win window) :app-id) (app-id)
  (setf (window-app-id win) app-id))

(define-handler (wm (win window) :parent) (parent)
  (setf (window-parent win) (and parent (find-window wm parent))))

(define-handler (wm (win window) :dimensions) (width height)
  (setf (window-width win) width
        (window-height win) height))

(define-handler (wm (win window) :dimensions-hint) (min-width min-height max-width max-height)
  (setf (window-min-width win) min-width
        (window-min-height win) min-height
        (window-max-width win) max-width
        (window-max-height win) max-height))

(define-handler (wm (win window) :fullscreen-requested) (output)
  (push (cons win output) (wm-pending-fullscreens wm)))

(define-handler (wm (win window) :exit-fullscreen-requested) ()
  (push (cons win :exit) (wm-pending-fullscreens wm)))
