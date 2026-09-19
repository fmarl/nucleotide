;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun focus-window (wm win)
  (show-workspace wm (window-workspace win))
  (setf (workspace-focused (window-workspace win)) win))

(defun effective-focus (wm)
  "While a highlight is active, the highlighted window is the only visible one."
  (or (wm-highlight wm) (wm-focused-window wm)))

(defun cycle-focus (wm &optional (direction :next))
  "Focus the window in DIRECTION: :PREV and :NEXT, or :UP and :DOWN."
  (let* ((ws (wm-active-workspace wm))
         (focused (wm-focused-window wm))
         (target (if focused
                     (layout-neighbor (workspace-layout ws) ws focused direction)
                     (first (workspace-windows ws)))))
    (when (member target (workspace-windows ws))
      (focus-window wm target)
      (mark-dirty wm))))

(defun move-window (wm &optional (direction :next))
  (let ((ws (wm-active-workspace wm))
        (focused (wm-focused-window wm)))
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
    (when (eq (workspace-focused ws) win)
      (setf (workspace-focused ws) (focus-successor ws win remaining)))
    (setf (workspace-windows ws) remaining)))

(defun move-to-workspace (win ws)
  (remove-window (window-workspace win) win)
  (setf (window-workspace win) ws
        (window-home win) nil
        (workspace-focused ws) win)
  (push win (workspace-windows ws)))

(defun close-focused (wm)
  (let ((win (effective-focus wm)))
    (when win
      (push win (wm-pending-closes wm))
      (mark-dirty wm))))

(defun send-to-workspace (wm ws)
  (let ((win (wm-focused-window wm)))
    (when (and win ws (not (eq ws (window-workspace win))))
      (move-to-workspace win ws)
      (mark-dirty wm))))

(defun set-active-layout (wm layout)
  "LAYOUT is a function like TILING or the name of a layout class like
SCROLLING. Choosing the class the workspace already uses keeps its state."
  (unless (and (symbolp layout)
               (find-class layout nil)
               (typep (wm-active-layout wm) layout))
    (setf (wm-active-layout wm) (make-layout layout)))
  (mark-dirty wm))

(defun toggle-highlight (wm app-id)
  (setf (wm-highlight wm)
        (unless (wm-highlight wm)
          (find app-id (all-windows wm)
                :key #'window-app-id
                :test #'equal)))
  (mark-dirty wm))

(defun toggle-fullscreen (wm)
  (let ((win (effective-focus wm)))
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
