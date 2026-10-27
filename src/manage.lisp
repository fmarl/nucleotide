;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defconstant +all-edges+
  (logior +river-window-v1-edges-top+ +river-window-v1-edges-bottom+
          +river-window-v1-edges-left+ +river-window-v1-edges-right+))

(defconstant +window-capabilities+
  +river-window-v1-capabilities-fullscreen+)

(defun manage-bindings (wm)
  (drain-requests (wm)
                  (:enable-binding (binding) (enable-binding binding))))

(defun manage-closes (wm)
  (drain-requests (wm)
                  (:close (win) (river-window-v1.close (window-proxy win)))))

(defun manage-submap (wm)
  (drain-requests (wm)
                  (:enter-submap (submap)
                                 (mapc #'river-xkb-binding-v1.enable (submap-bindings submap))
                                 (when (wm-xkb-seat wm)
                                   (river-xkb-bindings-seat-v1.ensure-next-key-eaten (wm-xkb-seat wm))))
                  (:leave-submap (submap)
                                 (mapc #'river-xkb-binding-v1.disable (submap-bindings submap)))))

(defun fullscreen-output (wm win request)
  (or request
      (let ((output (window-output wm win)))
        (and output (output-proxy output)))))

(defun enter-fullscreen (win output)
  (river-window-v1.fullscreen (window-proxy win) output)
  (river-window-v1.inform-fullscreen (window-proxy win))
  (setf (%window-fullscreen win) output))

(defun exit-fullscreen (win)
  (river-window-v1.exit-fullscreen (window-proxy win))
  (river-window-v1.inform-not-fullscreen (window-proxy win))
  (setf (%window-fullscreen win) nil))

(defun manage-fullscreens (wm)
  (drain-requests (wm)
                  (:fullscreen (win requested)
                               (let ((output (fullscreen-output wm win requested)))
                                 (when output
                                   (enter-fullscreen win output))))
                  (:exit-fullscreen (win)
                                    (when (window-fullscreen win)
                                      (exit-fullscreen win)))))

(defun manage-fullscreen-outputs (wm)
  "Follow fullscreen windows whose workspace moved to another output."
  (dolist (win (all-windows wm))
    (let ((output (and (window-fullscreen win) (window-output wm win))))
      (when (and output (not (eq (window-fullscreen win) (output-proxy output))))
        (enter-fullscreen win (output-proxy output))))))

(defun configure-window (win)
  (river-window-v1.set-tiled (window-proxy win)
                             (if (window-floating-p win)
                                 +river-window-v1-edges-none+
                                 +all-edges+))
  (river-window-v1.use-ssd (window-proxy win))
  (river-window-v1.set-capabilities (window-proxy win) +window-capabilities+))

(defun manage-new-windows (wm)
  (dolist (win (all-windows wm))
    (unless (window-rules-applied-p win)
      (setf (%window-rules-applied-p win) t)
      (apply-window-rules wm win)
      (configure-window win)
      (run-event-hooks :window-opened win)))
  (drain-requests (wm)
                  (:configure (win) (configure-window win))))

(defun propose-dimensions (win width height)
  (river-window-v1.propose-dimensions (window-proxy win) width height))

(defun tiled-windows (ws)
  (remove-if (lambda (win) (or (window-fullscreen win) (window-floating-p win)))
             (workspace-windows ws)))

(defun place-window (win rect output)
  (position-window win (rect-x rect) (rect-y rect) (and output (clip-box rect output)))
  (propose-dimensions win (rect-width rect) (rect-height rect)))

(defun manage-layout (ws output)
  (let ((area (usable-area output))
        (tiled (tiled-windows ws))
        (layout (workspace-layout ws)))
    (layout-update layout ws tiled area)
    (loop for win in tiled
          for rect in (layout-arrange layout ws tiled area)
          do (place-window win rect output))))

(defun manage-warp (wm)
  (let ((warp (first (last (take-requests wm '(:warp)))))
        (seat (wm-seat wm)))
    (when (and warp seat (supports-p seat 3))
      (apply #'river-seat-v1.pointer-warp seat (rest warp)))))

(defun note-focus (wm focused)
  (unless (eq focused (wm-last-focus wm))
    (setf (wm-last-focus wm) focused)
    (when focused
      (setf (wm-focus-history wm) (cons focused (remove focused (wm-focus-history wm)))))
    (run-event-hooks :focus-changed focused))
  (let ((ws (wm-active-workspace wm)))
    (unless (eq ws (wm-last-workspace wm))
      (setf (wm-last-workspace wm) ws)
      (run-event-hooks :workspace-changed ws))))

(defun manage-focus (wm)
  (let ((focused (wm-focus wm))
        (seat (wm-seat wm)))
    (when (and seat (not (wm-layer-shell-focus wm)))
      (if focused
          (river-seat-v1.focus-window seat (window-proxy focused))
          (river-seat-v1.clear-focus seat)))
    (note-focus wm focused)))

(defun manage-layer-shell (wm)
  (let ((output (wm-focused-output wm)))
    (when (and output (output-layer-shell output))
      (river-layer-shell-output-v1.set-default (output-layer-shell output)))))

(defun manage (wm)
  (manage-bindings wm)
  (manage-closes wm)
  (manage-submap wm)
  (manage-new-windows wm)
  (manage-fullscreens wm)
  (manage-fullscreen-outputs wm)
  (manage-pointer-op wm)
  (dolist (output (wm-outputs wm))
    (when (and (output-workspace output) (plusp (rect-width (output-rect output))))
      (manage-layout (output-workspace output) output)))
  (manage-highlight wm)
  (manage-pending-dimensions wm)
  (manage-layer-shell wm)
  (manage-focus wm)
  (manage-warp wm)
  (ipc-update wm))
