;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defconstant +all-edges+
  (logior +river-window-v1-edges-top+ +river-window-v1-edges-bottom+
          +river-window-v1-edges-left+ +river-window-v1-edges-right+))

(defconstant +window-capabilities+
  +river-window-v1-capabilities-fullscreen+)

(defun manage-bindings (wm)
  (mapc #'enable-binding (drain (wm-pending-bindings wm))))

(defun manage-closes (wm)
  (dolist (win (drain (wm-pending-closes wm)))
    (river-window-v1.close (window-proxy win))))

(defun manage-submap (wm)
  (dolist (op (reverse (drain (wm-pending-submap-ops wm))))
    (destructuring-bind (kind . submap) op
      (ecase kind
        (:enter
         (mapc #'river-xkb-binding-v1.enable (submap-bindings submap))
         (when (wm-xkb-seat wm)
           (river-xkb-bindings-seat-v1.ensure-next-key-eaten (wm-xkb-seat wm))))
        (:leave
         (mapc #'river-xkb-binding-v1.disable (submap-bindings submap)))))))

(defun fullscreen-output (wm win request)
  (or request
      (let ((output (window-output wm win)))
        (and output (output-proxy output)))))

(defun enter-fullscreen (win output)
  (river-window-v1.fullscreen (window-proxy win) output)
  (river-window-v1.inform-fullscreen (window-proxy win))
  (setf (window-fullscreen win) output))

(defun exit-fullscreen (win)
  (river-window-v1.exit-fullscreen (window-proxy win))
  (river-window-v1.inform-not-fullscreen (window-proxy win))
  (setf (window-fullscreen win) nil))

(defun manage-fullscreens (wm)
  (dolist (entry (reverse (drain (wm-pending-fullscreens wm))))
    (destructuring-bind (win . request) entry
      (if (eq request :exit)
          (when (window-fullscreen win)
            (exit-fullscreen win))
          (let ((output (fullscreen-output wm win request)))
            (when output
              (enter-fullscreen win output)))))))

(defun manage-fullscreen-outputs (wm)
  "Follow fullscreen windows whose workspace moved to another output."
  (dolist (win (all-windows wm))
    (let ((output (and (window-fullscreen win) (window-output wm win))))
      (when (and output (not (eq (window-fullscreen win) (output-proxy output))))
        (enter-fullscreen win (output-proxy output))))))

(defun initialize-window (wm win)
  (setf (window-rules-applied win) t
        (window-floating win) (and (auto-float-p win) t))
  (apply-window-rules wm win)
  (when (and (window-floating win) (not (window-float-offset win)))
    (setf (window-pending-dimensions win) '(0 0))))

(defun configure-window (win)
  (river-window-v1.set-tiled (window-proxy win)
                             (if (window-floating win)
                                 +river-window-v1-edges-none+
                                 +all-edges+))
  (river-window-v1.use-ssd (window-proxy win))
  (river-window-v1.set-capabilities (window-proxy win) +window-capabilities+)
  (setf (window-configured win) t))

(defun manage-new-windows (wm)
  (dolist (win (all-windows wm))
    (unless (window-rules-applied win)
      (initialize-window wm win)
      (run-event-hooks :window-opened win))
    (unless (window-configured win)
      (configure-window win))))

(defun propose-dimensions (win width height)
  (river-window-v1.propose-dimensions (window-proxy win) width height))

(defun manage-pending-dimensions (wm)
  (dolist (win (all-windows wm))
    (let ((dimensions (drain (window-pending-dimensions win))))
      (when (and dimensions (window-floating win))
        (apply #'propose-dimensions win dimensions)))))

(defun tiled-windows (ws)
  (remove-if (lambda (win) (or (window-fullscreen win) (window-floating win)))
             (workspace-windows ws)))

(defun place-window (win rect output)
  (destructuring-bind (x y width height) rect
    (setf (window-x win) x
          (window-y win) y
          (window-clip win) (and output (clip-box x y width height output)))
    (propose-dimensions win width height)))

(defun manage-layout (ws output)
  (multiple-value-bind (x y width height) (usable-area output)
    (let ((tiled (tiled-windows ws)))
      (loop for win in tiled
            for rect in (layout-arrange (workspace-layout ws) ws tiled x y width height)
            do (place-window win rect output)))))

(defun manage-warp (wm)
  (let ((target (drain (wm-pending-warp wm)))
        (seat (wm-seat wm)))
    (when (and target seat (supports-p seat 3))
      (apply #'river-seat-v1.pointer-warp seat target))))

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
  (let ((focused (effective-focus wm))
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
    (when (and (output-workspace output) (plusp (output-width output)))
      (manage-layout (output-workspace output) output)))
  (manage-highlight wm)
  (manage-pending-dimensions wm)
  (manage-layer-shell wm)
  (manage-focus wm)
  (manage-warp wm)
  (ipc-update wm))
