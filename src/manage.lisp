;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defconstant +all-edges+
  (logior +river-window-v1-edges-top+ +river-window-v1-edges-bottom+
          +river-window-v1-edges-left+ +river-window-v1-edges-right+))

(defconstant +window-capabilities+
  (logior +river-window-v1-capabilities-fullscreen+ +river-window-v1-capabilities-maximize+))

(defun manage-binding-layouts (wm)
  (drain-requests (wm)
                  (:configure-binding (binding)
                                      (when *bindings-layout*
                                        (river-xkb-binding-v1.set-layout-override
                                         binding *bindings-layout*)))))

(defun manage-bindings (wm)
  "No binding is enabled while the session is locked. Unlocking enables those
outside of submaps again, new ones among them."
  (manage-binding-layouts wm)
  (let ((new (mapcar #'second (take-requests wm '(:enable-binding))))
        (locked (wm-locked wm)))
    (cond ((eq locked (wm-bindings-locked wm))
           (unless locked
             (mapc #'enable-binding new)))
          (locked (mapc #'disable-binding (all-bindings wm)))
          (t (mapc #'enable-binding (wm-bindings wm))))
    (setf (wm-bindings-locked wm) locked)))

(defun manage-closes (wm)
  (drain-requests (wm)
                  (:close (win) (river-window-v1.close (window-proxy win)))))

(defun manage-submap (wm)
  (drain-requests (wm)
                  (:enter-submap (submap)
                                 (mapc #'river-xkb-binding-v1.enable (submap-bindings submap))
                                 (let ((seat (wm-xkb-seat wm)))
                                   (when seat
                                     (river-xkb-bindings-seat-v1.ensure-next-key-eaten seat))))
                  (:leave-submap (submap)
                                 (mapc #'river-xkb-binding-v1.disable (submap-bindings submap)))))

(defun fullscreen-output (wm win requested)
  (if (member requested (wm-outputs wm))
      requested
      (window-output win)))

(defun enter-fullscreen (win output)
  (river-window-v1.fullscreen (window-proxy win) (output-proxy output))
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
    (let ((output (and (window-fullscreen win) (window-output win))))
      (when (and output (not (eq output (window-fullscreen win))))
        (enter-fullscreen win output)))))

(defun manage-maximized (wm)
  (drain-requests (wm)
                  (:inform-maximized (win)
                                     (if (window-maximized-p win)
                                         (river-window-v1.inform-maximized (window-proxy win))
                                         (river-window-v1.inform-unmaximized (window-proxy win))))))

(defun configure-window (win)
  "Only windows that support nothing else draw their own decorations."
  (river-window-v1.set-tiled (window-proxy win)
                             (if (window-floating-p win)
                                 +river-window-v1-edges-none+
                                 +all-edges+))
  (if (eql (window-decoration-hint win) +river-window-v1-decoration-hint-only-supports-csd+)
      (river-window-v1.use-csd (window-proxy win))
      (river-window-v1.use-ssd (window-proxy win)))
  (river-window-v1.set-capabilities (window-proxy win) +window-capabilities+))

(defun new-window-output (wm)
  (or (and *open-on-pointer-output* (pointer-output wm))
      (wm-focused-output wm)))

(defun place-new-window (wm win)
  "Without a workspace rule, WIN opens with its shown parent or on
NEW-WINDOW-OUTPUT and gets the focus. A dialog of the highlighted window joins
it. Only the manage sequence knows all of them."
  (let* ((output (new-window-output wm))
         (rule (rule-workspace wm (window-actions win) output))
         (parent (window-parent win))
         (highlight (wm-highlight wm))
         (joins-highlight (and (not rule) (dialog-of-p win highlight)))
         (ws (cond (rule)
                   (joins-highlight (window-workspace highlight))
                   ((and parent (workspace-shown-p (window-workspace parent)))
                    (window-workspace parent))
                   (output (output-workspace output))
                   (t (window-workspace win)))))
    (unless (eq ws (window-workspace win))
      (move-to-workspace win ws))
    (cond (joins-highlight (setf (workspace-focus ws) win))
          ((or (not rule) (eq rule (wm-active-workspace wm)))
           (focus-window wm win)))))

(defun manage-new-windows (wm)
  "Rules come first, so that placing a window knows whether it floats."
  (dolist (win (all-windows wm))
    (unless (window-rules-applied-p win)
      (setf (%window-rules-applied-p win) t)
      (apply-window-rules wm win)
      (place-new-window wm win)
      (configure-window win)
      (run-event-hooks :window-opened win)))
  (drain-requests (wm)
                  (:configure (win) (configure-window win))))

(defun propose-size (win size)
  (river-window-v1.propose-dimensions (window-proxy win) (size-width size) (size-height size)))

(defun layout-windows (ws)
  "The windows the layout of WS manages: those that do not float. Maximized ones
keep their place."
  (remove-if #'window-floating-p (workspace-windows ws)))

(defun place-window (win rect output)
  "Large gaps or many windows squeeze rects below a pixel. River rejects negative
sizes, and 0 lets the window choose."
  (let ((rect (rect (rect-x rect) (rect-y rect)
                    (max 1 (rect-width rect)) (max 1 (rect-height rect)))))
    (position-window win (rect-origin rect) (and output (clip-box rect output)))
    (propose-size win (rect-size rect))))

(defun manage-layout (ws output)
  "Place the tiled and maximized windows of WS, which OUTPUT shows. River places
fullscreen windows, and floating ones are placed when rendering."
  (let* ((area (usable-area output))
         (layout (workspace-layout ws))
         (managed (layout-windows ws))
         (arranged (remove-if #'window-fullscreen managed)))
    (layout-update layout ws managed area)
    (let ((rects (mapcar #'cons arranged (layout-arrange layout ws arranged area))))
      (dolist (win (workspace-windows ws))
        (case (window-placement win)
          (:tiled (place-window win (cdr (assoc win rects)) output))
          (:maximized (place-window win area output)))))))

(defun manage-warp (wm)
  (let ((target (second (first (last (take-requests wm '(:warp))))))
        (seat (wm-seat wm)))
    (when (and target seat (supports-p seat 3))
      (river-seat-v1.pointer-warp seat (point-x target) (point-y target)))))

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
  "A layer shell surface with non-exclusive focus keeps it until the focus moves
to another window or a window is clicked."
  (let ((focused (wm-focus wm))
        (seat (wm-seat wm))
        (clicked (take-requests wm '(:interaction))))
    (when (and seat (ecase (wm-layer-shell-focus wm)
                      ((nil) t)
                      (:non-exclusive (or clicked (not (eq focused (wm-last-focus wm)))))
                      (:exclusive nil)))
      (if focused
          (river-seat-v1.focus-window seat (window-proxy focused))
          (river-seat-v1.clear-focus seat)))
    (note-focus wm focused)))

(defun manage-layer-shell (wm)
  (let ((output (wm-focused-output wm)))
    (when (and output (output-layer-shell output))
      (river-layer-shell-output-v1.set-default (output-layer-shell output)))))

(defun manage (wm)
  "The steps depend on their order: window states settle before pointer ops start
or end, ops before the layout places windows, and the layout before pending
sizes, focus and IPC see the result."
  (manage-bindings wm)
  (manage-closes wm)
  (manage-submap wm)
  (manage-new-windows wm)
  (manage-fullscreens wm)
  (manage-fullscreen-outputs wm)
  (manage-maximized wm)
  (manage-pointer-op wm)
  (dolist (output (wm-outputs wm))
    (when (and (output-workspace output) (output-ready-p output))
      (manage-layout (output-workspace output) output)))
  (manage-highlight wm)
  (manage-pending-sizes wm)
  (manage-layer-shell wm)
  (manage-focus wm)
  (manage-warp wm)
  (ipc-update wm))
