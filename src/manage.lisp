;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *border-width* 2)

(defparameter *focused-border-rgba* '(111 140 171 255))

(defparameter *unfocused-border-rgba* '(171 171 171 255))

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
      (initialize-window wm win))
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
  "Move WIN to RECT, clipped to OUTPUT if given."
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

(defun manage-highlight (wm)
  (let ((win (wm-highlight wm))
        (output (wm-focused-output wm)))
    (when (and win output (not (window-floating win)) (not (window-fullscreen win)))
      (multiple-value-bind (x y width height) (usable-area output)
        (place-window win (first (monocle 1 x y width height)) nil)))))

(defun manage-warp (wm)
  (let ((target (drain (wm-pending-warp wm)))
        (seat (wm-seat wm)))
    (when (and target seat (supports-p seat 3))
      (apply #'river-seat-v1.pointer-warp seat target))))

(defun manage-focus (wm)
  (let ((focused (effective-focus wm)))
    (when (and (wm-seat wm) focused (not (wm-layer-shell-focus wm)))
      (river-seat-v1.focus-window (wm-seat wm) (window-proxy focused)))))

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
  (manage-warp wm))

(defun rgba->uint32 (rgba)
  (mapcar (lambda (x)
            (round (* x (/ #xffffffff 255))))
          rgba))

(defun window-visible-p (wm win)
  (let ((ws (window-workspace win))
        (highlight (wm-highlight wm)))
    (cond ((eq win highlight) t)
          ((not (workspace-shown-p ws)) nil)
          ((and highlight (eq ws (wm-active-workspace wm))) nil)
          ((or (window-fullscreen win) (window-floating win)) t)
          (t (not (eq (window-clip win) :hidden))))))

(defun stacking-rank (wm win)
  (+ (cond ((window-fullscreen win) 4)
           ((window-floating win) 2)
           (t 0))
     (if (eq win (effective-focus wm)) 1 0)))

(defun clip-request (win)
  "The set_clip_box arguments for WIN; an empty box disables clipping."
  (if (consp (window-clip win)) (window-clip win) '(0 0 0 0)))

(defun border-color (wm win)
  (rgba->uint32 (if (eq (effective-focus wm) win)
                    *focused-border-rgba*
                    *unfocused-border-rgba*)))

(defun render-show (wm win)
  (when (and (window-floating win) (plusp (window-width win)))
    (place-floating wm win))
  (river-window-v1.show (window-proxy win))
  (apply #'river-window-v1.set-borders (window-proxy win) +all-edges+ *border-width*
         (border-color wm win))
  (when (supports-p (window-proxy win) 2)
    (apply #'river-window-v1.set-clip-box (window-proxy win) (clip-request win)))
  (river-node-v1.set-position (window-node win) (window-x win) (window-y win))
  (river-node-v1.place-top (window-node win)))

(defun render (wm)
  (let* ((windows (all-windows wm))
         (visible (remove-if-not (lambda (win) (window-visible-p wm win)) windows)))
    (dolist (win (set-difference windows visible))
      (river-window-v1.hide (window-proxy win)))
    (dolist (win (stable-sort (reverse visible) #'<
                              :key (lambda (win) (stacking-rank wm win))))
      (render-show wm win))))
