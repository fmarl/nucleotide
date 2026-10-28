;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *warp-pointer* t
  :documentation "Move the pointer to an output when the focus moves there.")

(defun usable-area (output)
  (let ((usable (output-usable output)))
    (if (and usable (plusp (rect-width usable)))
        usable
        (output-rect output))))

(defun clip-box (rect output)
  "NIL if RECT lies within OUTPUT, :HIDDEN if it lies outside, else the box of
OUTPUT relative to RECT to clip it to."
  (let ((area (output-rect output)))
    (cond ((rect-within-p rect area) nil)
          ((zerop (overlap-area rect area)) :hidden)
          (t (rect (- (rect-x area) (rect-x rect)) (- (rect-y area) (rect-y rect))
                   (rect-width area) (rect-height area))))))

(defun window-output (wm win)
  (or (workspace-output (window-workspace win)) (wm-focused-output wm)))

(defun workspace-shown-p (ws)
  (let ((output (workspace-output ws)))
    (and output (eq ws (output-workspace output)))))

(defun nth-workspace (wm n)
  "The Nth workspace of the focused output."
  (let ((output (wm-focused-output wm)))
    (nth n (if output (output-workspaces output) (wm-orphans wm)))))

(defun adopt-workspaces (output workspaces)
  (dolist (ws workspaces)
    (setf (workspace-output ws) output))
  (setf (output-workspaces output) workspaces
        (output-workspace output) (first workspaces)))

(defun assign-output (wm output)
  "Give a new OUTPUT the workspaces without an output, or fresh ones."
  (let ((workspaces (or (drain (wm-orphans wm)) (make-workspaces))))
    (adopt-workspaces output workspaces)
    (when (member (wm-active-workspace wm) workspaces)
      (setf (output-workspace output) (wm-active-workspace wm)))))

(defun evacuate-output (wm output target)
  "Move the windows of OUTPUT to the same workspaces of TARGET. With a name,
keep OUTPUT's workspaces so the windows can return to them."
  (let ((workspaces (output-workspaces output))
        (name (output-name output)))
    (mapc (lambda (ws there) (migrate-windows ws there :home (and name ws)))
          workspaces (output-workspaces target))
    (when name
      (setf (wm-stash wm) (alist-set name workspaces (wm-stash wm))))
    (when (member (wm-active-workspace wm) workspaces)
      (setf (wm-active-workspace wm) (output-workspace target)))))

(defun unassign-output (wm output)
  (let ((target (find output (wm-outputs wm) :test-not #'eq)))
    (setf (wm-outputs wm) (remove output (wm-outputs wm)))
    (dolist (ws (output-workspaces output))
      (setf (workspace-output ws) nil))
    (if target
        (evacuate-output wm output target)
        (setf (wm-orphans wm) (output-workspaces output)))))

(defun restore-output (wm output)
  (let ((stashed (alist-get (output-name output) (wm-stash wm))))
    (when stashed
      (let ((current (output-workspaces output))
            (shown (position (output-workspace output) (output-workspaces output))))
        (setf (wm-stash wm) (alist-remove (output-name output) (wm-stash wm)))
        (mapc #'migrate-windows current stashed)
        (adopt-workspaces output stashed)
        (setf (output-workspace output) (nth shown stashed))
        (when (member (wm-active-workspace wm) current)
          (setf (wm-active-workspace wm) (output-workspace output)))
        (dolist (win (all-windows wm))
          (when (member (window-home win) stashed)
            (return-home win)))))))

(defun show-workspace (wm ws)
  (when (workspace-output ws)
    (setf (output-workspace (workspace-output ws)) ws))
  (setf (wm-active-workspace wm) ws))

(defun follow-focus (wm previous-output)
  (let ((output (wm-focused-output wm)))
    (when (and *warp-pointer* output (not (eq output previous-output)))
      (request wm :warp (rect-center (output-rect output))))
    (mark-dirty wm)))

(defun switch-to-workspace (wm ws)
  (when (and ws (not (eq ws (wm-active-workspace wm))))
    (let ((previous (wm-focused-output wm)))
      (show-workspace wm ws)
      (follow-focus wm previous))))

(defun sorted-outputs (wm)
  (sort (copy-list (wm-outputs wm))
        (lambda (a b)
          (let ((a (output-rect a))
                (b (output-rect b)))
            (or (< (rect-x a) (rect-x b))
                (and (= (rect-x a) (rect-x b))
                     (< (rect-y a) (rect-y b))))))))

(defun neighbor-output (wm direction)
  (let* ((outputs (sorted-outputs wm))
         (pos (position (wm-focused-output wm) outputs)))
    (when (and pos (rest outputs))
      (nth (mod (+ pos (step-direction direction)) (length outputs)) outputs))))

(define-command focus-output (wm &optional (direction :next))
  "Focus the output in DIRECTION: :LEFT, :RIGHT, :PREV or :NEXT."
  (let ((target (neighbor-output wm direction))
        (previous (wm-focused-output wm)))
    (when target
      (setf (wm-active-workspace wm) (output-workspace target))
      (follow-focus wm previous))))

(define-command send-to-output (wm &optional (direction :next))
  "Send the focused window to the output in DIRECTION."
  (let ((target (neighbor-output wm direction)))
    (when target
      (send-focused-to wm (output-workspace target)))))

(define-command move-workspace-to-output (wm &optional (direction :next))
  "Exchange the windows and layout of the active workspace with those of the
workspace shown on the neighboring output, and follow them there."
  (let ((target (neighbor-output wm direction))
        (previous (wm-focused-output wm)))
    (when target
      (swap-workspace-contents (wm-active-workspace wm) (output-workspace target))
      (setf (wm-active-workspace wm) (output-workspace target))
      (follow-focus wm previous))))

(define-command switch-workspace (wm number)
  "Show workspace NUMBER of the focused output."
  (switch-to-workspace wm (nth-workspace wm (1- number))))

(defun attach-output (wm proxy)
  (let ((output (make-instance 'output :proxy proxy)))
    (setf (wm-outputs wm) (append (wm-outputs wm) (list output)))
    (assign-output wm output)
    (forward-events wm proxy output)
    (when (wm-layer-shell wm)
      (let ((layer-shell (river-layer-shell-v1.get-output (wm-layer-shell wm)
                                                          proxy)))
        (setf (output-layer-shell output) layer-shell)
        (forward-events wm layer-shell output)))))

(define-handler (wm (output output) :wl-output) (global-name)
  (let ((version (gethash global-name (wm-wl-output-versions wm))))
    (when (and version (>= version 4))
      (let ((wl-output (wl-registry.bind (wm-registry wm) global-name 'wl-output 4)))
        (setf (output-wl-output output) wl-output)
        (forward-events wm wl-output output)))))

(define-handler (wm (output output) :name) (name)
  (setf (output-name output) name)
  (restore-output wm output)
  (mark-dirty wm))

(define-handler (wm (output output) :position) (x y)
  (let ((rect (output-rect output)))
    (setf (output-rect output) (rect x y (rect-width rect) (rect-height rect)))))

(define-handler (wm (output output) :dimensions) (width height)
  (let ((rect (output-rect output)))
    (setf (output-rect output) (rect (rect-x rect) (rect-y rect) width height))))

(define-handler (wm (output output) :non-exclusive-area) (x y width height)
  (setf (output-usable output) (rect x y width height)))

(define-handler (wm (output output) :removed) ()
  (unassign-output wm output)
  (dolist (win (all-windows wm))
    (when (eq (window-fullscreen win) (output-proxy output))
      (request wm :exit-fullscreen win)))
  (mark-dirty wm)
  (when (output-wl-output output)
    (wl-output.release (output-wl-output output)))
  (when (output-layer-shell output)
    (river-layer-shell-output-v1.destroy (output-layer-shell output)))
  (river-output-v1.destroy (output-proxy output)))
