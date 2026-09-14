;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; Every output has its own workspaces and shows one of them. The workspaces
;;; of a removed output are kept under its name; their windows move to another
;;; output and return when the output is connected again.

(defparameter *warp-pointer* t
  "Move the pointer to an output when the focus moves there.")

(defun usable-area (output)
  (if (plusp (output-usable-width output))
      (values (output-usable-x output) (output-usable-y output)
              (output-usable-width output) (output-usable-height output))
      (values (output-x output) (output-y output)
              (output-width output) (output-height output))))

(defun clip-box (x y width height output)
  "NIL if the window at X Y of WIDTH HEIGHT lies within OUTPUT, :HIDDEN if
it lies outside, else the visible part relative to the window."
  (let ((left (output-x output))
        (top (output-y output))
        (right (+ (output-x output) (output-width output)))
        (bottom (+ (output-y output) (output-height output))))
    (cond ((and (<= left x) (<= top y)
                (<= (+ x width) right) (<= (+ y height) bottom))
           nil)
          ((or (<= right x) (<= (+ x width) left)
               (<= bottom y) (<= (+ y height) top))
           :hidden)
          (t (list (- left x) (- top y)
                   (output-width output) (output-height output))))))

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

(defun migrate-windows (from to &key home)
  (dolist (win (workspace-windows from))
    (setf (window-workspace win) to
          (window-home win) home))
  (setf (workspace-windows to) (append (workspace-windows from) (workspace-windows to))
        (workspace-focused to) (or (workspace-focused to) (workspace-focused from))
        (workspace-windows from) '()
        (workspace-focused from) nil))

(defun evacuate-output (wm output target)
  "Move the windows of OUTPUT to the same workspaces of TARGET; with a name,
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

(defun return-home (win)
  (let ((home (window-home win)))
    (remove-window (window-workspace win) win)
    (setf (window-workspace win) home
          (window-home win) nil)
    (push win (workspace-windows home))
    (unless (workspace-focused home)
      (setf (workspace-focused home) win))))

(defun restore-output (wm output)
  "Give a reconnected OUTPUT its old workspaces and bring their windows back."
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

(defun swap-workspace-contents (a b)
  (dolist (win (workspace-windows a))
    (setf (window-workspace win) b))
  (dolist (win (workspace-windows b))
    (setf (window-workspace win) a))
  (rotatef (workspace-windows a) (workspace-windows b))
  (rotatef (workspace-focused a) (workspace-focused b))
  (rotatef (workspace-layout a) (workspace-layout b)))

(defun output-center (output)
  (list (+ (output-x output) (floor (output-width output) 2))
        (+ (output-y output) (floor (output-height output) 2))))

(defun follow-focus (wm previous-output)
  (let ((output (wm-focused-output wm)))
    (when (and *warp-pointer* output (not (eq output previous-output)))
      (setf (wm-pending-warp wm) (output-center output)))
    (mark-dirty wm)))

(defun switch-workspace (wm ws)
  (when (and ws (not (eq ws (wm-active-workspace wm))))
    (let ((previous (wm-focused-output wm)))
      (show-workspace wm ws)
      (follow-focus wm previous))))

(defun sorted-outputs (wm)
  (sort (copy-list (wm-outputs wm))
        (lambda (a b)
          (or (< (output-x a) (output-x b))
              (and (= (output-x a) (output-x b))
                   (< (output-y a) (output-y b)))))))

(defun neighbor-output (wm direction)
  (let* ((outputs (sorted-outputs wm))
         (pos (position (wm-focused-output wm) outputs)))
    (when (and pos (rest outputs))
      (nth (mod (+ pos (step-direction direction)) (length outputs)) outputs))))

(defun focus-output (wm &optional (direction :next))
  (let ((target (neighbor-output wm direction))
        (previous (wm-focused-output wm)))
    (when target
      (setf (wm-active-workspace wm) (output-workspace target))
      (follow-focus wm previous))))

(defun send-to-output (wm &optional (direction :next))
  (let ((target (neighbor-output wm direction)))
    (when target
      (send-to-workspace wm (output-workspace target)))))

(defun move-workspace-to-output (wm &optional (direction :next))
  "Exchange the windows and layout of the active workspace with those of the
workspace shown on the neighboring output, and follow them there."
  (let ((target (neighbor-output wm direction))
        (previous (wm-focused-output wm)))
    (when target
      (swap-workspace-contents (wm-active-workspace wm) (output-workspace target))
      (setf (wm-active-workspace wm) (output-workspace target))
      (follow-focus wm previous))))
