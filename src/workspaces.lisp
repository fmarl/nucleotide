;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; A window is in the window list of its workspace, and the focus of a
;;; workspace is one of its windows or NIL. Only the functions here change
;;; which workspace a window is in.

(defmethod (setf workspace-focus) :before (win (ws workspace))
  (assert (or (null win) (member win (workspace-windows ws))) ()
          "~S is not a window of ~S" win ws))

(defun add-window (ws win &key (focus t))
  "Add WIN, which is in no workspace, to WS. It gets the focus of WS with FOCUS,
or when WS has none."
  (assert (null (window-workspace win)) () "~S is already in ~S" win (window-workspace win))
  (setf (%window-workspace win) ws
        (%workspace-windows ws) (cons win (workspace-windows ws)))
  (when (or focus (null (workspace-focus ws)))
    (setf (workspace-focus ws) win)))

(defun focus-successor (ws win remaining)
  (let ((candidate (or (window-parent win)
                       (layout-successor (workspace-layout ws) ws win))))
    (if (member candidate remaining)
        candidate
        (first remaining))))

(defun remove-window (win)
  (let* ((ws (window-workspace win))
         (remaining (remove win (workspace-windows ws))))
    (when (eq (workspace-focus ws) win)
      (setf (workspace-focus ws) (focus-successor ws win remaining)))
    (setf (%workspace-windows ws) remaining
          (%window-workspace win) nil)))

(defun move-to-workspace (win ws)
  "Moved on purpose, WIN no longer returns to a removed output."
  (remove-window win)
  (setf (%window-home win) nil)
  (add-window ws win))

(defun return-home (win)
  (let ((home (window-home win)))
    (remove-window win)
    (setf (%window-home win) nil)
    (add-window home win :focus nil)))

(defun swap-windows (ws a b)
  (setf (%workspace-windows ws) (swap (workspace-windows ws) a b)))

(defun migrate-windows (from to &key home)
  "Move all windows of FROM to TO. Windows that already have a home keep it."
  (dolist (win (workspace-windows from))
    (setf (%window-workspace win) to
          (%window-home win) (or (window-home win) home)))
  (setf (%workspace-windows to) (append (workspace-windows from) (workspace-windows to))
        (workspace-focus to) (or (workspace-focus to) (workspace-focus from))
        (workspace-focus from) nil
        (%workspace-windows from) '()))

(defun swap-workspace-contents (a b)
  "Windows that are moved on purpose no longer return to a removed output."
  (dolist (win (workspace-windows a))
    (setf (%window-workspace win) b
          (%window-home win) nil))
  (dolist (win (workspace-windows b))
    (setf (%window-workspace win) a
          (%window-home win) nil))
  (let ((windows-of-a (workspace-windows a)))
    (setf (%workspace-windows a) (workspace-windows b)
          (%workspace-windows b) windows-of-a))
  (rotatef (workspace-focus a) (workspace-focus b))
  (rotatef (workspace-layout a) (workspace-layout b)))
