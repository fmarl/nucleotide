;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite floating :in :nucleotide)
(in-suite floating)

(defun make-win (&key parent (min '(0 0)) (max '(0 0)))
  (let ((win (make-instance 'n:window :workspace nil)))
    (setf (n:window-parent win) parent
          (n::window-min-width win) (first min)
          (n::window-min-height win) (second min)
          (n::window-max-width win) (first max)
          (n::window-max-height win) (second max))
    win))

(test dialogs-float
  (is (n::auto-float-p (make-win :parent (make-win))))
  (is (n::auto-float-p (make-win :min '(400 300) :max '(400 300))))
  (is (not (n::auto-float-p (make-win))))
  (is (not (n::auto-float-p (make-win :min '(400 300) :max '(800 600))))))

(test floating-windows-start-centered
  (is (equal '(300 250) (n::center-in-area 400 300 1000 800))))

(test floating-windows-stay-out-of-the-layout
  (let* ((ws (make-instance 'n:workspace :name "test"))
         (tiled (make-instance 'n:window :workspace ws))
         (dialog (make-instance 'n:window :workspace ws)))
    (setf (n:workspace-windows ws) (list dialog tiled)
          (n:window-floating dialog) t)
    (is (equal (list tiled) (n::tiled-windows ws)))))

(test closing-a-dialog-focuses-its-parent
  (let* ((ws (make-instance 'n:workspace :name "test"))
         (parent (make-instance 'n:window :workspace ws))
         (other (make-instance 'n:window :workspace ws))
         (dialog (make-instance 'n:window :workspace ws)))
    (setf (n:window-parent dialog) parent
          (n:workspace-windows ws) (list dialog other parent)
          (n::workspace-focused ws) dialog)
    (n::remove-window ws dialog)
    (is (eq parent (n::workspace-focused ws)))))

(test pending-dimensions-only-resize-floating-windows
  (let* ((wm (make-test-wm))
         (win (make-instance 'n:window :workspace (ws wm 1))))
    (setf (n:workspace-windows (ws wm 1)) (list win)
          (n::window-pending-dimensions win) '(400 300))
    (finishes (n::manage-pending-dimensions wm))
    (is (null (n::window-pending-dimensions win)))))

(test overlap-with-outputs
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000)))
    (is (eq left (n::output-under wm 100 100 400 300)))
    (is (eq right (n::output-under wm 900 100 400 300)))
    (is (eq left (n::output-under wm 700 100 400 300)))
    (is (null (n::output-under wm 5000 100 400 300)))
    (is (null (n::output-under (make-test-wm) 0 0 10 10)))))

(test dragged-windows-move-to-the-output-they-are-on
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (win (open-window-on (n:output-workspace left))))
    (setf (n:window-floating win) t
          (n::window-x win) 1200
          (n::window-y win) 100
          (n::window-width win) 400
          (n::window-height win) 300)
    (n::settle-floating wm win)
    (is (eq (n:output-workspace right) (n:window-workspace win)))
    (is (equal '(200 100) (n::window-float-offset win)))
    (is (eq right (n:wm-focused-output wm)))))
