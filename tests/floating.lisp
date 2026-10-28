;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite floating :in :nucleotide)
(in-suite floating)

(defun make-win (&key parent (min '(0 0)) (max '(0 0)))
  (let ((win (make-instance 'n::window)))
    (setf (n::%window-parent win) parent
          (n::%window-min-width win) (first min)
          (n::%window-min-height win) (second min)
          (n::%window-max-width win) (first max)
          (n::%window-max-height win) (second max))
    win))

(test dialogs-float
  (is (n::auto-float-p (make-win :parent (make-win))))
  (is (n::auto-float-p (make-win :min '(400 300) :max '(400 300))))
  (is (not (n::auto-float-p (make-win))))
  (is (not (n::auto-float-p (make-win :min '(400 300) :max '(800 600))))))

(test dialogs-start-centered-on-their-parent-if-it-is-visible
  (let* ((wm (make-test-wm))
         (ws (n::output-workspace (add-output wm 0)))
         (parent (open-window-on ws))
         (dialog (make-win :parent parent)))
    (n::add-window ws dialog :focus nil)
    (setf (n::%window-width dialog) 400
          (n::%window-height dialog) 300
          (n::%window-x parent) 100
          (n::%window-y parent) 100
          (n::%window-width parent) 600
          (n::%window-height parent) 400)
    (is (equalp (n::point 200 150) (n::initial-float-offset wm dialog)))
    (setf (n::%window-clip parent) :hidden)
    (is (equalp (n::point 300 250) (n::initial-float-offset wm dialog)))))

(test closing-a-dialog-focuses-its-parent
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (parent (open-window-on ws))
         (other (open-window-on ws))
         (dialog (open-window-on ws)))
    (declare (ignore other))
    (setf (n::%window-parent dialog) parent)
    (n::remove-window dialog)
    (is (eq parent (n::workspace-focus ws)))))

(test overlap-with-outputs
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000)))
    (is (eq left (n::output-under wm (n::rect 100 100 400 300))))
    (is (eq right (n::output-under wm (n::rect 900 100 400 300))))
    (is (eq left (n::output-under wm (n::rect 700 100 400 300))))
    (is (null (n::output-under wm (n::rect 5000 100 400 300))))
    (is (null (n::output-under (make-test-wm) (n::rect 0 0 10 10))))))

(test dragged-windows-move-to-the-output-they-are-on
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (win (open-window-on (n::output-workspace left))))
    (setf (n::%window-floating-p win) t
          (n::%window-x win) 1200
          (n::%window-y win) 100
          (n::%window-width win) 400
          (n::%window-height win) 300)
    (n::settle-floating wm win)
    (is (eq (n::output-workspace right) (n::window-workspace win)))
    (is (equalp (n::point 200 100) (n::window-float-offset win)))
    (is (eq right (n::wm-focused-output wm)))))

(defun maximized-and-back (win)
  (let ((wm (make-test-wm)))
    (n::maximize wm win)
    (setf (n::%window-fullscreen win) nil)
    (n::unmaximize wm win)
    (n::window-pending-size win)))

(test floating-windows-get-their-size-back-after-maximizing
  (let* ((wm (make-test-wm))
         (win (open-window-on (ws wm 1))))
    (setf (n::%window-floating-p win) t
          (n::%window-width win) 400
          (n::%window-height win) 300)
    (is (equalp (n::size 400 300) (maximized-and-back win)))
    (setf (n::%window-width win) 1000
          (n::%window-height win) 800
          (n::%window-fullscreen win) :output)
    (is (equalp (n::size 0 0) (maximized-and-back win)))))

(test a-window-without-a-size-has-none-to-restore
  (let* ((wm (make-test-wm))
         (win (open-window-on (ws wm 1))))
    (n::maximize wm win)
    (is (null (n::window-restore-size win)))))

(test a-held-window-gets-the-size-it-is-held-at
  (let* ((wm (make-test-wm))
         (win (open-window-on (ws wm 1))))
    (setf (n::%window-floating-p win) t
          (n::%window-maximized-p win) t
          (n::%window-width win) 1000
          (n::%window-height win) 800)
    (let ((rect (n::take-hold wm win)))
      (is (equalp (n::size 1000 800) (n::rect-size rect)))
      (is (equalp (n::size 1000 800) (n::window-pending-size win))))))

(test floating-a-maximized-window-restores-its-size
  (let* ((wm (make-test-wm))
         (win (open-window-on (ws wm 1))))
    (setf (n::%window-width win) 400
          (n::%window-height win) 300)
    (n::maximize wm win)
    (setf (n::%window-width win) 1000
          (n::%window-height win) 800)
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::toggle-floating wm))
    (is (n::window-floating-p win))
    (is (not (n::window-maximized-p win)))
    (is (equalp (n::size 400 300) (n::window-pending-size win)))))
