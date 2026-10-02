;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite rules :in :nucleotide)
(in-suite rules)

(defmacro with-rules (&body body)
  `(let ((n::*window-rules* '()))
     ,@body))

(defun titled-win (app-id title)
  (let ((win (make-instance 'n:window :workspace nil)))
    (setf (n:window-app-id win) app-id
          (n:window-title win) title)
    win))

(test rules-match-app-id-and-title
  (with-rules
      (n:define-window-rule (:app-id "mpv") :float t)
    (n:define-window-rule (:title (n:contains "Picture")) :float t :workspace 3)
    (is (equal '(:float t) (n::window-actions (titled-win "mpv" "x"))))
    (is (equal '(:float t :workspace 3)
               (n::window-actions (titled-win "firefox" "Picture-in-Picture"))))
    (is (null (n::window-actions (titled-win "firefox" "Mozilla"))))))

(test later-rules-win
  (with-rules
      (n:define-window-rule (:app-id "mpv") :float t)
    (n:define-window-rule (:app-id "mpv" :title "big") :float nil)
    (is (null (getf (n::window-actions (titled-win "mpv" "big")) :float :unset)))))

(test redefining-a-rule-replaces-it
  (with-rules
      (n:define-window-rule (:app-id "mpv") :float t)
    (n:define-window-rule (:app-id "mpv") :workspace 2)
    (is (= 1 (length n::*window-rules*)))))

(test rules-move-and-float-windows
  (with-rules
      (let* ((wm (make-test-wm))
             (win (titled-win "mpv" "video")))
        (setf (n:window-workspace win) (ws wm 1)
              (n:workspace-windows (ws wm 1)) (list win))
        (n:define-window-rule (:app-id "mpv") :float t :workspace 3)
        (n::apply-window-rules wm win)
        (is (eq (ws wm 3) (n:window-workspace win)))
        (is (equal (list win) (n:workspace-windows (ws wm 3))))
        (is (null (n:workspace-windows (ws wm 1))))
        (is (eq t (n:window-floating win))))))
