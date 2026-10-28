;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite rules :in :nucleotide)
(in-suite rules)

(defmacro with-rules (&body body)
  `(let ((n::*window-rules* '()))
     ,@body))

(defun titled-win (app-id title)
  (let ((win (make-instance 'n::window)))
    (setf (n::%window-app-id win) app-id
          (n::%window-title win) title)
    win))

(test rules-match-app-id-and-title
  (with-rules
    (n::rule (:app-id "mpv") :float t)
    (n::rule (:title (n::contains "Picture")) :float t :workspace 3)
    (is (equal '(:float t) (n::window-actions (titled-win "mpv" "x"))))
    (is (equal '(:float t :workspace 3)
               (n::window-actions (titled-win "firefox" "Picture-in-Picture"))))
    (is (null (n::window-actions (titled-win "firefox" "Mozilla"))))))

(test later-rules-win
  (with-rules
    (n::rule (:app-id "mpv") :float t)
    (n::rule (:app-id "mpv" :title "big") :float nil)
    (is (null (getf (n::window-actions (titled-win "mpv" "big")) :float :unset)))))

(test redefining-a-rule-replaces-it
  (with-rules
    (n::rule (:app-id "mpv") :float t)
    (n::rule (:app-id "mpv") :workspace 2)
    (is (= 1 (length n::*window-rules*)))))

(test rules-move-and-float-windows
  (with-rules
    (let* ((wm (make-test-wm))
           (win (titled-win "mpv" "video")))
      (n::add-window (ws wm 1) win)
      (n::rule (:app-id "mpv") :float t :workspace 3)
      (n::place-new-window wm win)
      (n::apply-window-rules wm win)
      (is (eq (ws wm 3) (n::window-workspace win)))
      (is (equal (list win) (n::workspace-windows (ws wm 3))))
      (is (null (n::workspace-windows (ws wm 1))))
      (is (eq t (n::window-floating-p win))))))

(test rules-are-checked-when-they-are-made
  (with-rules
    (dolist (rule '(((:class "mpv") (:float t))
                    ((:app-id 5) (:float t))
                    ((:app-id) (:float t))
                    ((:app-id "mpv") (:floating t))
                    ((:app-id "mpv") (:workspace 0))
                    ((:app-id "mpv") (:output 1))
                    ((:app-id "mpv") (:float))))
      (signals n::nucleotide-error (n::add-window-rule (first rule) (second rule))
               "~S was accepted" rule))
    (finishes (n::rule (:title (n::contains "Picture") :app-id 'string-upcase)
                       :float nil :workspace 2 :output "DP-1" :fullscreen t))))

(test rules-count-workspaces-on-the-output-they-name
  (let* ((wm (make-test-wm))
         (a (add-output wm 0 :name "A"))
         (b (add-output wm 1000 :name "B")))
    (flet ((target (&rest actions)
             (n::rule-workspace wm actions a)))
      (is (eq (second (n::output-workspaces b)) (target :workspace 2 :output "B")))
      (is (eq (n::output-workspace b) (target :output "B")))
      (is (eq (second (n::output-workspaces a)) (target :workspace 2 :output "C")))
      (is (eq (second (n::output-workspaces a)) (target :workspace 2)))
      (is (null (target :output "C")))
      (is (null (target :float t))))))
