;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite outputs :in :nucleotide)
(in-suite outputs)

(test the-first-output-takes-over-the-orphans
  (let* ((wm (make-test-wm))
         (orphan (ws wm 2))
         (win (open-window-on orphan))
         (output (add-output wm 0)))
    (is (null (n::wm-orphans wm)))
    (is (eq orphan (nth 1 (n::output-workspaces output))))
    (is (eq output (n::workspace-output (n::window-workspace win))))
    (is (eq (ws wm 1) (n::output-workspace output)))))

(test every-output-has-its-own-workspaces
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000)))
    (is (= 4 (length (n::output-workspaces right))))
    (is (null (intersection (n::output-workspaces left) (n::output-workspaces right))))
    (n::show-workspace wm (nth 2 (n::output-workspaces right)))
    (is (eq right (n::wm-focused-output wm)))
    (is (eq (nth 2 (n::output-workspaces right)) (ws wm 3)))
    (is (eq (first (n::output-workspaces left)) (n::output-workspace left)))))

(test unplugged-windows-move-to-the-same-workspace-elsewhere
  (let* ((wm (make-test-wm))
         (laptop (add-output wm 0 :name "eDP-1"))
         (monitor (add-output wm 1000 :name "HDMI-A-1"))
         (home (nth 2 (n::output-workspaces monitor)))
         (win (open-window-on home)))
    (n::show-workspace wm home)
    (n::unassign-output wm monitor)
    (is (eq (nth 2 (n::output-workspaces laptop)) (n::window-workspace win)))
    (is (eq home (n::window-home win)))
    (is (eq laptop (n::wm-focused-output wm)))
    (is (equal (list "HDMI-A-1") (mapcar #'car (n::wm-stash wm))))))

(test replugged-outputs-get-their-workspaces-and-windows-back
  (let* ((wm (make-test-wm))
         (laptop (add-output wm 0 :name "eDP-1"))
         (monitor (add-output wm 1000 :name "HDMI-A-1"))
         (home (nth 2 (n::output-workspaces monitor)))
         (returning (open-window-on home))
         (moved (open-window-on home)))
    (n::unassign-output wm monitor)
    (n::move-to-workspace moved (first (n::output-workspaces laptop)))
    (let ((again (add-output wm 1000 :name "HDMI-A-1")))
      (n::restore-output wm again)
      (is (eq home (nth 2 (n::output-workspaces again))))
      (is (eq home (n::window-workspace returning)))
      (is (null (n::window-home returning)))
      (is (eq (first (n::output-workspaces laptop)) (n::window-workspace moved)))
      (is (null (n::wm-stash wm))))))

(test windows-return-to-their-first-output-after-several-removals
  (let* ((wm (make-test-wm))
         (first-monitor (add-output wm 0 :name "DP-1"))
         (second-monitor (add-output wm 1000 :name "DP-2"))
         (laptop (add-output wm 2000 :name "eDP-1"))
         (home (nth 2 (n::output-workspaces first-monitor)))
         (win (open-window-on home)))
    (n::unassign-output wm first-monitor)
    (is (eq second-monitor (n::workspace-output (n::window-workspace win))))
    (n::unassign-output wm second-monitor)
    (is (eq laptop (n::workspace-output (n::window-workspace win))))
    (is (eq home (n::window-home win)))
    (let ((again (add-output wm 0 :name "DP-1")))
      (n::restore-output wm again)
      (is (eq home (n::window-workspace win))))))

(test removing-the-last-output-keeps-its-workspaces-for-the-next
  (let* ((wm (make-test-wm))
         (output (add-output wm 0))
         (win (open-window-on (ws wm 2))))
    (n::unassign-output wm output)
    (is (= 4 (length (n::wm-orphans wm))))
    (let ((next (add-output wm 0)))
      (is (eq next (n::workspace-output (n::window-workspace win)))))))

(test moving-a-workspace-exchanges-contents
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (here (n::output-workspace left))
         (there (n::output-workspace right))
         (win (open-window-on here))
         (layout (n::workspace-layout here)))
    (n::swap-workspace-contents here there)
    (is (eq there (n::window-workspace win)))
    (is (equal (list win) (n::workspace-windows there)))
    (is (null (n::workspace-windows here)))
    (is (eq win (n::workspace-focus there)))
    (is (eq layout (n::workspace-layout there)))))

(test neighbor-outputs-are-ordered-left-to-right
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000)))
    (is (eq right (n::neighbor-output wm :next)))
    (is (eq right (n::neighbor-output wm :prev)))
    (setf (n::wm-active-workspace wm) (n::output-workspace right))
    (is (eq left (n::neighbor-output wm :next)))))

(test clip-boxes
  (let ((output (make-instance 'n::output :proxy nil)))
    (setf (n::output-width output) 1000
          (n::output-height output) 800)
    (is (null (n::clip-box 8 8 496 784 output)))
    (is (eq :hidden (n::clip-box 1008 8 496 784 output)))
    (is (equal '(-504 0 1000 800) (n::clip-box 504 0 1000 800 output)))))

(test windows-moved-with-their-workspace-stay-where-they-were-moved
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (win (open-window-on (n::output-workspace left))))
    (setf (n::window-home win) (ws wm 3))
    (n::swap-workspace-contents (n::output-workspace left) (n::output-workspace right))
    (is (null (n::window-home win)))))
