;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite manage :in :nucleotide)
(in-suite manage)

(test fullscreen-goes-to-the-window-s-own-output
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (win (open-window-on (second (n::output-workspaces right)))))
    (setf (n::output-proxy left) :left
          (n::output-proxy right) :right)
    (is (eq :right (n::fullscreen-output win nil)))
    (is (eq :requested (n::fullscreen-output win :requested)))))

(test floating-windows-stay-out-of-the-layout
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (tiled (open-window-on ws))
         (dialog (open-window-on ws)))
    (setf (n::%window-floating-p dialog) t)
    (is (equal (list tiled) (n::layout-windows ws)))))

(test pending-sizes-only-resize-floating-windows
  (let* ((wm (make-test-wm))
         (tiled (open-window-on (ws wm 1)))
         (floating (open-window-on (ws wm 1)))
         (maximized (open-window-on (ws wm 1)))
         (proposed '()))
    (setf (n::%window-floating-p floating) t
          (n::%window-floating-p maximized) t
          (n::%window-maximized-p maximized) t
          (n::%window-pending-size tiled) (n::size 400 300)
          (n::%window-pending-size floating) (n::size 500 200)
          (n::%window-pending-size maximized) (n::size 0 0))
    (with-stubs ((n::propose-size
                  (lambda (win size) (push (list win size) proposed))))
      (n::manage-pending-sizes wm))
    (is (equalp (list (list floating (n::size 500 200))) proposed))
    (is (null (n::window-pending-size tiled)))
    (is (null (n::window-pending-size maximized)))))

(test closed-windows-drop-out-of-every-reference
  (let* ((wm (make-test-wm))
         (closed (open-window-on (ws wm 1)))
         (child (open-window-on (ws wm 1))))
    (setf (n::%window-node closed) :node
          (n::%window-proxy closed) :proxy
          (n::%window-parent child) closed
          (n::wm-highlight wm) closed
          (n::wm-pointer-window wm) closed
          (n::wm-last-focus wm) closed
          (n::wm-focus-history wm) (list closed child))
    (n::request wm :close closed)
    (n::request wm :fullscreen closed nil)
    (n::request wm :exit-fullscreen child)
    (with-stubs ((n::river-node-v1.destroy (lambda (node) (declare (ignore node))))
                 (n::river-window-v1.destroy (lambda (proxy) (declare (ignore proxy)))))
      (n::detach-window wm closed))
    (is (null (n::window-parent child)))
    (is (null (n::wm-highlight wm)))
    (is (null (n::wm-pointer-window wm)))
    (is (null (n::wm-last-focus wm)))
    (is (equal (list child) (n::wm-focus-history wm)))
    (is (equal (list (list :exit-fullscreen child))
               (n::requests wm '(:close :fullscreen :exit-fullscreen))))
    (is (equal (list child) (n::workspace-windows (ws wm 1))))))

(test requests-run-oldest-first-and-toggling-cancels-them
  (let* ((wm (make-test-wm))
         (win (open-window-on (ws wm 1)))
         (calls '()))
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm))))
                 (n::enter-fullscreen (lambda (win output) (push (list :enter win output) calls)))
                 (n::exit-fullscreen (lambda (win) (push (list :exit win) calls))))
      (n::toggle-fullscreen wm)
      (n::toggle-fullscreen wm)
      (is (null (n::requests wm '(:fullscreen :exit-fullscreen))))
      (setf (n::%window-fullscreen win) :output)
      (n::request wm :exit-fullscreen win)
      (n::request wm :fullscreen win :other)
      (n::manage-fullscreens wm))
    (is (equal (list (list :exit win) (list :enter win :other)) (reverse calls)))
    (is (null (n::wm-requests wm)))))

(test dialogs-open-on-the-output-that-shows-their-parent
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (parent (open-window-on (n::output-workspace left)))
         (dialog (open-window-on (n::output-workspace left))))
    (setf (n::%window-parent dialog) parent
          (n::wm-pointer-position wm) (n::point 1500 400))
    (n::place-new-window wm dialog)
    (is (eq (n::output-workspace left) (n::window-workspace dialog)))
    (is (eq left (n::wm-focused-output wm)))
    (is (null (n::workspace-windows (n::output-workspace right))))))

(test dialogs-of-hidden-windows-open-under-the-pointer
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (parent (open-window-on (second (n::output-workspaces left))))
         (dialog (open-window-on (n::output-workspace left))))
    (setf (n::%window-parent dialog) parent
          (n::wm-pointer-position wm) (n::point 1500 400))
    (n::place-new-window wm dialog)
    (is (eq (n::output-workspace right) (n::window-workspace dialog)))
    (is (eq right (n::wm-focused-output wm)))))

(test workspace-rules-count-on-the-output-under-the-pointer-and-keep-the-focus
  (with-rules
    (let* ((wm (make-test-wm))
           (left (add-output wm 0))
           (right (add-output wm 1000))
           (focused (open-window-on (n::output-workspace right)))
           (win (titled-win "mpv" "video")))
      (n::add-window (n::output-workspace left) win :focus nil)
      (n::rule (:app-id "mpv") :workspace 3)
      (setf (n::wm-pointer-position wm) (n::point 1500 400))
      (n::place-new-window wm win)
      (is (eq (nth 2 (n::output-workspaces right)) (n::window-workspace win)))
      (is (eq left (n::wm-focused-output wm)))
      (is (eq focused (n::workspace-focus (n::output-workspace right)))))))

(test fullscreen-windows-keep-their-place-in-the-layout
  (let* ((wm (make-test-wm))
         (output (add-output wm 0))
         (ws (n::output-workspace output))
         (a (open-window-on ws))
         (b (open-window-on ws)))
    (setf (n::workspace-layout ws) (make-instance 'n::scrolling))
    (with-stubs ((n::propose-size (lambda (win size) (declare (ignore win size)))))
      (n::manage-layout ws output)
      (setf (n::%window-fullscreen a) :output)
      (n::manage-layout ws output))
    (is (= 504 (n::window-x b)))))
