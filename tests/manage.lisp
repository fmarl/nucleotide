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
    (is (eq :right (n::fullscreen-output wm win nil)))
    (is (eq :requested (n::fullscreen-output wm win :requested)))))

(test floating-windows-stay-out-of-the-layout
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (tiled (open-window-on ws))
         (dialog (open-window-on ws)))
    (setf (n::%window-floating-p dialog) t)
    (is (equal (list tiled) (n::tiled-windows ws)))))

(test pending-dimensions-only-resize-floating-windows
  (let* ((wm (make-test-wm))
         (tiled (open-window-on (ws wm 1)))
         (floating (open-window-on (ws wm 1)))
         (proposed '()))
    (setf (n::%window-floating-p floating) t
          (n::%window-pending-dimensions tiled) '(400 300)
          (n::%window-pending-dimensions floating) '(500 200))
    (with-stubs ((n::propose-dimensions
                  (lambda (win width height) (push (list win width height) proposed))))
      (n::manage-pending-dimensions wm))
    (is (equal (list (list floating 500 200)) proposed))
    (is (null (n::window-pending-dimensions tiled)))))

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
