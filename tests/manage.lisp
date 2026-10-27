;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite manage :in :nucleotide)
(in-suite manage)

(test fullscreen-goes-to-the-window-s-own-output
  (let* ((wm (make-test-wm))
         (left (add-output wm 0))
         (right (add-output wm 1000))
         (win (make-instance 'n::window :workspace (second (n::output-workspaces right)))))
    (setf (n::output-proxy left) :left
          (n::output-proxy right) :right)
    (is (eq :right (n::fullscreen-output wm win nil)))
    (is (eq :requested (n::fullscreen-output wm win :requested)))))

(test empty-workspaces-clear-the-focus
  (let* ((wm (make-test-wm))
         (win (make-instance 'n::window :workspace (ws wm 1) :proxy :window))
         (calls '()))
    (push win (n::workspace-windows (ws wm 1)))
    (setf (n::workspace-focus (ws wm 1)) win
          (n::wm-seat wm) :seat)
    (with-stubs ((n::river-seat-v1.focus-window
                  (lambda (seat window) (declare (ignore seat)) (push (list :focus window) calls)))
                 (n::river-seat-v1.clear-focus
                  (lambda (seat) (declare (ignore seat)) (push (list :clear) calls))))
      (n::manage-focus wm)
      (n::show-workspace wm (ws wm 2))
      (n::manage-focus wm))
    (is (equal '((:focus :window) (:clear)) (reverse calls)))))

(test floating-windows-stay-out-of-the-layout
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (tiled (make-instance 'n::window :workspace ws))
         (dialog (make-instance 'n::window :workspace ws)))
    (setf (n::workspace-windows ws) (list dialog tiled)
          (n::window-floating dialog) t)
    (is (equal (list tiled) (n::tiled-windows ws)))))

(test pending-dimensions-only-resize-floating-windows
  (let* ((wm (make-test-wm))
         (tiled (make-instance 'n::window :workspace (ws wm 1)))
         (floating (make-instance 'n::window :workspace (ws wm 1)))
         (proposed '()))
    (setf (n::workspace-windows (ws wm 1)) (list tiled floating)
          (n::window-floating floating) t
          (n::window-pending-dimensions tiled) '(400 300)
          (n::window-pending-dimensions floating) '(500 200))
    (with-stubs ((n::propose-dimensions
                  (lambda (win width height) (push (list win width height) proposed))))
      (n::manage-pending-dimensions wm))
    (is (equal (list (list floating 500 200)) proposed))
    (is (null (n::window-pending-dimensions tiled)))))

(test closed-windows-drop-out-of-every-reference
  (let* ((wm (make-test-wm))
         (closed (open-window-on (ws wm 1)))
         (child (open-window-on (ws wm 1))))
    (setf (n::window-node closed) :node
          (n::window-proxy closed) :proxy
          (n::window-parent child) closed
          (n::wm-highlight wm) closed
          (n::wm-pointer-window wm) closed
          (n::wm-last-focus wm) closed
          (n::wm-focus-history wm) (list closed child)
          (n::wm-pending-closes wm) (list closed)
          (n::wm-pending-fullscreens wm) (list (cons closed nil) (cons child :exit)))
    (with-stubs ((n::river-node-v1.destroy (lambda (node) (declare (ignore node))))
                 (n::river-window-v1.destroy (lambda (proxy) (declare (ignore proxy)))))
      (n::detach-window wm closed))
    (is (null (n::window-parent child)))
    (is (null (n::wm-highlight wm)))
    (is (null (n::wm-pointer-window wm)))
    (is (null (n::wm-last-focus wm)))
    (is (equal (list child) (n::wm-focus-history wm)))
    (is (null (n::wm-pending-closes wm)))
    (is (equal (list (cons child :exit)) (n::wm-pending-fullscreens wm)))
    (is (equal (list child) (n::workspace-windows (ws wm 1))))))

(test the-focus-is-the-highlighted-window-while-there-is-one
  (let* ((wm (make-test-wm))
         (focused (open-window-on (ws wm 1)))
         (highlighted (open-window-on (ws wm 2))))
    (is (eq focused (n::wm-focus wm)))
    (setf (n::wm-highlight wm) highlighted)
    (is (eq highlighted (n::wm-focus wm)))
    (is (eq focused (n::workspace-focus (ws wm 1))))))

(test workspace-commands-act-on-the-workspace-focus-during-a-highlight
  (let* ((wm (make-test-wm))
         (focused (open-window-on (ws wm 1)))
         (highlighted (open-window-on (ws wm 2))))
    (add-output wm 0)
    (setf (n::wm-highlight wm) highlighted)
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::send-focused-to wm (ws wm 3)))
    (is (eq (ws wm 3) (n::window-workspace focused)))
    (is (eq (ws wm 2) (n::window-workspace highlighted)))))
