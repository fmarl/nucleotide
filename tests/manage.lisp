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
    (setf (n::workspace-focused (ws wm 1)) win
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
