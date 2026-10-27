;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite highlight :in :nucleotide)
(in-suite highlight)

(test highlight-prefers-the-last-focused-match
  (let* ((wm (make-test-wm))
         (old (open-window-on (ws wm 1)))
         (new (open-window-on (ws wm 2))))
    (setf (n::%window-app-id old) "emacs"
          (n::%window-app-id new) "emacs"
          (n::wm-focus-history wm) (list new old))
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::toggle-highlight wm :app-id "emacs")
      (is (eq new (n::wm-highlight wm)))
      (n::toggle-highlight wm :app-id "emacs")
      (is (null (n::wm-highlight wm))))))

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
    (setf (n::wm-highlight wm) highlighted)
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::send-focused-to wm (ws wm 3)))
    (is (eq (ws wm 3) (n::window-workspace focused)))
    (is (eq (ws wm 2) (n::window-workspace highlighted)))))
