;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite highlight :in :nucleotide)
(in-suite highlight)

(test highlight-prefers-the-last-focused-match
  (let* ((wm (make-test-wm))
         (old (open-window-on (ws wm 1)))
         (new (open-window-on (ws wm 2))))
    (setf (n::window-app-id old) "emacs"
          (n::window-app-id new) "emacs"
          (n::wm-focus-history wm) (list new old))
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::toggle-highlight wm :app-id "emacs")
      (is (eq new (n::wm-highlight wm)))
      (n::toggle-highlight wm :app-id "emacs")
      (is (null (n::wm-highlight wm))))))
