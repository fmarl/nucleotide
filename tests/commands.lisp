;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite commands :in :nucleotide)
(in-suite commands)

(test commands-get-the-wm-and-their-arguments
  (let ((n::*commands* '())
        (wm (make-instance 'n::wm)))
    (n::define-command test-command (wm a &optional (b 2))
      (list wm a b))
    (is (equal (list wm 1 2) (n::run-command wm '(test-command 1))))
    (is (equal (list wm 1 3) (n::run-command wm '(test-command 1 3))))
    (is (equal '(test-command) (n::command-names)))
    (signals n::nucleotide-error (n::run-command wm 'missing))))

(test every-command-but-toggle-highlight-ends-the-highlight
  (let ((n::*commands* (list 'n::toggle-highlight))
        (ended '()))
    (n::define-command plain-test-command (wm)
      (declare (ignore wm)))
    (with-stubs ((n::end-highlight (lambda (wm) (push wm ended)))
                 (n::toggle-highlight (lambda (wm &rest match) (declare (ignore wm match)))))
      (n::run-command :wm 'plain-test-command)
      (n::run-command :wm 'n::toggle-highlight))
    (is (equal '(:wm) ended))))
