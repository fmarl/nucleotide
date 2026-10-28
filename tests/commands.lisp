;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite commands :in :nucleotide)
(in-suite commands)

(test commands-get-the-wm-and-their-arguments
  (let ((n::*commands* '())
        (wm (make-instance 'n::wm)))
    (unwind-protect
         (progn
           (n::define-command test-command (wm a &optional (b 2))
             (list wm a b))
           (is (equal (list wm 1 2) (n::run-command wm '(test-command 1))))
           (is (equal (list wm 1 3) (n::run-command wm '(test-command 1 3))))
           (is (equal '(test-command) (n::command-names)))
           (signals n::nucleotide-error (n::run-command wm 'missing)))
      (fmakunbound 'test-command))))

(test every-command-but-toggle-highlight-ends-the-highlight
  (let ((n::*commands* (list 'n::toggle-highlight))
        (ended '()))
    (unwind-protect
         (progn
           (n::define-command plain-test-command (wm)
             (declare (ignore wm)))
           (with-stubs ((n::end-highlight (lambda (wm) (push wm ended)))
                        (n::toggle-highlight (lambda (wm &rest match)
                                               (declare (ignore wm match)))))
             (n::run-command :wm 'plain-test-command)
             (n::run-command :wm 'n::toggle-highlight))
           (is (equal '(:wm) ended)))
      (fmakunbound 'plain-test-command))))

(test (bindings-check-the-arguments-of-their-command :fixture fresh-config)
  (dolist (action '((n::switch-workspace 0) (n::switch-workspace "1") (n::switch-workspace)
                    (n::send-to-workspace 1 2) (n::focus :leftt) (n::focus :left :right)
                    (n::focus-output :up) (n::consume-or-expel :up) (n::set-layout foo)
                    (n::set-layout (n::tiling :ratio 9)) (n::spawn) (n::spawn 1 2)
                    (n::set-column-width "-10") (n::toggle-highlight :appid "emacs")
                    (n::close-window 1)))
    (signals n::nucleotide-error (n::set-binding "Super+X" action)
             "~S was bound" action))
  (is (search "between 0 and 1"
              (handler-case (n::set-binding "Super+X" '(n::set-layout (n::tiling :ratio 9)))
                (n::nucleotide-error (c) (princ-to-string c)))))
  (dolist (action '((n::switch-workspace 9) (n::focus) (n::focus :up) (n::focus-output :prev)
                    (n::set-layout n::scrolling) (n::set-layout (n::tiling :ratio 1/2))
                    (n::spawn "foot") (n::spawn "foot" "-e" "htop") (n::set-window-height 1/2)
                    (n::toggle-highlight :app-id "emacs") (n::toggle-highlight) n::close-window))
    (is (equal "Super+X" (n::set-binding "Super+X" action)) "~S was refused" action))
  (finishes (n::use-defaults)))

(defun compile-warnings (form)
  (let ((warnings '()))
    (handler-bind ((warning (lambda (c)
                              (push c warnings)
                              (muffle-warning c))))
      (funcall (compile nil `(lambda () ,form))))
    warnings))

(test commands-take-any-ordinary-lambda-list
  (let ((n::*commands* '()))
    (unwind-protect
         (progn
           (is (null (compile-warnings
                      '(n::define-command counted-test-command
                        (wm &optional (count (error "too early") count-p))
                        (declare (ignore wm count-p) (type (integer 1) count))
                        count))))
           (is (null (compile-warnings
                      '(n::define-command named-test-command
                        (wm &key ((:name label) "x" label-p))
                        (declare (ignore wm label-p) (type string label))
                        label))))
           (finishes (n::normalize-action '(counted-test-command)))
           (signals n::nucleotide-error (n::normalize-action '(counted-test-command 0)))
           (finishes (n::normalize-action '(named-test-command :name "y")))
           (signals n::nucleotide-error (n::normalize-action '(named-test-command :name 3)))
           (signals n::nucleotide-error (n::normalize-action '(named-test-command :size 3))))
      (fmakunbound 'counted-test-command)
      (fmakunbound 'named-test-command))))
