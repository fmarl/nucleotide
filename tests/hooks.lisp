;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite hooks :in :nucleotide)
(in-suite hooks)

(test (hooks :fixture fresh-config)
  (let ((seen '()))
    (n::on :window-title (lambda (win title) (declare (ignore win title)) (error "boom")))
    (n::on :window-title (lambda (win title) (push (list win title) seen)))
    (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
      (n::run-event-hooks :window-title :win "title"))
    (is (equal '((:win "title")) seen))
    (signals n::nucleotide-error (n::on :no-such-event #'identity))))

(test (focus-changes-run-hooks-and-fill-the-history :fixture fresh-config)
  (let* ((wm (make-test-wm))
         (a (open-window-on (ws wm 1)))
         (b (open-window-on (ws wm 1)))
         (seen '()))
    (n::on :focus-changed (lambda (win) (push win seen)))
    (n::note-focus wm a)
    (n::note-focus wm a)
    (n::note-focus wm b)
    (n::note-focus wm a)
    (is (equal (list a b a) seen))
    (is (equal (list a b) (n::wm-focus-history wm)))))

(test (titles-reach-hooks-once-the-window-opened :fixture fresh-config)
  (let ((win (make-instance 'n::window))
        (seen '()))
    (n::on :window-title (lambda (win title) (declare (ignore win)) (push title seen)))
    (n::handle-event nil win :title "first")
    (setf (n::%window-rules-applied-p win) t)
    (n::handle-event nil win :title "second")
    (is (equal "second" (n::window-title win)))
    (is (equal '("second") seen))))
