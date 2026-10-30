;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite eventloop :in :nucleotide)
(in-suite eventloop)

(test timers-run-in-the-order-they-are-due
  (let ((loop (n::%make-event-loop))
        (ran '()))
    (n::event-loop-schedule loop 0 (lambda () (push :second ran)))
    (n::event-loop-schedule loop -1 (lambda () (push :first ran)))
    (n::event-loop-schedule loop 3600 (lambda () (push :later ran)))
    (is (zerop (n::next-timeout loop)))
    (n::run-due-timers loop)
    (is (equal '(:second :first) ran))
    (is (< 3599 (n::next-timeout loop) 3601))))

(test cancelled-timers-do-not-run
  (let* ((loop (n::%make-event-loop))
         (ran nil)
         (timer (n::event-loop-schedule loop 0 (lambda () (setf ran t)))))
    (n::event-loop-cancel loop timer)
    (n::run-due-timers loop)
    (is (null ran))
    (is (null (n::next-timeout loop)))))

(test a-failing-timer-does-not-stop-the-others
  (let ((loop (n::%make-event-loop))
        (ran nil))
    (n::event-loop-schedule loop 0 (lambda () (error "failed")))
    (n::event-loop-schedule loop 0 (lambda () (setf ran t)))
    (n::run-due-timers loop)
    (is (eq t ran))))

(test only-the-thread-of-the-loop-schedules-timers
  (let ((loop (n::%make-event-loop :thread sb-thread:*current-thread*)))
    (is (typep (sb-thread:join-thread
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case (n::event-loop-schedule loop 0 (lambda ()))
                     (n::nucleotide-error (c) c)))))
               'n::nucleotide-error))
    (is (null (n::loop-timers loop)))))
