;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite eventloop :in :nucleotide)
(in-suite eventloop)

(test timers-run-in-the-order-they-are-due
  (let ((loop (w::%make-event-loop))
        (ran '()))
    (w::event-loop-schedule loop 0 (lambda () (push :second ran)))
    (w::event-loop-schedule loop -1 (lambda () (push :first ran)))
    (w::event-loop-schedule loop 3600 (lambda () (push :later ran)))
    (is (zerop (w::next-timeout loop)))
    (w::run-due-timers loop)
    (is (equal '(:second :first) ran))
    (is (< 3599 (w::next-timeout loop) 3601))))

(test cancelled-timers-do-not-run
  (let* ((loop (w::%make-event-loop))
         (ran nil)
         (timer (w::event-loop-schedule loop 0 (lambda () (setf ran t)))))
    (w::event-loop-cancel loop timer)
    (w::run-due-timers loop)
    (is (null ran))
    (is (null (w::next-timeout loop)))))

(test a-failing-timer-does-not-stop-the-others
  (let ((loop (w::%make-event-loop))
        (ran nil))
    (w::event-loop-schedule loop 0 (lambda () (error "failed")))
    (w::event-loop-schedule loop 0 (lambda () (setf ran t)))
    (w::run-due-timers loop)
    (is (eq t ran))))

(test only-the-thread-of-the-loop-schedules-timers
  (let ((loop (w::%make-event-loop :thread sb-thread:*current-thread*)))
    (is (typep (sb-thread:join-thread
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case (w::event-loop-schedule loop 0 (lambda ()))
                     (w::nucleotide-error (c) c)))))
               'w::nucleotide-error))
    (is (null (w::loop-timers loop)))))

(test the-debugger-hook-of-the-binary-skips-failing-events
  (is (eq :done
          (sb-thread:join-thread
           (sb-thread:make-thread
            (lambda ()
              (let ((sb-ext:*invoke-debugger-hook* nil)
                    (w:*debug-on-error* t))
                (n::confine-thread-errors)
                (w:call-reporting-errors (lambda () (error "failed")))
                :done)))
           :default :aborted))))
