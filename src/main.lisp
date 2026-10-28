;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun confine-thread-errors ()
  "With the debugger disabled, an error in any thread quits the process.
Skip the failing event instead, or else end only the failing thread."
  (let ((previous sb-ext:*invoke-debugger-hook*))
    (setf sb-ext:*invoke-debugger-hook*
          (lambda (condition hook)
            (let ((skip (find-restart 'skip condition))
                  (thread sb-thread:*current-thread*))
              (cond
                (skip
                 (log-message :error "event skipped: ~A" condition)
                 (invoke-restart skip))
                ((not (eq thread (sb-thread:main-thread)))
                 (log-message :error "thread ~S ended: ~A" (sb-thread:thread-name thread) condition)
                 (sb-thread:abort-thread))
                (previous
                 (funcall previous condition hook))))))))

(defun run ()
  (handler-case (start-wm)
    (error (c)
      (log-message :error "~A" c)
      (return-from run 1)))
  (sb-thread:join-thread (wm-thread *wm*) :default nil)
  (stop-ipc *wm*)
  (if (loop-failure (wm-loop *wm*)) 1 0))

(defun main ()
  (sb-ext:disable-debugger)
  (confine-thread-errors)
  (let ((code (run)))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code code :abort t)))

(setf *builtin-commands* (copy-list *commands*))
