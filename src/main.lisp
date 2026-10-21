;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun run ()
  (handler-case (start-wm)
    (error (c)
      (format *error-output* "~&nucleotide: ~A~%" c)
      (return-from run 1)))
  (sb-thread:join-thread (wm-thread *wm*) :default nil)
  0)

(defun main ()
  (sb-ext:disable-debugger)
  (confine-thread-errors)
  (let ((code (run)))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code code :abort t)))

(setf *builtin-commands* (copy-list *commands*))
