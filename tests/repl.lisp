;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite repl :in :nucleotide)
(in-suite repl)

(test the-repl-secret-is-created-private-and-must-stay-so
  (with-private-directory (home)
    (with-environment (("HOME" (format nil "~A/" home)))
      (let ((path (namestring (n::repl-secret-file))))
        (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
          (n::ensure-repl-secret))
        (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat path)))))
        (is (= 64 (length (with-open-file (in path) (read-line in)))))
        (sb-posix:chmod path #o644)
        (signals n::nucleotide-error (n::ensure-repl-secret))))))
