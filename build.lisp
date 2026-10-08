;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(require :asdf)

(asdf:load-asd
 (merge-pathnames
  "nucleotide.asd"
  (uiop:pathname-directory-pathname *load-truename*)))
(asdf:load-system "nucleotide")

(if (uiop:getenvp "NUCLEOTIDE_NO_REPL")
    (progn
      (setf nucleotide::*repl-allowed* nil)
      (format t "~&nucleotide build: built without REPL support~%"))
    (when (asdf:find-system "slynk" nil)
      (asdf:load-system "slynk")
      (when (asdf:find-system "slynk/mrepl" nil)
        (asdf:load-system "slynk/mrepl"))
      (format t "~&nucleotide build: slynk baked in, toggle the REPL with Super+Space r~%")))

(sb-ext:save-lisp-and-die
 "nucleotide"
 :executable t
 :compression t
 :toplevel (lambda ()
             (sb-ext:disable-debugger)
             (nucleotide::confine-thread-errors)
             (nucleotide:start-wm)
             (nucleotide::start-repl-from-env)
             (sb-thread:join-thread (nucleotide::wm-thread nucleotide:*wm*))))
