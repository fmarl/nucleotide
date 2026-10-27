;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(require :asdf)

(asdf:load-asd
 (merge-pathnames
  "nucleotide.asd"
  (uiop:pathname-directory-pathname *load-truename*)))
(asdf:load-system "nucleotide")

(when (asdf:find-system "slynk" nil)
  (asdf:load-system "slynk")
  (when (asdf:find-system "slynk/mrepl" nil)
    (asdf:load-system "slynk/mrepl"))
  (format t "~&nucleotide build: slynk baked in, toggle the REPL with Super+Space r~%"))

(sb-ext:save-lisp-and-die "nucleotide" :executable t :compression t :toplevel #'nucleotide::main)
