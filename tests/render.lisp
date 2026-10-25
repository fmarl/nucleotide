;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite render :in :nucleotide)
(in-suite render)

(test colors
  (is (equal '(111 140 171 255) (n::parse-color "#6f8cab")))
  (is (equal '(111 140 171 128) (n::parse-color "#6f8cab80")))
  (signals n::nucleotide-error (n::parse-color "6f8cab"))
  (signals error (n::parse-color "#zzzzzz")))

(test an-empty-clip-box-disables-clipping
  (is (equal '(0 0 0 0) (n::clip-request (make-instance 'n::window :workspace nil)))))
