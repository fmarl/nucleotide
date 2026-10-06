;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

;; Run: sbcl --load dev.lisp
;; Then connect from Emacs: M-x sly-connect RET localhost RET 4005
;; (SLY sends the secret from ~/.sly-secret, created on first start.)

(require :asdf)
(asdf:load-asd (merge-pathnames "nucleotide.asd" *load-truename*))
(asdf:load-system "nucleotide")

(handler-case (nucleotide:start-repl-server :dont-close t)
  (error (c) (format t "~&nucleotide: no REPL server: ~A~%" c)))

(nucleotide:start-wm)
(format t "~&nucleotide is managing windows; hack away via SLY on port 4005~%")
