;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Run: sbcl --load dev.lisp
;; Then connect from Emacs: M-x sly-connect RET localhost RET 4005

(require :asdf)
(asdf:load-asd (merge-pathnames "nucleotide.asd" *load-truename*))
(asdf:load-system "nucleotide")

(handler-case (nucleotide:start-repl-server :port 4005)
  (error (c) (format t "~&nucleotide: no REPL server: ~A~%" c)))

(nucleotide:start-wm)
(format t "~&nucleotide is managing windows; hack away via SLY on port 4005~%")
