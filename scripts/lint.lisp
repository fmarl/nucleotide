;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

;; Compile nucleotide and its tests from scratch; fail on any warning,
;; style warnings included.

(require :asdf)

(asdf:load-asd (merge-pathnames "../nucleotide.asd" *load-truename*))

(defvar *warnings* 0)

(handler-bind ((warning
                (lambda (c)
                  ;; ASDF summarizes warnings we already counted, and compiling
                  ;; then loading in one image redefines every macro.
                  (unless (or (typep c 'uiop:compile-warned-warning)
                              (typep c 'uiop:compile-failed-warning)
                              (typep c 'sb-kernel:redefinition-warning))
                    (incf *warnings*)
                    (format *error-output* "~&~A: ~A~%" (type-of c) c))
                  (muffle-warning c))))
  (asdf:load-system "nucleotide/tests"
                    :force '("nucleotide" "nucleotide/tests")))

(format t "~&lint: ~D warning~:P~%" *warnings*)
(uiop:quit (if (zerop *warnings*) 0 1))
