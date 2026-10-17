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

(let ((library (loop for dir in (uiop:split-string (or (uiop:getenv "LD_LIBRARY_PATH") "")
                                                   :separator ":")
                     thereis (and (plusp (length dir))
                                  (probe-file (format nil "~A/~A" dir
                                                      nucleotide::*libxkbcommon*))))))
  (if library
      (setf nucleotide::*libxkbcommon* (namestring library))
      (warn "~A not found in LD_LIBRARY_PATH, the executable looks for it at startup"
            nucleotide::*libxkbcommon*)))

(sb-ext:save-lisp-and-die "nucleotide" :executable t :compression t :toplevel #'nucleotide::main)
