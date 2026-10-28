;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(use-modules (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz)
             (gnu packages xdisorg)
             (guix build-system asdf)
             (guix gexp)
             (guix git-download)
             (guix packages)
             ((guix licenses) #:prefix license:))

(define %source-dir (dirname (current-filename)))

(package
  (name "nucleotide")
  (version "0.4.0")
  (source (local-file %source-dir "nucleotide-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system asdf-build-system/sbcl)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'create-asdf-configuration 'build-program
          (lambda* (#:key outputs #:allow-other-keys)
            (build-program (string-append #$output "/bin/nucleotide")
                           outputs
                           #:dependencies '("nucleotide" "slynk" "slynk/mrepl")
                           #:dependency-prefixes (list #$output
                                                       #$(this-package-input
                                                          "sbcl-slynk"))
                           #:compress? #t
                           #:entry-program '((nucleotide::main))))))))
  (native-inputs (list libxkbcommon sbcl sbcl-fiveam))
  (inputs (list sbcl-slynk))
  (synopsis "Hackable Wayland window manager")
  (description
   "Nucleotide is a hackable Wayland window manager for the river compositor,
written in Common Lisp.")
  (home-page "https://codeberg.org/fmarl/nucleotide")
  (license license:gpl3+))
