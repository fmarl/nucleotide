;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite keys :in :nucleotide)
(in-suite keys)

(test modifiers
  (is (= 0 (n::parse-modifiers '())))
  (is (= 64 (n::parse-modifiers "Super")))
  (is (= 64 (n::parse-modifiers "Mod")))
  (is (= 65 (n::parse-modifiers "super+Shift")))
  (signals n::nucleotide-error (n::parse-modifiers "Hyper")))

(test keys
  (is (equal '(#x63 . 65) (n::parse-key "Super+Shift+C")))
  (is (equal '(#x31 . 64) (n::parse-key "Mod+1")))
  (is (equal '(#xff0d . 0) (n::parse-key "Return")))
  (is (equal '(#x2c . 64) (n::parse-key "Super+Comma")))
  (is (equal '(#x5b . 64) (n::parse-key "Super+BracketLeft")))
  (is (equal '(#x1008ff14 . 0) (n::parse-key "XF86AudioPlay")))
  (is (equal '(#x10020ac . 0) (n::parse-key (string (code-char #x20ac)))))
  (is (equal '(#xe0 . 0) (n::parse-key "AGRAVE")))
  (is (equal '(#xc0 . 0) (n::parse-key "Agrave")))
  (is (equal '(#xfe80 . 0) (n::parse-key "DEAD_A")))
  (is (equal '(#xff . 0) (n::parse-key "YDIAERESIS")))
  (is (equal '(#xfea0 . 0) (n::parse-key "cH")))
  (is (equal '(#x7e1 . 0) (n::parse-key "greek_ALPHA")))
  (is (equal '(#x1000561 . 0) (n::parse-key "armenian_AYB")))
  (signals n::nucleotide-error (n::parse-key "NoSymbol"))
  (signals n::nucleotide-error (n::parse-key "Super+NoSuchKey"))
  (signals n::nucleotide-error (n::parse-key "")))

(test key-sequences
  (is (equal '((#x20 . 64) (#x74 . 0)) (n::parse-key-sequence "Super+Space T")))
  (signals n::nucleotide-error (n::parse-key-sequence "A B C")))

(test (bindings-take-commands :fixture fresh-config)
  (n::bind "Super+Q" n::close-window)
  (n::bind "Super+H" (n::focus :left))
  (is (equal '(n::close-window) (action "Super+Q")))
  (is (equal '(n::focus :left) (action "Super+H")))
  (signals n::nucleotide-error (n::bind "Super+X" (n::no-such-command))))

(test (bindings-replace-the-same-keys :fixture fresh-config)
  (n::bind "Super+Q" n::close-window)
  (n::bind "Super+Q" n::exit-session)
  (is (= 1 (length n::*bindings*)))
  (is (equal '(n::exit-session) (action "Super+Q"))))

(test (chords-and-their-prefix-exclude-each-other :fixture fresh-config)
  (n::bind "Super+Space" n::toggle-floating)
  (n::bind "Super+Space T" (n::set-layout n::tiling))
  (n::bind "Super+Space M" (n::set-layout n::monocle))
  (is (= 2 (length n::*bindings*)))
  (n::bind "Super+Space" n::toggle-floating)
  (is (equal '(n::toggle-floating) (action "Super+Space")))
  (is (= 1 (length n::*bindings*))))

(test (unbind :fixture fresh-config)
  (n::bind "Super+Q" n::close-window)
  (n::unbind "Super+Q")
  (is (null n::*bindings*)))

(test (the-defaults-bind-known-commands :fixture fresh-config)
  (n::use-defaults)
  (is (equal '(n::switch-workspace 3) (action "Super+3")))
  (is (equal '(n::switch-workspace 3) (action "Super+Shift+3")))
  (is (equal '(n::send-to-workspace 3) (action "Super+Ctrl+Shift+3")))
  (is (equal '(n::toggle-highlight :app-id "emacs") (action "Super+Space E"))))
