# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

SBCL := sbcl --non-interactive
EMACS := emacs --batch -Q
LISP_FILES := $(wildcard *.lisp *.asd src/*.lisp src/core/*.lisp tests/*.lisp scripts/*.lisp)

.PHONY: all check lint fmt keysyms clean

all:
	$(SBCL) --load build.lisp

check:
	$(SBCL) --eval '(require :asdf)' \
	        --eval '(asdf:load-asd (truename "nucleotide.asd"))' \
	        --eval '(asdf:test-system "nucleotide")'

lint:
	$(SBCL) --load scripts/lint.lisp

fmt:
	$(EMACS) -l scripts/fmt.el $(LISP_FILES)

keysyms:
	guix shell libxkbcommon -- sh -c \
	  'install -m 644 "$$GUIX_ENVIRONMENT/include/xkbcommon/xkbcommon-keysyms.h" protocol/'

clean:
	rm -rf ./nucleotide
