# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

SBCL := sbcl --non-interactive
EMACS := emacs --batch -Q
LISP_FILES := $(wildcard *.lisp *.asd src/*.lisp src/core/*.lisp tests/*.lisp scripts/*.lisp)

.PHONY: all no-repl check lint fmt fmt-check clean

all:
	$(SBCL) --load build.lisp

no-repl:
	NUCLEOTIDE_NO_REPL=1 $(SBCL) --load build.lisp

check:
	$(SBCL) --eval '(require :asdf)' \
	        --eval '(asdf:load-asd (truename "nucleotide.asd"))' \
	        --eval '(asdf:test-system "nucleotide")'

lint:
	$(SBCL) --load scripts/lint.lisp

fmt:
	$(EMACS) -l scripts/fmt.el $(LISP_FILES)

fmt-check:
	FMT_CHECK=1 $(EMACS) -l scripts/fmt.el $(LISP_FILES)

clean:
	rm -rf ./nucleotide
