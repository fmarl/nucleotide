all:
	sbcl --non-interactive --load build.lisp

clean:
	rm -rf ./nucleotide
