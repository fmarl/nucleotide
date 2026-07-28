(require :asdf)
(asdf:load-asd (merge-pathnames "nucleotide.asd" *load-truename*))
(asdf:load-system "nucleotide")

(when (asdf:find-system "slynk" nil)
  (asdf:load-system "slynk"))

(if (asdf:find-system "slynk" nil)
    (progn
      (asdf:load-system "slynk")
      (when (asdf:find-system "slynk/mrepl" nil)
	(asdf:load-system "slynk/mrepl"))
      (format t "~&nucleotide build: slynk backed in, SLY will listen on port 4005~%")))

(sb-ext:save-lisp-and-die
 "nucleotide"
 :executable t
 :compression t
 :toplevel (lambda ()
	     (sb-ext:disable-debugger)
	     (nucleotide:start-repl-server :port 4005)
	     (nucleotide:start-wm)
	     (sb-thread:join-thread (nucleotide::wm-thread nucleotide:*wm*))))
