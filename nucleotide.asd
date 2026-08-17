;; SPDX-License-Identifier: GPL-3.0-or-later

(asdf:defsystem #:nucleotide
  :description "A hackable window manager for the river Wayland compositor"
  :license "GPL-3.0-or-later"
  :depends-on (#:sb-bsd-sockets #:sb-concurrency)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "wire")
	       (:file "client")
	       (:file "protocols")
	       (:file "xml")
	       (:file "scanner")
	       (:file "river")
	       (:file "eventloop")
	       (:file "wm")
	       (:file "debug")))
