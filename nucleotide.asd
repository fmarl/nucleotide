;; SPDX-License-Identifier: GPL-3.0-or-later

(asdf:defsystem #:nucleotide
  :description "A hackable window manager for the river Wayland compositor"
  :license "GPL-3.0-or-later"
  :depends-on (#:sb-bsd-sockets #:sb-concurrency)
  :pathname "src"
  :serial t
  :components ((:file "package")	       
	       (:module "core"
		:serial t
		:components ((:file "wire")
			     (:file "client")
			     (:file "xml")
			     (:file "protocols")
			     (:file "scanner")
			     (:file "river")
			     (:file "eventloop")
			     (:file "debug")))
	       (:file "wm")
	       (:file "keybinds")
	       (:file "layouts")))
