;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(asdf:defsystem #:nucleotide
  :description "A hackable window manager for the river Wayland compositor"
  :license "GPL-3.0-or-later"
  :version "0.4.0"
  :depends-on (#:sb-bsd-sockets #:sb-concurrency #:sb-posix)
  :pathname "src"
  :serial t
  :components ((:module "protocol"
                        :pathname "../protocol"
                        :components ((:static-file "river-window-management-v1.xml")
                                     (:static-file "river-xkb-bindings-v1.xml")
                                     (:static-file "river-layer-shell-v1.xml")
                                     (:static-file "river-input-management-v1.xml")
                                     (:static-file "river-libinput-config-v1.xml")))
               (:file "package")
               (:module "core"
                        :serial t
                        :components ((:file "log")
                                     (:file "conditions")
                                     (:file "wire")
                                     (:file "client")
                                     (:file "xml")
                                     (:file "wayland")
                                     (:file "scanner")
                                     (:file "river")
                                     (:file "eventloop")))
               (:file "util")
               (:file "values")
               (:file "geometry")
               (:file "json")
               (:file "settings")
               (:file "hooks")
               (:file "commands")
               (:file "keys")
               (:file "bindings")
               (:file "repl")
               (:file "model")
               (:file "workspaces")
               (:file "layouts")
               (:file "events")
               (:file "autostart")
               (:file "libinput")
               (:file "wm")
               (:file "outputs")
               (:file "floating")
               (:file "rules")
               (:file "manage")
               (:file "render")
               (:file "windows")
               (:file "highlight")
               (:file "ipc")
               (:file "scrolling")
               (:file "scrolling-commands")
               (:file "switcher")
               (:file "defaults")
               (:file "init-file")
               (:file "main"))
  :in-order-to ((asdf:test-op (asdf:test-op "nucleotide/tests"))))

(asdf:defsystem #:nucleotide/tests
  :description "Tests for nucleotide"
  :license "GPL-3.0-or-later"
  :depends-on (#:nucleotide #:fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "xml")
               (:file "json")
               (:file "scanner")
               (:file "client")
               (:file "server")
               (:file "workspaces")
               (:file "layouts")
               (:file "scrolling")
               (:file "keys")
               (:file "commands")
               (:file "outputs")
               (:file "floating")
               (:file "rules")
               (:file "manage")
               (:file "render")
               (:file "highlight")
               (:file "ipc")
               (:file "hooks")
               (:file "config")
               (:file "repl")
               (:file "libinput")
               (:file "switcher"))
  :perform (asdf:test-op (o c)
                         (unless (uiop:symbol-call '#:fiveam '#:run! :nucleotide)
                           (error "nucleotide tests failed"))))
