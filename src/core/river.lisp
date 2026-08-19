;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *river-protocol-files*
    '("river-window-management-v1.xml"
      "river-xkb-bindings-v1.xml"
      "river-layer-shell-v1.xml"
      "river-input-management-v1.xml"
      "river-libinput-config-v1.xml"
      "river-xkb-config-v1.xml"))

  (defun load-river-protocols ()
    (dolist (file *river-protocol-files*)
      (load-protocol (asdf:system-relative-pathname
                      "nucleotide" (concatenate 'string "protocol/" file)))))

  (load-river-protocols))
