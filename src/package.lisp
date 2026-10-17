;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide.xml
  (:use #:cl)
  (:export
   #:parse
   #:xml-error
   #:node-name
   #:node-attrs
   #:node-children
   #:attribute
   #:children-named))

(defpackage #:nucleotide
  (:use #:cl)
  (:local-nicknames (#:xml #:nucleotide.xml)))
