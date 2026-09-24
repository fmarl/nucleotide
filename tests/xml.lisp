;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite xml :in :nucleotide)
(in-suite xml)

(test parse-nested-elements
  (let ((root (xml:parse "<?xml version=\"1.0\"?>
<!-- a comment -->
<protocol name=\"p\">
  <interface name='i' version=\"2\"><request name=\"r\"/></interface>
  <copyright>text is skipped</copyright>
</protocol>")))
    (is (string= "protocol" (xml:node-name root)))
    (is (string= "p" (xml:attribute root "name")))
    (let ((interface (first (xml:children-named root "interface"))))
      (is (string= "2" (xml:attribute interface "version")))
      (is (string= "r" (xml:attribute (first (xml:node-children interface)) "name"))))))

(test unescape-entities
  (is (string= "a<b>&\"'" (xml:attribute (xml:parse "<e v=\"a&lt;b&gt;&amp;&quot;&apos;\"/>")
                                         "v"))))

(test malformed-input
  (signals xml:xml-error (xml:parse "<a></b>"))
  (signals xml:xml-error (xml:parse "<a>"))
  (signals xml:xml-error (xml:parse "<a v=x/>"))
  (signals xml:xml-error (xml:parse "<a v=\"&nope;\"/>"))
  (signals xml:xml-error (xml:parse "<a v=\"&amp\"/>")))
