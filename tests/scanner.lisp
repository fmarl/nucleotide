;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite scanner :in :nucleotide)
(in-suite scanner)

(defparameter *protocol*
  "<protocol name=\"test\">
  <interface name=\"test_thing_v1\" version=\"3\">
    <request name=\"destroy\" type=\"destructor\"/>
    <request name=\"get_child\">
      <arg name=\"id\" type=\"new_id\" interface=\"test_child_v1\"/>
      <arg name=\"scale\" type=\"fixed\"/>
      <arg name=\"parent\" type=\"object\" interface=\"test_thing_v1\"/>
    </request>
    <request name=\"newer\" since=\"2\"/>
    <event name=\"title\"><arg name=\"title\" type=\"string\"/></event>
    <enum name=\"edges\" bitfield=\"true\">
      <entry name=\"top\" value=\"1\"/>
      <entry name=\"all\" value=\"0xf\"/>
    </enum>
  </interface>
</protocol>")

(defun forms (head)
  (remove head (let ((*package* (find-package '#:nucleotide/tests)))
                 (w::protocol-forms (xml:parse *protocol*)))
          :key #'first :test-not #'eq))

(test interface-form
  (is (equal '((w::define-interface test-thing-v1 "test_thing_v1"))
             (forms 'w::define-interface))))

(test requests-get-opcodes-in-order
  (is (equal '((w::define-request (test-thing-v1 destroy 0 :destructor t))
               (w::define-request (test-thing-v1 get-child 1)
                 (id (:new-id test-child-v1))
                 (scale :fixed)
                 (parent (:object)))
               (w::define-request (test-thing-v1 newer 2 :since 2)))
             (forms 'w::define-request))))

(test events-use-keywords
  (is (equal '((w::define-event (test-thing-v1 :title 0) (title :string)))
             (forms 'w::define-event))))

(test enums-become-constants
  (is (equal '((defconstant +test-thing-v1-edges-top+ 1)
               (defconstant +test-thing-v1-edges-all+ 15))
             (forms 'defconstant))))

(test generated-river-constants
  (is (= 64 n::+river-seat-v1-modifiers-mod4+))
  (is (= 15 n::+all-edges+)))

(test rejects-other-documents
  (signals w::nucleotide-error (w::protocol-forms (xml:parse "<html/>"))))
