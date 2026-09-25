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
    </request>
    <event name=\"title\"><arg name=\"title\" type=\"string\"/></event>
    <enum name=\"edges\" bitfield=\"true\">
      <entry name=\"top\" value=\"1\"/>
      <entry name=\"all\" value=\"0xf\"/>
    </enum>
  </interface>
</protocol>")

(defun forms (head)
  (remove head (n::protocol-forms (xml:parse *protocol*))
          :key #'first :test-not #'eq))

(test interface-form
  (is (equal '((n::define-interface n::test-thing-v1 "test_thing_v1"))
             (forms 'n::define-interface))))

(test requests-get-opcodes-in-order
  (is (equal '((n::define-request (n::test-thing-v1 n::destroy 0 :destructor t))
               (n::define-request (n::test-thing-v1 n::get-child 1)
                 (n::id (:new-id n::test-child-v1))
                 (n::scale :fixed)))
             (forms 'n::define-request))))

(test events-use-keywords
  (is (equal '((n::define-event (n::test-thing-v1 :title 0) (n::title :string)))
             (forms 'n::define-event))))

(test enums-become-constants
  (is (equal '((defconstant n::+test-thing-v1-edges-top+ 1)
               (defconstant n::+test-thing-v1-edges-all+ 15))
             (forms 'defconstant))))

(test generated-river-constants
  (is (= 64 n::+river-seat-v1-modifiers-mod4+))
  (is (= 15 n::+all-edges+)))

(test rejects-other-documents
  (signals n:nucleotide-error (n::protocol-forms (xml:parse "<html/>"))))
