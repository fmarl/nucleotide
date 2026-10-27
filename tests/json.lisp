;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite json :in :nucleotide)
(in-suite json)

(test encode-values
  (is (equal "{\"a\":1,\"b\":true,\"c\":false,\"d\":null,\"e\":[1,\"f\"],\"g\":[]}"
             (json:encode '(:object ("a" . 1) ("b" . :true) ("c" . :false) ("d" . :null)
                            ("e" 1 "f") ("g")))))
  (is (equal "\"x\\\"y\\\\z\"" (json:encode "x\"y\\z")))
  (is (equal "\"a\\u000Ab\"" (json:encode (format nil "a~%b"))))
  (is (equal "\"grüße\"" (json:encode "grüße"))))

(test parse-values
  (is (equal "EventStream" (json:parse "\"EventStream\"")))
  (is (= 7 (json:lookup (json:parse
                         "{\"Action\":{\"FocusWorkspace\":{\"reference\":{\"Id\":7}}}}")
                        "Action" "FocusWorkspace" "reference" "Id")))
  (is (equal '(1 "a\"b" :true :null (:object))
             (json:parse "[1, \"a\\\"b\", true, null, {}]")))
  (signals json:json-error (json:parse "{\"a\":"))
  (signals json:json-error (json:parse "1 2"))
  (signals json:json-error (json:parse "[1.5]"))
  (is (equal "€" (json:parse "\"\\u20ac\"")))
  (signals json:json-error (json:parse "\"\\u12"))
  (signals json:json-error (json:parse "\"\\uZZZZ\""))
  (is (equal (string (code-char #x1f600)) (json:parse "\"\\ud83d\\ude00\"")))
  (signals json:json-error (json:parse "\"\\ud83d\""))
  (signals json:json-error (json:parse "\"\\ude00\"")))

(test deeply-nested-json-is-rejected
  (signals json:json-error
           (json:parse (make-string 60000 :initial-element #\[))))
