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

(test only-control-characters-are-escaped
  (is (equal (format nil "\"\\u001F ~~~C\"" (code-char #x7f))
             (json:encode (coerce (list (code-char #x1f) #\Space #\~ (code-char #x7f))
                                  'string)))))

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

(test separators-are-required
  (signals json:json-error (json:parse "[1 2]"))
  (signals json:json-error (json:parse "{\"a\" 1}"))
  (signals json:json-error (json:parse "{\"a\":1 \"b\":2}"))
  (signals json:json-error (json:parse "[1,]"))
  (signals json:json-error (json:parse "{\"a\":1,}")))

(test u-escapes-take-four-hex-digits
  (is (equal (string (code-char #xff)) (json:parse "\"\\u00fF\"")))
  (signals json:json-error (json:parse "\"\\u00g0\""))
  (signals json:json-error (json:parse "\"\\u00G0\"")))

(test surrogates-pair-only-in-their-ranges
  (is (equal (string (code-char #xd7ff)) (json:parse "\"\\ud7ff\"")))
  (is (equal (string (code-char #xe000)) (json:parse "\"\\ue000\"")))
  (is (equal (string (code-char #x10000)) (json:parse "\"\\ud800\\udc00\"")))
  (is (equal (string (code-char #x10ffff)) (json:parse "\"\\udbff\\udfff\"")))
  (signals json:json-error (json:parse "\"\\udc00\""))
  (signals json:json-error (json:parse "\"\\udfff\""))
  (signals json:json-error (json:parse "\"\\ud800\\u0041\""))
  (signals json:json-error (json:parse "\"\\udbff\\ue000\""))
  (signals json:json-error (json:parse "\"\\ud800\\udbff\""))
  (signals json:json-error (json:parse "\"\\udc00\\udc00\"")))

(defun nested-arrays (depth)
  (concatenate 'string (make-string depth :initial-element #\[) "1"
               (make-string depth :initial-element #\])))

(test nesting-up-to-the-limit-is-accepted
  (is (equal '((1)) (json:parse (nested-arrays 2))))
  (finishes (json:parse (nested-arrays json::+json-max-depth+)))
  (signals json:json-error (json:parse (nested-arrays (1+ json::+json-max-depth+)))))

;;; Generated values, from a fixed seed so that a failure can be rerun.

(defvar *json-random* (sb-ext:seed-random-state 4711))

(defun random-below (n)
  (random n *json-random*))

(defun random-json-char ()
  (code-char (loop for code = (case (random-below 4)
                                (0 (random-below #x20))
                                (1 (random-below #x80))
                                (2 (random-below #x10000))
                                (t (+ #x10000 (random-below #x100000))))
                   unless (<= #xd800 code #xdfff)
                     return code)))

(defun random-json-string ()
  (coerce (loop repeat (random-below 8) collect (random-json-char)) 'string))

(defun random-json (depth)
  (case (random-below (if (> depth 3) 3 5))
    (0 (random-json-string))
    (1 (- (random-below (expt 2 70)) (expt 2 69)))
    (2 (elt '(:true :false :null) (random-below 3)))
    (3 (loop repeat (random-below 4) collect (random-json (1+ depth))))
    (t (cons :object (loop repeat (random-below 4)
                           collect (cons (random-json-string)
                                         (random-json (1+ depth))))))))

(test encoded-values-parse-back
  (let ((changed '()))
    (dotimes (i 2000)
      (let ((value (random-json 0)))
        (unless (equal value (json:parse (json:encode value)))
          (push value changed))))
    (is (null changed) "~D values came back changed, e.g. ~S"
        (length changed) (first changed))))

(defun damaged (text)
  "TEXT with one character deleted, inserted or replaced."
  (let* ((alphabet "[]{}\",:\\u0123456789abcdeflnrtu -+.")
         (char (string (char alphabet (random-below (length alphabet)))))
         (i (random-below (1+ (length text))))
         (j (min (length text) (1+ i))))
    (case (random-below 3)
      (0 (concatenate 'string (subseq text 0 i) (subseq text j)))
      (1 (concatenate 'string (subseq text 0 i) char (subseq text i)))
      (t (concatenate 'string (subseq text 0 i) char (subseq text j))))))

(test malformed-json-signals-only-json-errors
  (let ((other '()))
    (dotimes (i 5000)
      (let ((text (json:encode (random-json 0))))
        (dotimes (k (1+ (random-below 3)))
          (setf text (damaged text)))
        (handler-case (json:parse text)
          (json:json-error ())
          (error (e) (push (list text (type-of e)) other)))))
    (is (null other) "~D inputs signalled other errors, e.g. ~S"
        (length other) (first other))))
