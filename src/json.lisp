;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun json-bool (value)
  (if value :true :false))

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop for char across string
        do (case char
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (t (if (< (char-code char) #x20)
                    (format stream "\\u~4,'0X" (char-code char))
                    (write-char char stream)))))
  (write-char #\" stream))

(defun write-json (value stream)
  (flet ((write-items (open items write-item close)
           (write-char open stream)
           (loop for (item . more) on items
                 do (funcall write-item item)
                 (when more
                   (write-char #\, stream)))
           (write-char close stream)))
    (case value
      (:true (write-string "true" stream))
      (:false (write-string "false" stream))
      (:null (write-string "null" stream))
      (t (etypecase value
           (string (write-json-string value stream))
           (integer (format stream "~D" value))
           (list (if (eq (first value) :object)
                     (write-items #\{ (rest value)
                                  (lambda (pair)
                                    (write-json-string (car pair) stream)
                                    (write-char #\: stream)
                                    (write-json (cdr pair) stream))
                                  #\})
                     (write-items #\[ value (lambda (item) (write-json item stream)) #\]))))))))

(defun json-string (value)
  (with-output-to-string (stream)
    (write-json value stream)))

(defun json-get (object &rest keys)
  (reduce (lambda (object key)
            (and (consp object)
                 (eq (first object) :object)
                 (cdr (assoc key (rest object) :test #'string=))))
          keys
          :initial-value object))

(defun read-json-char (in &rest expected)
  (let ((char (and (peek-char t in nil) (read-char in))))
    (unless (member char expected)
      (nucleotide-error "expected ~{~C~^ or ~} in JSON" expected))
    char))

(defun read-json-escape (in)
  (let ((char (read-char in nil)))
    (case char
      (#\n #\Newline)
      (#\t #\Tab)
      (#\r #\Return)
      (#\b #\Backspace)
      (#\f #\Page)
      (#\u (code-char (parse-integer (map 'string (lambda (i)
                                                    (declare (ignore i))
                                                    (read-char in))
                                          '(0 1 2 3))
                                     :radix 16)))
      ((nil) (nucleotide-error "unterminated JSON string"))
      (t char))))

(defun read-json-string (in)
  (read-json-char in #\")
  (with-output-to-string (out)
    (loop for char = (read-char in nil)
          do (case char
               ((nil) (nucleotide-error "unterminated JSON string"))
               (#\" (return))
               (#\\ (write-char (read-json-escape in) out))
               (t (write-char char out))))))

(defun read-json-token (in)
  (with-output-to-string (out)
    (loop for char = (peek-char nil in nil)
          until (member char '(nil #\, #\] #\} #\: #\Space #\Tab #\Newline #\Return))
          do (write-char (read-char in) out))))

(defun read-json-items (in close read-item)
  (if (eql (peek-char t in nil) close)
      (progn (read-char in) '())
      (loop collect (funcall read-item)
            until (eql (read-json-char in #\, close) close))))

(defconstant +json-max-depth+ 64
  "Deeper input is rejected before it can exhaust the stack.")

(defun read-json (in &optional (depth 0))
  (when (> depth +json-max-depth+)
    (nucleotide-error "JSON nested too deeply"))
  (flet ((read-value () (read-json in (1+ depth))))
    (case (peek-char t in nil)
      ((nil) (nucleotide-error "unexpected end of JSON"))
      (#\{ (read-char in)
           (cons :object (read-json-items in #\}
                                          (lambda ()
                                            (let ((key (read-json-string in)))
                                              (read-json-char in #\:)
                                              (cons key (read-value)))))))
      (#\[ (read-char in)
           (read-json-items in #\] #'read-value))
      (#\" (read-json-string in))
      (t (let ((token (read-json-token in)))
           (cond ((string= token "true") :true)
                 ((string= token "false") :false)
                 ((string= token "null") :null)
                 ((ignore-errors (parse-integer token)))
                 (t (nucleotide-error "unsupported JSON value ~S" token))))))))

(defun parse-json (string)
  (with-input-from-string (in string)
    (prog1 (read-json in)
      (when (peek-char t in nil)
        (nucleotide-error "trailing characters after JSON")))))
