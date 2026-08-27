;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide.xml)

(define-condition xml-error (simple-error) ())

(defun xml-error (control &rest args)
  (error 'xml-error :format-control control :format-arguments args))

(defstruct (cursor (:constructor make-cursor (string)))
  (string "" :type string :read-only t)
  (pos 0 :type fixnum))

(defun eofp (cursor)
  (>= (cursor-pos cursor) (length (cursor-string cursor))))

(defun current-char (cursor)
  (unless (eofp cursor)
    (char (cursor-string cursor) (cursor-pos cursor))))

(defun looking-at-p (cursor prefix)
  (let ((string (cursor-string cursor))
        (pos (cursor-pos cursor)))
    (and (<= (+ pos (length prefix)) (length string))
         (string= prefix string :start2 pos :end2 (+ pos (length prefix))))))

(defun expect (cursor char)
  (unless (eql (current-char cursor) char)
    (xml-error "expected ~C at position ~D" char (cursor-pos cursor)))
  (incf (cursor-pos cursor)))

(defun skip-past (cursor marker)
  (let ((found (search marker (cursor-string cursor)
                       :start2 (cursor-pos cursor))))
    (unless found
      (xml-error "unterminated ~S" marker))
    (setf (cursor-pos cursor) (+ found (length marker)))))

(defun skip-whitespace (cursor)
  (loop while (member (current-char cursor) '(#\Space #\Tab #\Newline #\Return))
        do (incf (cursor-pos cursor))))

(defun skip-to-tag (cursor)
  (loop
   (cond ((eofp cursor) (return))
         ((looking-at-p cursor "<!--") (skip-past cursor "-->"))
         ((looking-at-p cursor "<?") (skip-past cursor "?>"))
         ((looking-at-p cursor "<!") (skip-past cursor ">"))
         ((eql #\< (current-char cursor)) (return))
         (t (incf (cursor-pos cursor))))))

(defun read-name (cursor)
  (let ((start (cursor-pos cursor)))
    (loop while (let ((c (current-char cursor)))
                  (and c (or (alphanumericp c) (find c "_-:."))))
          do (incf (cursor-pos cursor)))
    (when (= start (cursor-pos cursor))
      (xml-error "expected a name at position ~D" start))
    (subseq (cursor-string cursor) start (cursor-pos cursor))))

(defparameter *entities*
  '(("lt" . "<") ("gt" . ">") ("amp" . "&") ("quot" . "\"") ("apos" . "'")))

(defun unescape (string)
  (let ((amp (position #\& string)))
    (if (null amp)
        string
        (let* ((end (or (position #\; string :start amp)
                        (xml-error "unterminated entity in ~S" string)))
               (entity (subseq string (1+ amp) end)))
          (concatenate 'string
                       (subseq string 0 amp)
                       (or (cdr (assoc entity *entities* :test #'string=))
                           (xml-error "unknown XML entity &~A;" entity))
                       (unescape (subseq string (1+ end))))))))

(defun read-attribute (cursor)
  (skip-whitespace cursor)
  (let ((c (current-char cursor)))
    (when (or (null c) (member c '(#\> #\/)))
      (return-from read-attribute nil)))
  (let ((name (read-name cursor)))
    (skip-whitespace cursor)
    (expect cursor #\=)
    (skip-whitespace cursor)
    (let ((quote-char (current-char cursor)))
      (unless (member quote-char '(#\" #\'))
        (xml-error "expected a quoted value for attribute ~A" name))
      (incf (cursor-pos cursor))
      (let* ((string (cursor-string cursor))
             (end (position quote-char string :start (cursor-pos cursor))))
        (unless end
          (xml-error "unterminated value for attribute ~A" name))
        (prog1 (cons name (unescape (subseq string (cursor-pos cursor) end)))
          (setf (cursor-pos cursor) (1+ end)))))))

(defun read-attributes (cursor)
  (loop for attribute = (read-attribute cursor)
        while attribute
        collect attribute))

(defun read-closing-tag-p (cursor name)
  (when (eofp cursor)
    (xml-error "unterminated element ~A" name))
  (when (looking-at-p cursor "</")
    (incf (cursor-pos cursor) 2)
    (let ((closing (read-name cursor)))
      (unless (string= closing name)
        (xml-error "mismatched close tag: <~A> closed by </~A>" name closing)))
    (skip-whitespace cursor)
    (expect cursor #\>)
    t))

(defun read-children (cursor name)
  (loop do (skip-to-tag cursor)
        until (read-closing-tag-p cursor name)
        collect (read-element cursor)))

(defun read-element (cursor)
  (expect cursor #\<)
  (let ((name (read-name cursor))
        (attrs (read-attributes cursor)))
    (skip-whitespace cursor)
    (list name
          attrs
          (cond ((looking-at-p cursor "/>")
                 (incf (cursor-pos cursor) 2)
                 '())
                ((eql #\> (current-char cursor))
                 (incf (cursor-pos cursor))
                 (read-children cursor name))
                (t (xml-error "malformed element ~A" name))))))

(defun parse (string)
  (let ((cursor (make-cursor string)))
    (skip-to-tag cursor)
    (read-element cursor)))

(defun node-name (node) (first node))
(defun node-attrs (node) (second node))
(defun node-children (node) (third node))

(defun attribute (node name)
  (cdr (assoc name (node-attrs node) :test #'string=)))

(defun children-named (node name)
  (remove name (node-children node) :key #'node-name :test #'string/=))
