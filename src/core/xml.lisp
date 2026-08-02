;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide.xml)


(defstruct (cursor (:constructor make-cursor (string)))
  (string "" :type string :read-only t)
  (pos 0 :type fixnum))

(defun eof-p (cursor)
  (>= (cursor-pos cursor) (length (cursor-string cursor))))

(defun current (cursor)
  (unless (eof-p cursor)
    (char (cursor-string cursor) (cursor-pos cursor))))

(defun looking-at-p (cursor prefix)
  (let ((string (cursor-string cursor))
        (pos (cursor-pos cursor)))
    (and (<= (+ pos (length prefix)) (length string))
         (string= prefix string :start2 pos :end2 (+ pos (length prefix))))))

(defun expect (cursor char)
  (unless (eql (current cursor) char)
    (error "expected ~C at position ~D" char (cursor-pos cursor)))
  (incf (cursor-pos cursor)))

(defun skip-past (cursor marker)
  (let ((found (search marker (cursor-string cursor)
                       :start2 (cursor-pos cursor))))
    (unless found
      (error "unterminated ~S" marker))
    (setf (cursor-pos cursor) (+ found (length marker)))))

(defun skip-whitespace (cursor)
  (loop while (member (current cursor) '(#\Space #\Tab #\Newline #\Return))
        do (incf (cursor-pos cursor))))

(defun skip-to-tag (cursor)
  (loop
    (cond ((eof-p cursor) (return))
          ((looking-at-p cursor "<!--") (skip-past cursor "-->"))
          ((looking-at-p cursor "<?") (skip-past cursor "?>"))
          ((looking-at-p cursor "<!") (skip-past cursor ">"))
          ((eql #\< (current cursor)) (return))
          (t (incf (cursor-pos cursor))))))

(defun read-name (cursor)
  (let ((start (cursor-pos cursor)))
    (loop while (let ((c (current cursor)))
                  (and c (or (alphanumericp c) (find c "_-:."))))
          do (incf (cursor-pos cursor)))
    (when (= start (cursor-pos cursor))
      (error "expected a name at position ~D" start))
    (subseq (cursor-string cursor) start (cursor-pos cursor))))

(defun unescape (string)
  (if (not (find #\& string))
      string
      (with-output-to-string (out)
        (let ((pos 0))
          (loop while (< pos (length string))
                do (let ((amp (position #\& string :start pos)))
                     (cond
                       ((null amp)
                        (write-string string out :start pos)
                        (return))
                       (t
                        (write-string string out :start pos :end amp)
                        (let* ((end (position #\; string :start amp))
                               (entity (subseq string (1+ amp) end)))
                          (write-string
                           (cond ((string= entity "lt") "<")
                                 ((string= entity "gt") ">")
                                 ((string= entity "amp") "&")
                                 ((string= entity "quot") "\"")
                                 ((string= entity "apos") "'")
                                 (t (error "unknown XML entity &~A;" entity)))
                           out)
                          (setf pos (1+ end)))))))))))

(defun read-attribute (cursor)
  (skip-whitespace cursor)
  (let ((c (current cursor)))
    (when (or (null c) (member c '(#\> #\/)))
      (return-from read-attribute nil)))
  (let ((name (read-name cursor)))
    (skip-whitespace cursor)
    (expect cursor #\=)
    (skip-whitespace cursor)
    (let ((quote-char (current cursor)))
      (unless (member quote-char '(#\" #\'))
        (error "expected a quoted value for attribute ~A" name))
      (incf (cursor-pos cursor))
      (let* ((string (cursor-string cursor))
             (end (position quote-char string :start (cursor-pos cursor))))
        (unless end
          (error "unterminated value for attribute ~A" name))
        (prog1 (cons name (unescape (subseq string (cursor-pos cursor) end)))
          (setf (cursor-pos cursor) (1+ end)))))))

(defun read-attributes (cursor)
  (loop for attribute = (read-attribute cursor)
        while attribute
        collect attribute))

(defun read-element (cursor)
  (expect cursor #\<)
  (let ((name (read-name cursor))
        (attrs (read-attributes cursor))
        (children '()))
    (skip-whitespace cursor)
    (cond
      ((looking-at-p cursor "/>")
       (incf (cursor-pos cursor) 2))
      ((eql #\> (current cursor))
       (incf (cursor-pos cursor))
       (loop
         (skip-to-tag cursor)
         (when (eof-p cursor)
           (error "unterminated element ~A" name))
         (when (looking-at-p cursor "</")
           (incf (cursor-pos cursor) 2)
           (let ((closing (read-name cursor)))
             (unless (string= closing name)
               (error "mismatched close tag: <~A> closed by </~A>"
                      name closing)))
           (skip-whitespace cursor)
           (expect cursor #\>)
           (return))
         (push (read-element cursor) children)))
      (t (error "malformed element ~A" name)))
    (list name attrs (nreverse children))))

(defun parse (string)
  (let ((cursor (make-cursor string)))
    (skip-to-tag cursor)
    (read-element cursor)))

(defun node-name (node) (first node))
(defun node-attrs (node) (second node))
(defun node-children (node) (third node))

(defun attr (node name)
  (cdr (assoc name (node-attrs node) :test #'string=)))

(defun children-named (node name)
  (remove name (node-children node) :key #'node-name :test #'string/=))
