;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keysym-header-pathname ()
    (or (loop for dir in (append (mapcar (lambda (dir) (merge-pathnames "../include/" dir))
                                         (uiop:getenv-absolute-directories "PATH"))
                                 (uiop:getenv-absolute-directories "C_INCLUDE_PATH")
                                 (uiop:getenv-absolute-directories "CPATH")
                                 (list #p"/usr/include/"))
              thereis (probe-file (merge-pathnames "xkbcommon/xkbcommon-keysyms.h" dir)))
        (error "xkbcommon-keysyms.h not found, is libxkbcommon installed?")))

  (defun header-codepoint (line)
    "The character of a comment like /* U+20AC EURO SIGN */. Inexact ones look
like /*(U+...)*/ or /*<U+...>*/."
    (let ((start (search "/* U+" line)))
      (and start (parse-integer line :start (+ start 5) :radix 16 :junk-allowed t))))

  (defun keysym-header-entries (pathname)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil)
            while line
            when (uiop:string-prefix-p "#define XKB_KEY_" line)
            collect (destructuring-bind (name value &rest comment)
                        (remove "" (uiop:split-string (subseq line 16)) :test #'string=)
                      (declare (ignore comment))
                      (list name (parse-integer value :start 2 :radix 16)
                            (header-codepoint line)))))))

(defun keysym-table (entries)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name keysym) in entries
          do (setf (gethash name table) keysym))
    table))

(defun codepoint-table (entries)
  (let ((table (make-hash-table)))
    (loop for (nil keysym codepoint) in entries
          when codepoint
          do (setf (gethash codepoint table)
                   (min keysym (gethash codepoint table keysym))))
    table))

(defun keysym-table-ignoring-case (keysyms)
  "Names that differ only in case resolve to the one with the most lower case
letters, like Greek_alpha, as in xkb_keysym_from_name."
  (let ((table (make-hash-table :test #'equalp)))
    (flet ((lower-count (name) (count-if #'lower-case-p name)))
      (maphash (lambda (name keysym)
                 (let ((other (gethash name table)))
                   (when (or (null other)
                             (> (lower-count name) (lower-count (car other)))
                             (and (= (lower-count name) (lower-count (car other)))
                                  (< keysym (cdr other))))
                     (setf (gethash name table) (cons name keysym)))))
               keysyms))
    table))

(defmacro define-keysyms ()
  (let ((entries (keysym-header-entries (keysym-header-pathname))))
    `(progn
       (defparameter *keysyms* (keysym-table ',entries))
       (defparameter *keysyms-ignoring-case* (keysym-table-ignoring-case *keysyms*))
       (defparameter *codepoint-keysyms* (codepoint-table ',entries)))))

(define-keysyms)

(defun char-keysym (char)
  "The keysym of CHAR as in xkb_utf32_to_keysym, like EuroSign for €: river
matches bindings with the keysyms of the layout."
  (let ((code (char-code char)))
    (cond ((or (<= #x20 code #x7e) (<= #xa0 code #xff)) code)
          ((gethash code *codepoint-keysyms*))
          (t (logior #x1000000 code)))))

(defun keysym-from-name (name)
  (or (gethash name *keysyms*)
      (cdr (gethash name *keysyms-ignoring-case*))))

(defparameter *modifier-names*
  `(("Shift" . ,+river-seat-v1-modifiers-shift+)
    ("Ctrl" . ,+river-seat-v1-modifiers-ctrl+)
    ("Control" . ,+river-seat-v1-modifiers-ctrl+)
    ("Alt" . ,+river-seat-v1-modifiers-mod1+)
    ("Mod1" . ,+river-seat-v1-modifiers-mod1+)
    ("Mod3" . ,+river-seat-v1-modifiers-mod3+)
    ("Super" . ,+river-seat-v1-modifiers-mod4+)
    ("Mod4" . ,+river-seat-v1-modifiers-mod4+)
    ("Mod" . ,+river-seat-v1-modifiers-mod4+)
    ("Mod5" . ,+river-seat-v1-modifiers-mod5+)))

(defun split (string separator)
  (remove "" (uiop:split-string string :separator separator) :test #'string=))

(defun parse-modifiers (names)
  (reduce #'logior
          (mapcar (lambda (name)
                    (or (cdr (assoc name *modifier-names* :test #'string-equal))
                        (nucleotide-error "unknown modifier ~S" name)))
                  (if (stringp names) (split names "+") names))
          :initial-value 0))

(defun key-keysym (name)
  (let ((keysym (if (= 1 (length name))
                    (char-keysym (char-downcase (char name 0)))
                    (keysym-from-name name))))
    (when (member keysym '(nil 0))
      (nucleotide-error "unknown key ~S" name))
    keysym))

(defun parse-key (key)
  "The (keysym . modifiers) of KEY, like \"Super+Shift+C\" or \"XF86AudioPlay\"."
  (let ((parts (split key "+")))
    (unless parts
      (nucleotide-error "~S is not a key" key))
    (cons (key-keysym (car (last parts))) (parse-modifiers (butlast parts)))))

(defun parse-key-sequence (keys)
  (let ((sequence (mapcar #'parse-key (split keys " "))))
    (unless (<= 1 (length sequence) 2)
      (nucleotide-error "~S is not a key or a prefix and a key" keys))
    sequence))

(defvar *bindings* '()
  "Alist from key sequences, lists of (keysym . modifiers), to actions.")

(defun prefix-p (a b)
  (and (< (length a) (length b)) (equal a (subseq b 0 (length a)))))

(defun set-binding (keys action)
  "Like BIND with an evaluated ACTION. Replaces bindings of the same keys and of
chords that share their prefix with KEYS."
  (let ((sequence (parse-key-sequence keys))
        (action (normalize-action action)))
    (setf *bindings*
          (append (remove-if (lambda (other)
                               (or (equal other sequence)
                                   (prefix-p other sequence)
                                   (prefix-p sequence other)))
                             *bindings* :key #'car)
                  (list (cons sequence action))))
    keys))

(defmacro bind (keys action)
  "Bind KEYS, like \"Super+Q\" or \"Super+Space T\", to ACTION, like (focus :left)."
  `(set-binding ,keys ',action))

(defun unbind (keys)
  "Remove the binding of KEYS."
  (setf *bindings* (remove (parse-key-sequence keys) *bindings* :key #'car :test #'equal)))
