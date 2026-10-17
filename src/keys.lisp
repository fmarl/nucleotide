;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *libxkbcommon* "libxkbcommon.so.0")

(defun xkb-keysym-from-name-address ()
  (unless (sb-sys:find-dynamic-foreign-symbol-address "xkb_keysym_from_name")
    (sb-alien:load-shared-object *libxkbcommon* :dont-save t))
  (sb-sys:find-dynamic-foreign-symbol-address "xkb_keysym_from_name"))

(defconstant +keysym-case-insensitive+ 1)

(defun keysym-from-name (name)
  (let ((address (xkb-keysym-from-name-address)))
    (flet ((lookup (flags)
             (sb-alien:alien-funcall
              (sb-alien:sap-alien (sb-sys:int-sap address)
                                  (function sb-alien:unsigned-int sb-alien:c-string sb-alien:int))
              name flags)))
      (let ((keysym (lookup 0)))
        (if (zerop keysym)
            (lookup +keysym-case-insensitive+)
            keysym)))))

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
                    (let ((code (char-code (char-downcase (char name 0)))))
                      (if (< code #x100) code (logior #x1000000 code)))
                    (keysym-from-name name))))
    (when (zerop keysym)
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
  "Run ACTION, a command like (FOCUS :LEFT), on KEYS: a key, or a prefix and a
key like \"Super+Space T\". It replaces the bindings of the same keys, and of
keys it is a prefix of or that are its prefix."
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
  `(set-binding ,keys ',action))

(defun unbind (keys)
  (setf *bindings* (remove (parse-key-sequence keys) *bindings* :key #'car :test #'equal)))
