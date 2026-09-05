;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *modifier-names*
  `((:shift . ,+river-seat-v1-modifiers-shift+)
    (:ctrl . ,+river-seat-v1-modifiers-ctrl+)
    (:alt . ,+river-seat-v1-modifiers-mod1+)
    (:mod3 . ,+river-seat-v1-modifiers-mod3+)
    (:super . ,+river-seat-v1-modifiers-mod4+)
    (:mod5 . ,+river-seat-v1-modifiers-mod5+)))

(defparameter *keysym-names*
  '((:space . #x0020) (:return . #xff0d) (:tab . #xff09) (:escape . #xff1b)
    (:backspace . #xff08) (:delete . #xffff) (:print . #xff61)
    (:home . #xff50) (:end . #xff57) (:page-up . #xff55) (:page-down . #xff56)
    (:left . #xff51) (:up . #xff52) (:right . #xff53) (:down . #xff54)
    (:f1 . #xffbe) (:f2 . #xffbf) (:f3 . #xffc0) (:f4 . #xffc1)
    (:f5 . #xffc2) (:f6 . #xffc3) (:f7 . #xffc4) (:f8 . #xffc5)
    (:f9 . #xffc6) (:f10 . #xffc7) (:f11 . #xffc8) (:f12 . #xffc9)))

(defun parse-modifiers (spec)
  "SPEC is NIL, a modifier keyword like :SUPER, a list of them, or a bitmask."
  (etypecase spec
    (integer spec)
    (keyword (or (cdr (assoc spec *modifier-names*))
                 (nucleotide-error "unknown modifier ~S" spec)))
    (list (reduce #'logior (mapcar #'parse-modifiers spec) :initial-value 0))))

(defun parse-keysym (key)
  "KEY is a character, a key keyword like :RETURN, or a keysym."
  (etypecase key
    (integer key)
    (character
     ;; xkb keysyms equal the code point for Latin-1; everything else
     ;; lives in the Unicode keysym range.
     (let ((code (char-code key)))
       (if (< code #x100) code (logior #x1000000 code))))
    (keyword (or (cdr (assoc key *keysym-names*))
                 (nucleotide-error "unknown key ~S" key)))))

(defun key-id (modifiers key)
  (cons (parse-keysym key) (parse-modifiers modifiers)))

(defvar *keybinds* '()
  "Alist from (keysym . modifiers) to a function of the WM.")

(defvar *key-chords* '()
  "Alist from the prefix's (keysym . modifiers) to an alist like *KEYBINDS*.")

(defun set-keybind (modifiers key function)
  "Run FUNCTION with the WM on KEY with MODIFIERS, replacing what was there."
  (let ((id (key-id modifiers key)))
    (setf *key-chords* (alist-remove id *key-chords*)
          *keybinds* (alist-set id function *keybinds*))
    id))

(defun remove-keybind (modifiers key)
  (setf *keybinds* (alist-remove (key-id modifiers key) *keybinds*)))

(defun set-key-chord (prefix chord function)
  "Run FUNCTION with the WM when CHORD follows PREFIX, both (MODIFIERS KEY)."
  (let ((prefix-id (apply #'key-id prefix)))
    (setf *keybinds* (alist-remove prefix-id *keybinds*)
          *key-chords* (alist-set prefix-id
                                  (alist-set (apply #'key-id chord) function
                                             (alist-get prefix-id *key-chords*))
                                  *key-chords*))))

(defun remove-key-chord (prefix &optional chord)
  "Remove CHORD from PREFIX's submap, or the whole submap without CHORD."
  (let ((prefix-id (apply #'key-id prefix)))
    (setf *key-chords*
          (if (and chord (assoc prefix-id *key-chords* :test #'equal))
              (alist-set prefix-id
                         (alist-remove (apply #'key-id chord)
                                       (alist-get prefix-id *key-chords*))
                         *key-chords*)
              (alist-remove prefix-id *key-chords*)))))

(defun clear-keybinds ()
  (setf *keybinds* '()
        *key-chords* '()))

(defmacro define-keybinds ((wm) &body bindings)
  "Add keybinds. Each binding is (MODIFIERS KEY &BODY BODY); BODY runs with
WM bound to the window manager."
  `(progn
     ,@(loop for (modifiers key . body) in bindings
             collect `(set-keybind ',modifiers ',key
                                   (lambda (,wm)
                                     (declare (ignorable ,wm))
                                     ,@body)))))

(defmacro define-key-chords ((wm) &body prefixes)
  "Add key chords. Each prefix is ((MODIFIERS KEY) &BODY CHORDS), each chord
(KEY &BODY BODY) or ((MODIFIERS KEY) &BODY BODY)."
  `(progn
     ,@(loop for (prefix . chords) in prefixes
             append (loop for (chord . body) in chords
                          collect `(set-key-chord
                                    ',prefix
                                    ',(if (consp chord) chord (list nil chord))
                                    (lambda (,wm)
                                      (declare (ignorable ,wm))
                                      ,@body))))))

(defun define-workspace-keybinds (switch-modifiers send-modifiers)
  "Bind 1-9 with SWITCH-MODIFIERS to switch workspaces, with SEND-MODIFIERS to
send the focused window there."
  (dotimes (i 9)
    (let ((key (digit-char (1+ i)))
          (i i))
      (set-keybind switch-modifiers key
                   (lambda (wm) (switch-workspace wm (nth-workspace wm i))))
      (set-keybind send-modifiers key
                   (lambda (wm) (send-to-workspace wm (nth-workspace wm i)))))))
