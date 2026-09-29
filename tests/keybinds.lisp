;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite keybinds :in :nucleotide)
(in-suite keybinds)

(defmacro with-empty-keybinds (&body body)
  `(let ((n::*keybinds* '())
         (n::*key-chords* '()))
     ,@body))

(test modifiers
  (is (= 0 (n::parse-modifiers nil)))
  (is (= 64 (n::parse-modifiers :super)))
  (is (= 65 (n::parse-modifiers '(:super :shift))))
  (signals n:nucleotide-error (n::parse-modifiers :hyper)))

(test keysyms
  (is (= #x61 (n::parse-keysym #\a)))
  (is (= #xff0d (n::parse-keysym :return)))
  (is (= #x10020ac (n::parse-keysym (code-char #x20ac))))
  (is (= 42 (n::parse-keysym 42))))

(test define-keybinds-adds-and-replaces
  (with-empty-keybinds
    (n:define-keybinds (wm)
      (:super #\a :first)
      ((:super :shift) #\a :second))
    (n:define-keybinds (wm)
      (:super #\a :replaced))
    (is (= 2 (length n::*keybinds*)))
    (is (eq :replaced
            (funcall (cdr (assoc (n::key-id :super #\a) n::*keybinds*
                                 :test #'equal))
                     nil)))))

(test keybind-bodies-see-the-wm
  (with-empty-keybinds
    (n:define-keybinds (the-wm)
      (:super #\a the-wm))
    (is (eq :wm (funcall (cdar n::*keybinds*) :wm)))))

(test chords-and-keybinds-exclude-each-other
  (with-empty-keybinds
    (n:define-keybinds (wm)
      (:super :space :plain))
    (n:define-key-chords (wm)
      ((:super :space)
       (#\t :t)
       ((:shift #\t) :shift-t)))
    (is (null n::*keybinds*))
    (is (= 2 (length (cdar n::*key-chords*))))
    (n:define-keybinds (wm)
      (:super :space :plain))
    (is (null n::*key-chords*))))

(test remove-keybinds
  (with-empty-keybinds
    (n:define-keybinds (wm) (:super #\a nil))
    (n:remove-keybind :super #\a)
    (is (null n::*keybinds*))
    (n:define-key-chords (wm) ((:super :space) (#\t nil) (#\m nil)))
    (n:remove-key-chord '(:super :space) '(nil #\t))
    (is (= 1 (length (cdar n::*key-chords*))))
    (n:remove-key-chord '(:super :space))
    (is (null n::*key-chords*))))
