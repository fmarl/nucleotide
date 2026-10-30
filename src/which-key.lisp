;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *which-key-delay* nil
  :documentation "Milliseconds to hold *WHICH-KEY-MODIFIER* before which-key shows, or NIL."
  :check (type-check '(or null (integer 0)) "NIL or milliseconds like 500"))

(define-setting *which-key-modifier* "Super"
  :documentation "The modifiers to hold for which-key."
  :check (lambda (value)
           (unless (plusp (parse-modifiers value))
             (nucleotide-error "~S names no modifier" value))))

(defstruct which-key
  "LEADER and DELAY, in seconds or NIL, are the settings as they were when the
bindings were installed. HELD are the watched modifiers held, SHOWN those shown
or NIL."
  (leader 0)
  delay
  (held 0)
  shown
  timer)

(defun leader-held-p (which-key modifiers)
  (let ((leader (which-key-leader which-key)))
    (= leader (logand leader modifiers))))

(defun watched-modifiers (wm)
  "The leader alone until held, so typing with Shift starts no manage sequences."
  (let ((which-key (wm-which-key wm)))
    (cond ((null (which-key-delay which-key)) 0)
          ((leader-held-p which-key (which-key-held which-key)) +all-modifiers+)
          (t (which-key-leader which-key)))))

(defun manage-modifiers-watch (wm)
  (let ((seat (wm-xkb-seat wm)))
    (when (and (take-requests wm '(:watch-modifiers)) seat (supports-p seat 3))
      (river-xkb-bindings-seat-v1.modifiers-watch seat (watched-modifiers wm)))))

(defun dismiss-which-key (wm)
  "Cancel a pending which-key and hide a shown one."
  (let* ((which-key (wm-which-key wm))
         (timer (shiftf (which-key-timer which-key) nil)))
    (when timer
      (event-loop-cancel (wm-loop wm) timer))
    (when (which-key-shown which-key)
      (setf (which-key-shown which-key) nil)
      (mark-dirty wm))))

(defun show-which-key (wm)
  (let ((which-key (wm-which-key wm)))
    (setf (which-key-timer which-key) nil
          (which-key-shown which-key) (which-key-held which-key))
    (mark-dirty wm)))

(defun start-which-key-delay (wm)
  (dismiss-which-key wm)
  (let ((which-key (wm-which-key wm)))
    (setf (which-key-timer which-key)
          (event-loop-schedule (wm-loop wm) (which-key-delay which-key)
                               (lambda () (show-which-key wm))))))

(defmethod binding-pressed progn ((wm wm))
  (dismiss-which-key wm))

(defmethod bindings-installed progn ((wm wm))
  (dismiss-which-key wm)
  (let ((which-key (wm-which-key wm)))
    (setf (which-key-leader which-key) (parse-modifiers *which-key-modifier*)
          (which-key-delay which-key) (and *which-key-delay* (/ *which-key-delay* 1000))))
  (request wm :watch-modifiers))

(define-handler (wm (seat river-xkb-bindings-seat-v1) :modifiers-update) (old new)
  (let* ((which-key (wm-which-key wm))
         (new (logand new +all-modifiers+))
         (was-held (leader-held-p which-key old))
         (held (leader-held-p which-key new)))
    (setf (which-key-held which-key) new)
    (cond ((or (null (which-key-delay which-key)) (wm-locked wm) (not held))
           (dismiss-which-key wm))
          ((not was-held) (start-which-key-delay wm))
          ((which-key-shown which-key) (setf (which-key-shown which-key) new)))
    (unless (eq was-held held)
      (request wm :watch-modifiers))))
