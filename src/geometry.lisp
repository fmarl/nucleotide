;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defstruct (rect (:constructor rect (x y width height)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (width 0 :type integer :read-only t)
  (height 0 :type integer :read-only t))

(defstruct (point (:constructor point (x y)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t))

(defstruct (size (:constructor size (width height)))
  (width 0 :type integer :read-only t)
  (height 0 :type integer :read-only t))

(defun point+ (a b)
  (point (+ (point-x a) (point-x b)) (+ (point-y a) (point-y b))))

(defun point- (a b)
  (point (- (point-x a) (point-x b)) (- (point-y a) (point-y b))))

(defun rect-at (origin size)
  (rect (point-x origin) (point-y origin) (size-width size) (size-height size)))

(defun rect-origin (rect)
  (point (rect-x rect) (rect-y rect)))

(defun rect-size (rect)
  (size (rect-width rect) (rect-height rect)))

(defun rect-list (rect)
  (list (rect-x rect) (rect-y rect) (rect-width rect) (rect-height rect)))

(defun rect-right (rect)
  (+ (rect-x rect) (rect-width rect)))

(defun rect-bottom (rect)
  (+ (rect-y rect) (rect-height rect)))

(defun rect-center (rect)
  (point (+ (rect-x rect) (floor (rect-width rect) 2))
         (+ (rect-y rect) (floor (rect-height rect) 2))))

(defun centered-offset (inner outer)
  "The offset that centers the size INNER in the size OUTER."
  (point (floor (- (size-width outer) (size-width inner)) 2)
         (floor (- (size-height outer) (size-height inner)) 2)))

(defun inset (rect gap)
  (rect (+ (rect-x rect) gap) (+ (rect-y rect) gap)
        (- (rect-width rect) (* 2 gap)) (- (rect-height rect) (* 2 gap))))

(defun rect-within-p (inner outer)
  (and (<= (rect-x outer) (rect-x inner))
       (<= (rect-y outer) (rect-y inner))
       (<= (rect-right inner) (rect-right outer))
       (<= (rect-bottom inner) (rect-bottom outer))))

(defun overlap-area (a b)
  (flet ((span (start end other-start other-end)
           (max 0 (- (min end other-end) (max start other-start)))))
    (* (span (rect-x a) (rect-right a) (rect-x b) (rect-right b))
       (span (rect-y a) (rect-bottom a) (rect-y b) (rect-bottom b)))))
