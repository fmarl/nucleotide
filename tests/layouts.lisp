;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite layouts :in :nucleotide)
(in-suite layouts)

(defun inside-p (rect x y width height)
  (destructuring-bind (rx ry rw rh) rect
    (and (<= x rx) (<= y ry)
         (<= (+ rx rw) (+ x width))
         (<= (+ ry rh) (+ y height)))))

(defun overlap-p (a b)
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (and (< ax (+ bx bw)) (< bx (+ ax aw))
           (< ay (+ by bh)) (< by (+ ay ah))))))

(test tiling-fits-and-does-not-overlap
  (loop for n from 0 to 6
        for rects = (n:tiling n 10 20 1000 800)
        do (is (= n (length rects)))
        (dolist (rect rects)
          (is (inside-p rect 10 20 1000 800)))
        (loop for (a . rest) on rects
              do (dolist (b rest)
                   (is (not (overlap-p a b)))))))

(test tiling-stack-fills-the-height
  (let* ((rects (n:tiling 4 0 0 1000 800 :gap 8))
         (last (car (last rects))))
    (is (= (- 800 8) (+ (second last) (fourth last))))))

(test single-window-gets-everything-but-the-gaps
  (is (equal '((8 8 984 784)) (n:tiling 1 0 0 1000 800 :gap 8))))

(test monocle-stacks-identical-rects
  (let ((rects (n:monocle 3 0 0 1000 800)))
    (is (= 3 (length rects)))
    (is (= 1 (length (remove-duplicates rects :test #'equal))))))
