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
        for rects = (n::tiling-rects n 10 20 1000 800 3/5 8)
        do (is (= n (length rects)))
        (dolist (rect rects)
          (is (inside-p rect 10 20 1000 800)))
        (loop for (a . rest) on rects
              do (dolist (b rest)
                   (is (not (overlap-p a b)))))))

(test tiling-stack-fills-the-height
  (let* ((rects (n::tiling-rects 4 0 0 1000 800 3/5 8))
         (last (car (last rects))))
    (is (= (- 800 8) (+ (second last) (fourth last))))))

(test single-window-gets-everything-but-the-gaps
  (is (equal '((8 8 984 784)) (n::tiling-rects 1 0 0 1000 800 3/5 8))))

(test monocle-stacks-identical-rects
  (let ((rects (n::monocle-rects 3 0 0 1000 800 8)))
    (is (equal (make-list 3 :initial-element (list 8 8 984 784)) rects))))

(test layouts-from-specs
  (is (typep (n::make-layout 'n::monocle) 'n::monocle))
  (is (= 1/2 (n::tiling-ratio (n::make-layout '(n::tiling :ratio 1/2)))))
  (signals n::nucleotide-error (n::make-layout 'n::wm))
  (signals n::nucleotide-error (n::make-layout 'no-such-layout)))

(test tiling-uses-its-ratio-and-the-gap
  (let ((n::*gap* 8))
    (is (equal '((8 8 492 784) (508 8 484 784))
               (n::layout-arrange (n::make-layout '(n::tiling :ratio 1/2)) nil '(a b)
                                  0 0 1000 800)))))

(defun workspace-with (&rest windows)
  (let ((ws (make-instance 'n::workspace :name "test")))
    (setf (n::workspace-windows ws) windows)
    ws))

(test neighbors-wrap-around-in-tiling
  (let* ((layout (n::make-layout 'n::tiling))
         (ws (workspace-with 'a 'b 'c)))
    (is (eq 'b (n::layout-neighbor layout ws 'a :next)))
    (is (eq 'a (n::layout-neighbor layout ws 'c :right)))
    (is (eq 'c (n::layout-neighbor layout ws 'a :up)))
    (is (null (n::layout-neighbor layout (workspace-with 'a) 'a :next)))))

(test moving-swaps-with-the-neighbor
  (let* ((layout (n::make-layout 'n::monocle))
         (ws (workspace-with 'a 'b 'c)))
    (n::layout-move layout ws 'c :next)
    (is (equal '(c b a) (n::workspace-windows ws)))))

(test the-successor-is-another-window
  (let ((layout (n::make-layout 'n::tiling)))
    (is (eq 'b (n::layout-successor layout (workspace-with 'a 'b) 'a)))
    (is (null (n::layout-successor layout (workspace-with 'a) 'a)))))
