;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite layouts :in :nucleotide)
(in-suite layouts)

(test tiling-fits-and-does-not-overlap
  (loop with area = (n::rect 10 20 1000 800)
        for n from 0 to 6
        for rects = (n::tiling-rects n area 3/5 8)
        do (is (= n (length rects)))
        (dolist (rect rects)
          (is (n::rect-within-p rect area)))
        (loop for (a . rest) on rects
              do (dolist (b rest)
                   (is (zerop (n::overlap-area a b)))))))

(test tiling-stack-fills-the-height
  (let ((rects (n::tiling-rects 4 (n::rect 0 0 1000 800) 3/5 8)))
    (is (= (- 800 8) (n::rect-bottom (car (last rects)))))))

(test single-window-gets-everything-but-the-gaps
  (is (equal '((8 8 984 784)) (boxes (n::tiling-rects 1 (n::rect 0 0 1000 800) 3/5 8)))))

(test monocle-stacks-identical-rects
  (let ((rects (boxes (n::monocle-rects 3 (n::rect 0 0 1000 800) 8))))
    (is (equal (make-list 3 :initial-element (list 8 8 984 784)) rects))))

(test tiling-uses-its-ratio-and-the-gap
  (let ((n::*gap* 8))
    (is (equal '((8 8 492 784) (508 8 484 784))
               (boxes (n::layout-arrange (n::make-layout '(n::tiling :ratio 1/2)) nil '(a b)
                                         (n::rect 0 0 1000 800)))))))

(defun workspace-with (&rest windows)
  (let ((ws (make-instance 'n::workspace :name "test")))
    (setf (n::%workspace-windows ws) windows)
    ws))

(test neighbors-wrap-around-in-tiling
  (let* ((layout (n::make-layout 'n::tiling))
         (ws (workspace-with 'a 'b 'c)))
    (is (eq 'b (n::layout-neighbor layout ws 'a :next)))
    (is (eq 'a (n::layout-neighbor layout ws 'c :right)))
    (is (eq 'c (n::layout-neighbor layout ws 'a :up)))
    (is (null (n::layout-neighbor layout (workspace-with 'a) 'a :next)))))

(test moving-swaps-with-the-neighbor
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (c (open-window-on ws))
         (b (open-window-on ws))
         (a (open-window-on ws)))
    (n::layout-move (n::make-layout 'n::monocle) ws c :next)
    (is (equal (list c b a) (n::workspace-windows ws)))))

(test the-successor-is-another-window
  (let ((layout (n::make-layout 'n::tiling)))
    (is (eq 'b (n::layout-successor layout (workspace-with 'a 'b) 'a)))
    (is (null (n::layout-successor layout (workspace-with 'a) 'a)))))

(test moving-passes-floating-windows-by
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (c (open-window-on ws))
         (dialog (open-window-on ws))
         (a (open-window-on ws)))
    (setf (n::%window-floating-p dialog) t)
    (n::layout-move (n::make-layout 'n::tiling) ws a :next)
    (is (equal (list c dialog a) (n::workspace-windows ws)))
    (n::layout-move (n::make-layout 'n::tiling) ws dialog :next)
    (is (equal (list c dialog a) (n::workspace-windows ws)))))

(defclass reversed-test-layout (n::layout)
  ())

(defmethod n::layout-neighbor ((layout reversed-test-layout) ws win direction)
  (n::cyclic-neighbor win (reverse (n::workspace-windows ws)) direction))

(test moving-follows-the-neighbors-of-the-layout
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (c (open-window-on ws))
         (b (open-window-on ws))
         (a (open-window-on ws))
         (layout (make-instance 'reversed-test-layout)))
    (is (eq c (n::layout-neighbor layout ws a :next)))
    (n::layout-move layout ws a :next)
    (is (equal (list c b a) (n::workspace-windows ws)))))

(test layouts-are-made-from-checked-specs
  (is (typep (n::make-layout 'n::monocle) 'n::monocle))
  (is (= 1/2 (n::tiling-ratio (n::make-layout '(n::tiling :ratio 1/2)))))
  (dolist (spec '((n::tiling :ratio 0) (n::tiling :ratio 1) (n::tiling :ratio "1/2")
                  (n::tiling :size 2) (n::tiling :ratio) (n::tiling . 3) n::wm no-such-layout))
    (signals n::nucleotide-error (n::make-layout spec) "~S was made" spec))
  (is (typep 'n::tiling 'n::layout-spec))
  (is (not (typep '(n::tiling :ratio 9) 'n::layout-spec))))
