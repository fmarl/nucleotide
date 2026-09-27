;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite scrolling :in :nucleotide)
(in-suite scrolling)

;;; An output of 1016x816 with gaps of 8 leaves a view of 1000x800, so a
;;; half-width column is 496 pixels wide and two of them fill the view.

(defun make-scrolling-workspace ()
  (let ((ws (make-instance 'n:workspace :name "test")))
    (setf (n:workspace-layout ws) (make-instance 'n:scrolling))
    ws))

(defun open-window (ws)
  (let ((win (make-instance 'n:window :workspace ws)))
    (push win (n:workspace-windows ws))
    (setf (n::workspace-focused ws) win)
    (arrange ws)
    win))

(defun arrange (ws &optional (tiled (n:workspace-windows ws)))
  (n:layout-arrange (n:workspace-layout ws) ws tiled 0 0 1016 816))

(defun focus (ws win)
  (setf (n::workspace-focused ws) win)
  (arrange ws))

(defun columns (ws)
  (mapcar #'n::column-windows (n::scrolling-columns (n:workspace-layout ws))))

(defun rect (ws win)
  (nth (position win (n:workspace-windows ws)) (arrange ws)))

(test new-windows-open-right-of-the-focused-column
  (let* ((ws (make-scrolling-workspace))
         (a (open-window ws))
         (b (open-window ws)))
    (is (equal (list (list a) (list b)) (columns ws)))
    (focus ws a)
    (let ((c (open-window ws)))
      (is (equal (list (list a) (list c) (list b)) (columns ws))))))

(test half-width-columns-fill-the-view
  (let* ((ws (make-scrolling-workspace))
         (a (open-window ws))
         (b (open-window ws)))
    (is (equal '(8 8 496 800) (rect ws a)))
    (is (equal '(512 8 496 800) (rect ws b)))))

(test view-scrolls-just-enough
  (let* ((ws (make-scrolling-workspace))
         (a (open-window ws))
         (b (open-window ws))
         (c (open-window ws)))
    (is (equal '(-496 8 496 800) (rect ws a)))
    (is (equal '(512 8 496 800) (rect ws c)))
    (focus ws b)
    (is (equal '(8 8 496 800) (rect ws b)))
    (focus ws a)
    (is (equal '(8 8 496 800) (rect ws a)))))

(test focus-and-move-between-columns
  (let* ((ws (make-scrolling-workspace))
         (layout (n:workspace-layout ws))
         (a (open-window ws))
         (b (open-window ws)))
    (is (eq a (n:layout-neighbor layout ws b :prev)))
    (is (null (n:layout-neighbor layout ws b :next)))
    (n:layout-move layout ws b :prev)
    (is (equal (list (list b) (list a)) (columns ws)))))

(test consume-and-expel
  (let* ((ws (make-scrolling-workspace))
         (layout (n:workspace-layout ws))
         (a (open-window ws))
         (b (open-window ws)))
    (setf (n::scrolling-columns layout)
          (n::consume-or-expel (n::scrolling-columns layout) b :prev))
    (is (equal (list (list a b)) (columns ws)))
    (is (eq a (n:layout-neighbor layout ws b :up)))
    (is (equal '((8 8 496 396) (8 412 496 396))
               (list (rect ws a) (rect ws b))))
    (setf (n::scrolling-columns layout)
          (n::consume-or-expel (n::scrolling-columns layout) b :next))
    (is (equal (list (list a) (list b)) (columns ws)))))

(test successor-prefers-the-same-column
  (let* ((ws (make-scrolling-workspace))
         (layout (n:workspace-layout ws))
         (a (open-window ws))
         (b (open-window ws))
         (c (open-window ws)))
    (declare (ignore a))
    (setf (n::scrolling-columns layout)
          (n::consume-or-expel (n::scrolling-columns layout) c :prev))
    (is (eq b (n:layout-successor layout ws c)))
    (is (eq c (n:layout-successor layout ws b)))))

(test successor-falls-back-to-the-left-column
  (let* ((ws (make-scrolling-workspace))
         (layout (n:workspace-layout ws))
         (a (open-window ws))
         (b (open-window ws)))
    (is (eq a (n:layout-successor layout ws b)))))

(test column-widths
  (let ((n:*column-widths* '(1/3 1/2 2/3)))
    (is (= 2/3 (n::next-column-width 1/2)))
    (is (= 1/3 (n::next-column-width 2/3)))
    (is (= 1/3 (n::next-column-width 1)))))

(test full-width-column
  (let* ((ws (make-scrolling-workspace))
         (layout (n:workspace-layout ws))
         (a (open-window ws)))
    (setf (n::scrolling-columns layout)
          (list (n::make-column (list a) 1)))
    (is (equal '(8 8 1000 800) (rect ws a)))))

(test fullscreen-windows-keep-their-column
  (let* ((ws (make-scrolling-workspace))
         (a (open-window ws))
         (b (open-window ws)))
    (is (equal '((512 8 496 800)) (arrange ws (list b))))
    (is (equal (list (list a) (list b)) (columns ws)))))

(test closed-windows-leave-their-column
  (let* ((ws (make-scrolling-workspace))
         (a (open-window ws))
         (b (open-window ws)))
    (n::remove-window ws b)
    (is (eq a (n::workspace-focused ws)))
    (arrange ws)
    (is (equal (list (list a)) (columns ws)))))
