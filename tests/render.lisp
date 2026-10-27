;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite render :in :nucleotide)
(in-suite render)

(test colors
  (is (equal '(111 140 171 255) (n::parse-color "#6f8cab")))
  (is (equal '(111 140 171 128) (n::parse-color "#6f8cab80")))
  (signals n::nucleotide-error (n::parse-color "6f8cab"))
  (signals n::nucleotide-error (n::parse-color "#zzzzzz")))

(test an-empty-clip-box-disables-clipping
  (is (equal '(0 0 0 0) (n::clip-box-arguments (make-instance 'n::window)))))

(test which-windows-are-visible
  (let* ((wm (make-test-wm))
         (output (add-output wm 0))
         (shown (n::output-workspace output))
         (tiled (open-window-on shown))
         (outside (open-window-on shown))
         (floating (open-window-on shown))
         (elsewhere (open-window-on (ws wm 2))))
    (setf (n::%window-clip outside) :hidden
          (n::%window-floating-p floating) t
          (n::%window-clip floating) :hidden)
    (is (equal '(t nil t nil)
               (mapcar (lambda (win) (n::window-visible-p wm win))
                       (list tiled outside floating elsewhere))))
    (setf (n::wm-highlight wm) elsewhere)
    (is (equal '(nil nil nil t)
               (mapcar (lambda (win) (n::window-visible-p wm win))
                       (list tiled outside floating elsewhere))))))

(test stacking-order
  (let* ((wm (make-test-wm))
         (ws (n::output-workspace (add-output wm 0)))
         (tiled (open-window-on ws))
         (floating (open-window-on ws))
         (fullscreen (open-window-on ws))
         (focused (open-window-on ws)))
    (setf (n::%window-floating-p floating) t
          (n::%window-fullscreen fullscreen) :output)
    (is (equal '(0 2 4 1)
               (mapcar (lambda (win) (n::stacking-rank wm win))
                       (list tiled floating fullscreen focused))))))
