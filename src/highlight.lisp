;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-command toggle-highlight (wm &rest match)
  "Show the most recently focused window that matches MATCH, a plist like the
one of RULE, alone on the focused output; again to stop."
  (setf (wm-highlight wm)
        (unless (wm-highlight wm)
          (find-if (lambda (win) (rule-matches-p match win))
                   (remove-duplicates (append (wm-focus-history wm) (all-windows wm))
                                      :from-end t))))
  (mark-dirty wm))

(defun end-highlight (wm)
  (when (wm-highlight wm)
    (setf (wm-highlight wm) nil)
    (mark-dirty wm)))

(defun manage-highlight (wm)
  (let ((win (wm-highlight wm))
        (output (wm-focused-output wm)))
    (when (and win output (not (window-floating win)) (not (window-fullscreen win)))
      (multiple-value-bind (x y width height) (usable-area output)
        (place-window win (first (monocle-rects 1 x y width height *gap*)) nil)))))
