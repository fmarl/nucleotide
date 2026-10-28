;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-command toggle-highlight (wm &rest match)
  "Show the most recently focused window that matches MATCH, a plist like the
one of RULE, alone on the focused output. Run it again to stop."
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
    (when (and win output)
      (case (window-placement win)
        (:tiled (place-window win (inset (usable-area output) *gap*) nil))
        (:maximized (place-window win (usable-area output) nil))))))
