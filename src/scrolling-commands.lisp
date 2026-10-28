;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun check-presets (value)
  (unless (and (consp value) (every (lambda (preset) (typep preset 'fraction)) value))
    (nucleotide-error "~S is not a list of fractions like (1/3 1/2 2/3)" value)))

(define-setting *column-widths* '(1/3 1/2 2/3)
  :documentation "Fractions of the output width that CYCLE-COLUMN-WIDTH steps through."
  :check #'check-presets)

(define-setting *window-heights* '(1/3 1/2 2/3)
  :documentation "Fractions of the column height that CYCLE-WINDOW-HEIGHT steps through."
  :check #'check-presets)

(defun next-preset (presets fraction)
  (or (find-if (lambda (preset) (> preset fraction)) presets)
      (first presets)))

(defun parse-percent (string)
  "The fraction and the sign, #\\+, #\\- or NIL, of a size like \"50%\" or \"+10%\"."
  (let* ((sign (and (plusp (length string)) (find (char string 0) "+-")))
         (start (if sign 1 0))
         (end (1- (length string))))
    (when (and (< start end)
               (char= #\% (char string end))
               (every #'digit-char-p (subseq string start end)))
      (values (/ (parse-integer string :start start :end end) 100) sign))))

(defun size-changer (change)
  "A function from a fraction to the one CHANGE makes of it."
  (multiple-value-bind (amount sign) (and (stringp change) (parse-percent change))
    (cond ((realp change) (constantly change))
          ((null amount)
           (nucleotide-error "~S is not a fraction or a size like \"50%\" or \"+10%\""
                             change))
          ((eql sign #\+) (lambda (fraction) (+ fraction amount)))
          ((eql sign #\-) (lambda (fraction) (- fraction amount)))
          (t (constantly amount)))))

(defun clamp-fraction (fraction)
  (max 1/10 (min 1 fraction)))

(defun scrolling-focus (wm)
  (let ((layout (wm-active-layout wm))
        (win (workspace-focus (wm-active-workspace wm))))
    (when (and (typep layout 'scrolling)
               win
               (column-containing (scrolling-columns layout) win))
      (values layout win))))

(defun update-columns (wm function)
  (multiple-value-bind (layout win) (scrolling-focus wm)
    (when layout
      (setf (scrolling-columns layout)
            (funcall function (scrolling-columns layout) win))
      (mark-dirty wm))))

(defun update-focused-column (wm function)
  (update-columns wm (lambda (columns win)
                       (let ((column (column-containing columns win)))
                         (substitute (funcall function column) column columns)))))

(defun column-with-height (column win function)
  "COLUMN with the height of WIN set to what FUNCTION makes of the fraction it
shows, or unset when FUNCTION returns NIL."
  (let ((stacked (stacked-windows column)))
    (if (and (rest stacked) (member win stacked))
        (let ((height (funcall function (nth (position win stacked)
                                             (height-fractions column stacked)))))
          (copy-column column :heights (if height
                                           (alist-set win (clamp-fraction height)
                                                      (column-heights column))
                                           (alist-remove win (column-heights column)))))
        column)))

(defun update-focused-height (wm function)
  (update-columns wm (lambda (columns win)
                       (let ((column (column-containing columns win)))
                         (substitute (column-with-height column win function)
                                     column columns)))))

(define-command consume-or-expel (wm direction)
  "Move the focused window out of its column towards DIRECTION, or into the
neighboring column if it is alone."
  (update-columns wm (lambda (columns win)
                       (consume-or-expel-in columns win direction))))

(define-command cycle-column-width (wm)
  "Give the focused column the next width of *COLUMN-WIDTHS*."
  (update-focused-column wm (lambda (column)
                              (copy-column column
                                           :width (next-preset *column-widths*
                                                               (column-width column))))))

(define-command set-column-width (wm change)
  "Change the width of the focused column, as a fraction of the output like 1/2,
or as in niri with \"50%\", \"+10%\" or \"-10%\"."
  (let ((changer (size-changer change)))
    (update-focused-column wm (lambda (column)
                                (copy-column column
                                             :width (clamp-fraction
                                                     (funcall changer
                                                              (column-width column))))))))

(define-command set-window-height (wm change)
  "Change the height of the focused window, as a fraction of its column like
1/2, or as in niri with \"50%\", \"+10%\" or \"-10%\". The other windows of the
column share the rest."
  (update-focused-height wm (size-changer change)))

(add-argument-check 'set-column-width #'size-changer)
(add-argument-check 'set-window-height #'size-changer)

(define-command cycle-window-height (wm)
  "Give the focused window the next height of *WINDOW-HEIGHTS*."
  (update-focused-height wm (lambda (height) (next-preset *window-heights* height))))

(define-command reset-window-height (wm)
  "Let the focused window share the height of its column again."
  (update-focused-height wm (constantly nil)))

(define-command toggle-full-width (wm)
  "Let the focused column fill the output, or give it the default width again."
  (update-focused-column wm (lambda (column)
                              (copy-column column
                                           :width (if (= 1 (column-width column))
                                                      *default-column-width*
                                                      1)))))

(define-command center-column (wm)
  "Scroll the focused column to the middle."
  (let ((layout (scrolling-focus wm)))
    (when layout
      (setf (scrolling-center-p layout) t)
      (mark-dirty wm))))
