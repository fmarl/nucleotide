;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun parse-color (color-code)
  (unless (and (stringp color-code)
               (member (length color-code) '(7 9))
               (char= #\# (char color-code 0)))
    (nucleotide-error "~S is not a color like \"#6f8cab\"" color-code))
  (append (loop for start from 1 below (length color-code) by 2
                collect (parse-integer color-code :start start :end (+ start 2) :radix 16))
          (and (= 7 (length color-code)) '(255))))

(define-setting *border-width* 2)

(define-setting *border-focused* "#6f8cab"
  :check #'parse-color)

(define-setting *border-unfocused* "#ababab"
  :check #'parse-color)

(defun rgba->uint32 (rgba)
  (mapcar (lambda (x)
            (round (* x (/ #xffffffff 255))))
          rgba))

(defun window-visible-p (wm win)
  (let ((ws (window-workspace win))
        (highlight (wm-highlight wm)))
    (cond ((eq win highlight) t)
          ((not (workspace-shown-p ws)) nil)
          ((and highlight (eq ws (wm-active-workspace wm))) nil)
          ((or (window-fullscreen win) (window-floating win)) t)
          (t (not (eq (window-clip win) :hidden))))))

(defun stacking-rank (wm win)
  (+ (cond ((window-fullscreen win) 4)
           ((window-floating win) 2)
           (t 0))
     (if (eq win (wm-focus wm)) 1 0)))

(defun clip-request (win)
  "The set_clip_box arguments for WIN; an empty box disables clipping."
  (if (consp (window-clip win)) (window-clip win) '(0 0 0 0)))

(defun border-color (wm win)
  (rgba->uint32 (parse-color (if (eq (wm-focus wm) win)
                                 *border-focused*
                                 *border-unfocused*))))

(defun render-show (wm win)
  (when (and (window-floating win) (plusp (window-width win)))
    (place-floating wm win))
  (river-window-v1.show (window-proxy win))
  (apply #'river-window-v1.set-borders (window-proxy win) +all-edges+ *border-width*
         (border-color wm win))
  (when (supports-p (window-proxy win) 2)
    (apply #'river-window-v1.set-clip-box (window-proxy win) (clip-request win)))
  (river-node-v1.set-position (window-node win) (window-x win) (window-y win))
  (river-node-v1.place-top (window-node win)))

(defun render (wm)
  (let* ((windows (all-windows wm))
         (visible (remove-if-not (lambda (win) (window-visible-p wm win)) windows)))
    (dolist (win (set-difference windows visible))
      (river-window-v1.hide (window-proxy win)))
    (dolist (win (stable-sort (reverse visible) #'<
                              :key (lambda (win) (stacking-rank wm win))))
      (render-show wm win))))
