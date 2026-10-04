;;; fmt.el --- indent Common Lisp files like SLY does  -*- lexical-binding: t -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

;; Usage: emacs --batch -Q -l scripts/fmt.el FILE...
;; With FMT_CHECK set, files are only checked, not changed.

(require 'cl-indent)

(defvar fmt-max-line-length 100)

;; What SLY derives from the &body parameters of these macros.
(dolist (spec '((defsystem . (4 &body))
                (define-keybinds . (4 &body))
                (define-key-chords . (4 &body))
                (define-handler . (4 4 &body))
                (define-request . (4 &body))
                (define-event . (4 &body))
                (define-interface . (4 4 4))
                (test . (4 &body))
                (with-empty-keybinds . (&body))))
  (put (car spec) 'common-lisp-indent-function (cdr spec)))

(defun fmt-format-buffer ()
  (lisp-mode)
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  (setq-local indent-tabs-mode nil)
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max)))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun fmt-long-lines (file)
  (goto-char (point-min))
  (let ((problems '()))
    (while (not (eobp))
      (when (> (- (line-end-position) (line-beginning-position))
               fmt-max-line-length)
        (push (format "%s:%d: line longer than %d characters"
                      file (line-number-at-pos) fmt-max-line-length)
              problems))
      (forward-line 1))
    (nreverse problems)))

(let ((check-only (getenv "FMT_CHECK"))
      (failed nil))
  (dolist (file command-line-args-left)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((original (buffer-string)))
        (fmt-format-buffer)
        (unless (equal original (buffer-string))
          (if check-only
              (progn (message "%s: not formatted, run make fmt" file)
                     (setq failed t))
            (write-region nil nil file nil 'silent)
            (message "formatted %s" file)))
        (dolist (problem (fmt-long-lines file))
          (message "%s" problem)
          (setq failed t)))))
  (setq command-line-args-left nil)
  (kill-emacs (if failed 1 0)))
