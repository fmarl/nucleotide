;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *window-rules* '()
  "List of (MATCH . ACTIONS) plists, applied in order to every new window.")

(defun add-window-rule (match actions)
  (setf *window-rules*
        (append (remove match *window-rules* :key #'car :test #'equal)
                (list (cons match actions)))))

(defmacro define-window-rule ((&rest match) &rest actions)
  "Apply ACTIONS to new windows whose properties match MATCH, a plist of
:APP-ID and :TITLE patterns: a string to compare with, or a predicate.
ACTIONS: :FLOAT T or NIL, :WORKSPACE number, :OUTPUT name, :FULLSCREEN T."
  `(add-window-rule (list ,@match) (list ,@actions)))

(defun contains (substring)
  (lambda (string) (search substring string)))

(defun pattern-matches-p (pattern value)
  (etypecase pattern
    (string (equal pattern value))
    (function (and value (funcall pattern value)))))

(defun rule-matches-p (match win)
  (loop for (key pattern) on match by #'cddr
        always (pattern-matches-p pattern (ecase key
                                            (:app-id (window-app-id win))
                                            (:title (window-title win))))))

(defun window-actions (win)
  "The actions of all matching rules; later rules come first and so win."
  (reduce (lambda (actions rule)
            (if (rule-matches-p (car rule) win)
                (append (cdr rule) actions)
                actions))
          *window-rules*
          :initial-value '()))

(defun rule-workspace (wm actions)
  (let ((number (getf actions :workspace))
        (output-name (getf actions :output)))
    (cond (number (nth-workspace wm (1- number)))
          (output-name
           (let ((output (find output-name (wm-outputs wm)
                               :key #'output-name :test #'equal)))
             (and output (output-workspace output)))))))

(defun apply-float-rule (win actions)
  (multiple-value-bind (present float) (get-properties actions '(:float))
    (when present
      (setf (window-floating win) (and float t)))))

(defun apply-workspace-rule (wm win actions)
  (let ((ws (rule-workspace wm actions)))
    (when (and ws (not (eq ws (window-workspace win))))
      (move-to-workspace win ws))))

(defun apply-fullscreen-rule (wm win actions)
  (when (getf actions :fullscreen)
    (push (cons win nil) (wm-pending-fullscreens wm))))

(defun apply-window-rules (wm win)
  (let ((actions (window-actions win)))
    (apply-float-rule win actions)
    (apply-workspace-rule wm win actions)
    (apply-fullscreen-rule wm win actions)))
