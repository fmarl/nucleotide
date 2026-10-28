;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun pattern-p (object)
  (or (stringp object) (functionp object) (and object (symbolp object))))

(defun match-p (object)
  (and (plist-p object)
       (loop for (key pattern) on object by #'cddr
             always (and (member key '(:app-id :title)) (pattern-p pattern)))))

(define-value-type match "a plist of :app-id and :title patterns like (:app-id \"foot\")"
  :type (satisfies match-p))

(define-value-type output-name "an output name like \"DP-1\""
  :type string)

(defparameter *rule-actions*
  '((:float . t) (:fullscreen . t) (:workspace . workspace-number) (:output . output-name)))

(defun action-type (action)
  (or (alist-get action *rule-actions*)
      (nucleotide-error "unknown rule action ~S, expected one of ~S"
                        action (mapcar #'car *rule-actions*))))

(defun check-rule (match actions)
  (check-value match 'match)
  (unless (plist-p actions)
    (nucleotide-error "~S is not a plist of actions like (:float t)" actions))
  (loop for (action value) on actions by #'cddr
        do (check-value value (action-type action))))

(defvar *window-rules* '()
  "List of (MATCH . ACTIONS) plists, applied in order to every new window.")

(defun add-window-rule (match actions)
  (check-rule match actions)
  (setf *window-rules*
        (append (remove match *window-rules* :key #'car :test #'equal)
                (list (cons match actions)))))

(defmacro rule ((&rest match) &rest actions)
  "Apply ACTIONS to new windows whose properties match MATCH, a plist of
:APP-ID and :TITLE patterns: a string to compare with, or a predicate. A
predicate that fails matches nothing. ACTIONS: :FLOAT T or NIL, :WORKSPACE
number, :OUTPUT name, :FULLSCREEN T."
  `(add-window-rule (list ,@match) (list ,@actions)))

(defun contains (substring)
  "A pattern for RULE that matches strings containing SUBSTRING."
  (lambda (string) (search substring string)))

(defun pattern-matches-p (pattern value)
  (if (stringp pattern)
      (equal pattern value)
      (and value (call-reporting-failure pattern pattern (list value) "a window rule pattern"))))

(defun rule-matches-p (match win)
  (loop for (key pattern) on match by #'cddr
        always (pattern-matches-p pattern (ecase key
                                            (:app-id (window-app-id win))
                                            (:title (window-title win))))))

(defun window-actions (win)
  "The actions of all matching rules. Later rules come first and so win."
  (reduce (lambda (actions rule)
            (if (rule-matches-p (car rule) win)
                (append (cdr rule) actions)
                actions))
          *window-rules*
          :initial-value '()))

(defun rule-workspace (wm actions output)
  "The workspace ACTIONS send a window to, counting numbered ones on the output
they name or else on OUTPUT."
  (let ((named (let ((name (getf actions :output)))
                 (and name (find name (wm-outputs wm) :key #'output-name :test #'equal))))
        (number (getf actions :workspace)))
    (cond (number (nth-workspace wm (1- number) (or named output)))
          (named (output-workspace named)))))

(defun apply-float-rule (win actions)
  (multiple-value-bind (present float) (get-properties actions '(:float))
    (float-new-window win (if present
                              (and float t)
                              (and (auto-float-p win) t)))))

(defun apply-fullscreen-rule (wm win actions)
  (when (getf actions :fullscreen)
    (request wm :fullscreen win nil)))

(defun apply-window-rules (wm win)
  "PLACE-NEW-WINDOW applies the workspace rules."
  (let ((actions (window-actions win)))
    (apply-float-rule win actions)
    (apply-fullscreen-rule wm win actions)))
