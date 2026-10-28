;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *menu-command* '("bemenu" "-i" "-l" "20" "-p" "window")
  :documentation "A dmenu-like program: reads choices on stdin, prints the chosen one."
  :check (type-check 'argv))

(defun window-label (win number)
  (let* ((ws (window-workspace win))
         (output (workspace-output ws)))
    (format nil "~D: [~@[~A:~]~A] ~@[~A~]~@[ - ~A~]"
            number (and output (output-name output)) (workspace-name ws)
            (window-app-id win) (window-title win))))

(defun menu-choose (choices)
  "Run *MENU-COMMAND* on CHOICES and return the index of the chosen one, or NIL."
  (let ((output (handler-case
                    (uiop:run-program *menu-command*
                                      :input (make-string-input-stream
                                              (format nil "~{~A~%~}" choices))
                                      :output '(:string :stripped t)
                                      :ignore-error-status t)
                  (error (c)
                    (notify "~A failed: ~A" (first *menu-command*) c)
                    nil))))
    (let ((number (and output (parse-integer output :junk-allowed t))))
      (and number (<= 1 number (length choices)) (1- number)))))

(defun focus-from-switcher (wm win)
  (when (live win)
    (let ((previous (wm-focused-output wm)))
      (end-highlight wm)
      (focus-window wm win)
      (follow-focus wm previous))))

(define-command switch-window (wm)
  "Choose a window with *MENU-COMMAND* and focus it."
  (let* ((windows (all-windows wm))
         (choices (loop for win in windows
                        for number from 1
                        collect (window-label win number))))
    (when windows
      (sb-thread:make-thread
       (lambda ()
         (let ((index (menu-choose choices)))
           (when index
             (event-loop-post (wm-loop wm)
                              (lambda () (focus-from-switcher wm (nth index windows)))))))
       :name "nucleotide window switcher"))))
