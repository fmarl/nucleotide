;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:n #:nucleotide)
                    (#:xml #:nucleotide.xml)
                    (#:json #:nucleotide.json)))

(in-package #:nucleotide/tests)

(def-suite :nucleotide)

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun buffer (size)
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defmacro with-stubs ((&rest stubs) &body body)
  (let ((saved (loop repeat (length stubs) collect (gensym))))
    `(let ,(loop for (name) in stubs
                 for var in saved
                 collect `(,var (fdefinition ',name)))
       (unwind-protect
            (progn ,@(loop for (name function) in stubs
                           collect `(setf (fdefinition ',name) ,function))
                   ,@body)
         ,@(loop for (name) in stubs
                 for var in saved
                 collect `(setf (fdefinition ',name) ,var))))))

(defmacro with-fresh-config (&body body)
  `(progv (n::config-variables) (n::default-config)
     ,@body))

(def-fixture fresh-config ()
  (with-fresh-config (&body)))

(defun action (keys)
  (cdr (assoc (n::parse-key-sequence keys) n::*bindings* :test #'equal)))

(defun make-test-wm ()
  (let ((workspaces (let ((n::*num-of-workspaces* 4)) (n::make-workspaces))))
    (make-instance 'n::wm :display nil
                   :orphans workspaces
                   :active-workspace (first workspaces))))

(defun boxes (rects)
  (mapcar (lambda (rect) (and rect (n::rect-list rect))) rects))

(defun ws (wm number)
  (n::nth-workspace wm (1- number)))

(defun add-output (wm x &key name)
  (let ((output (make-instance 'n::output :proxy nil))
        (n::*num-of-workspaces* 4))
    (setf (n::output-rect output) (n::rect x 0 1000 800)
          (n::output-name output) name
          (n::wm-outputs wm) (append (n::wm-outputs wm) (list output)))
    (n::assign-output wm output)
    output))

(defun open-window-on (ws)
  (let ((win (make-instance 'n::window)))
    (n::add-window ws win)
    win))

(defun call-with-environment (variables thunk)
  (let ((previous (mapcar (lambda (variable) (uiop:getenv (first variable))) variables)))
    (unwind-protect
         (progn
           (loop for (name value) in variables
                 do (sb-posix:setenv name value 1))
           (funcall thunk))
      (loop for (name) in variables
            for value in previous
            do (if value
                   (sb-posix:setenv name value 1)
                   (sb-posix:unsetenv name))))))

(defmacro with-environment ((&rest variables) &body body)
  `(call-with-environment (list ,@(mapcar (lambda (variable) `(list ,@variable)) variables))
                          (lambda () ,@body)))

(defun call-with-private-directory (function)
  (let ((directory (sb-posix:mkdtemp "/tmp/nucleotide-test-XXXXXX")))
    (unwind-protect (funcall function directory)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory) :validate t))))

(defmacro with-private-directory ((directory) &body body)
  `(call-with-private-directory (lambda (,directory) ,@body)))
