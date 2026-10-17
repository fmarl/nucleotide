;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:n #:nucleotide)
                    (#:xml #:nucleotide.xml)))

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
  `(let ((n::*commands* (copy-list n::*commands*))
         (n::*bindings* '())
         (n::*window-rules* '())
         (n::*hooks* '())
         (n::*settings* n::*settings*))
     (progv (mapcar #'car n::*settings*)
         (mapcar (lambda (entry) (funcall (getf (cdr entry) :default))) n::*settings*)
       ,@body)))

(def-fixture fresh-config ()
  (with-fresh-config (&body)))

(defun action (keys)
  (cdr (assoc (n::parse-key-sequence keys) n::*bindings* :test #'equal)))

(defun make-test-wm ()
  (let ((workspaces (let ((n::*num-of-workspaces* 4)) (n::make-workspaces))))
    (make-instance 'n::wm :display nil
                   :orphans workspaces
                   :active-workspace (first workspaces))))

(defun ws (wm number)
  (n::nth-workspace wm (1- number)))

(defun add-output (wm x &key name)
  (let ((output (make-instance 'n::output :proxy nil))
        (n::*num-of-workspaces* 4))
    (setf (n::output-x output) x
          (n::output-width output) 1000
          (n::output-height output) 800
          (n::output-name output) name
          (n::wm-outputs wm) (append (n::wm-outputs wm) (list output)))
    (n::assign-output wm output)
    output))

(defun open-window-on (ws)
  (let ((win (make-instance 'n::window :workspace ws)))
    (push win (n::workspace-windows ws))
    (setf (n::workspace-focused ws) win)
    win))
