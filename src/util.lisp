;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun notify (control &rest args)
  (let ((message (apply #'format nil control args)))
    (format *error-output* "~&nucleotide: ~A~%" message)
    (ignore-errors
      (uiop:launch-program (list "notify-send" "nucleotide" message)))
    (values)))

(defun confine-thread-errors ()
  "With the debugger disabled, an error in any thread quits the process.
Skip the failing event instead, or else end only the failing thread."
  (let ((previous sb-ext:*invoke-debugger-hook*))
    (setf sb-ext:*invoke-debugger-hook*
          (lambda (condition hook)
            (let ((skip (find-restart 'skip condition))
                  (thread sb-thread:*current-thread*))
              (cond
                (skip
                 (format *error-output* "~&nucleotide: error in event loop: ~A~%"
                         condition)
                 (invoke-restart skip))
                ((not (eq thread (sb-thread:main-thread)))
                 (format *error-output* "~&nucleotide: thread ~S ended: ~A~%"
                         (sb-thread:thread-name thread) condition)
                 (sb-thread:abort-thread))
                (previous
                 (funcall previous condition hook))))))))

(defun check-file-permissions (path forbidden-bits)
  (let ((stat (sb-posix:stat (sb-ext:native-namestring path))))
    (unless (= (sb-posix:stat-uid stat) (sb-posix:getuid))
      (nucleotide-error "~A is not owned by you" path))
    (unless (zerop (logand (sb-posix:stat-mode stat) forbidden-bits))
      (nucleotide-error "~A has unsafe permissions ~3,'0O"
                        path (logand (sb-posix:stat-mode stat) #o777)))))

(defmacro drain (place)
  "PLACE's list, leaving PLACE empty."
  `(prog1 ,place (setf ,place '())))

(defun spawn (command)
  "Start COMMAND detached from nucleotide. A string is run by sh, so it may
contain arguments (\"alacritty -e htop\"); a list is used as argv directly."
  (uiop:launch-program
   (list* "setsid" "-f"
          (if (stringp command)
              (list "sh" "-c" command)
              command))))

(defun alist-get (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun alist-remove (key alist)
  (remove key alist :key #'car :test #'equal))

(defun alist-set (key value alist)
  (acons key value (alist-remove key alist)))

(defun write-all (fd octets)
  (sb-sys:with-pinned-objects (octets)
    (loop with written = 0
          while (< written (length octets))
          do (incf written (sb-posix:write fd
                                           (sb-sys:sap+ (sb-sys:vector-sap octets)
                                                        written)
                                           (- (length octets) written))))))
