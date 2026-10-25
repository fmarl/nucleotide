;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun notify (control &rest args)
  (let ((message (apply #'format nil control args)))
    (format *error-output* "~&nucleotide: ~A~%" message)
    (ignore-errors
      (uiop:launch-program (list "notify-send" "nucleotide" message)))
    (values)))

(defun check-file-permissions (path forbidden-bits &key allow-root)
  (let* ((stat (sb-posix:stat (sb-ext:native-namestring path)))
         (uid (sb-posix:stat-uid stat)))
    (unless (or (= uid (sb-posix:getuid))
                (and allow-root (zerop uid)))
      (nucleotide-error "~A is not owned by you" path))
    (unless (zerop (logand (sb-posix:stat-mode stat) forbidden-bits))
      (nucleotide-error "~A has unsafe permissions ~3,'0O"
                        path (logand (sb-posix:stat-mode stat) #o777)))))

(defmacro drain (place)
  `(prog1 ,place (setf ,place '())))

(defun spawn-program (command)
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
