;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *repl-port* 4005
  :check (type-check '(integer 1 65535) "a port like 4005"))

(defun repl-secret-file ()
  (merge-pathnames ".sly-secret" (user-homedir-pathname)))

(defun random-hex (n-bytes)
  (let ((bytes (make-array n-bytes :element-type '(unsigned-byte 8))))
    (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence bytes in))
    (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list))))

(defun create-repl-secret (path)
  (let ((fd (sb-posix:open (sb-ext:native-namestring path)
                           (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl)
                           #o600)))
    (with-open-stream (out (sb-sys:make-fd-stream fd :output t
                                                  :external-format :latin-1))
      (write-line (random-hex 32) out)))
  (notify "created a REPL secret in ~A" path))

(defun ensure-repl-secret ()
  (let ((path (repl-secret-file)))
    (unless (probe-file path)
      (create-repl-secret path))
    (check-file-permissions path #o077)
    (when (zerop (length (with-open-file (in path) (read-line in nil ""))))
      (nucleotide-error "~A is empty" path))))

(defun ensure-slynk ()
  (unless (find-package '#:slynk)
    (asdf:load-system "slynk")))

(defun repl-listening-p (&optional (port *repl-port*))
  (let ((package (find-package '#:slynk)))
    (and package
         (find port (symbol-value (find-symbol "*SERVERS*" package)) :key #'second)
         t)))

(defun start-repl-server (&key (port *repl-port*) dont-close)
  "Open a Slynk listener on localhost:PORT, protected by ~/.sly-secret. Unless
DONT-CLOSE, it stops listening after the first connection."
  (ensure-slynk)
  (ensure-repl-secret)
  (when (repl-listening-p port)
    (nucleotide-error "a REPL is already listening on port ~D" port))
  (uiop:symbol-call '#:slynk '#:create-server :port port :dont-close dont-close)
  (loop repeat 100
        until (repl-listening-p port)
        do (sleep 0.01))
  port)

(defun stop-repl-server (&key (port *repl-port*))
  "Stop listening on PORT. Established connections stay open."
  (when (repl-listening-p port)
    (uiop:symbol-call '#:slynk '#:stop-server port)))

(define-command toggle-repl (wm)
  "Open the REPL port, or close it."
  (declare (ignore wm))
  (if (repl-listening-p)
      (progn
        (stop-repl-server)
        (notify "REPL port ~D closed" *repl-port*))
      (handler-case
          (progn
            (start-repl-server)
            (notify "REPL listening on port ~D" *repl-port*))
        (error (c)
          (notify "REPL not started: ~A" c)))))
