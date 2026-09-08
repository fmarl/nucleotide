;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *repl-allowed* t
  "NIL in builds that must never open a REPL port (make no-repl).")

(defparameter *repl-port* 4005)

(defparameter *repl-idle-timeout* 300
  "Seconds a listener opened with TOGGLE-REPL-SERVER waits for a connection
before it closes itself. NIL waits forever.")

(defvar *repl-timer* nil)

(defun repl-secret-file ()
  (merge-pathnames ".sly-secret" (user-homedir-pathname)))

(defun random-hex (n-bytes)
  (let ((bytes (make-array n-bytes :element-type '(unsigned-byte 8))))
    (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence bytes in))
    (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list))))

(defun create-repl-secret (path)
  ;; O_EXCL with mode 600: the file is never readable by others, not even
  ;; for a moment, and we never clobber an existing one.
  (let ((fd (sb-posix:open (sb-ext:native-namestring path)
                           (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl)
                           #o600)))
    (with-open-stream (out (sb-sys:make-fd-stream fd :output t
                                                  :external-format :latin-1))
      (write-line (random-hex 32) out)))
  (notify "created a REPL secret in ~A" path))

(defun ensure-repl-secret ()
  "Make sure Slynk will demand a secret. Slynk silently accepts everyone
when the file is missing, and does not check who else can read it."
  (let ((path (repl-secret-file)))
    (unless (probe-file path)
      (create-repl-secret path))
    (check-file-permissions path #o077)
    (when (zerop (length (with-open-file (in path) (read-line in nil ""))))
      (nucleotide-error "~A is empty" path))))

(defun ensure-slynk ()
  (unless *repl-allowed*
    (nucleotide-error "this nucleotide build has no REPL support"))
  (unless (find-package '#:slynk)
    (asdf:load-system "slynk")))

(defun repl-listening-p (&optional (port *repl-port*))
  (let ((package (find-package '#:slynk)))
    (and package
         (find port (symbol-value (find-symbol "*SERVERS*" package)) :key #'second)
         t)))

(defun cancel-repl-timeout ()
  (when *repl-timer*
    (sb-ext:unschedule-timer *repl-timer*)
    (setf *repl-timer* nil)))

(defun schedule-repl-timeout (port seconds)
  (cancel-repl-timeout)
  (setf *repl-timer*
        (sb-ext:make-timer
         (lambda ()
           (when (repl-listening-p port)
             (stop-repl-server :port port)
             (notify "REPL port ~D closed, nobody connected within ~Ds"
                     port seconds)))
         :name "nucleotide REPL timeout"
         :thread t))
  (sb-ext:schedule-timer *repl-timer* seconds))

(defun start-repl-server (&key (port *repl-port*) dont-close
                            (timeout *repl-idle-timeout*))
  "Open a Slynk listener on localhost:PORT, protected by ~/.sly-secret.
Unless DONT-CLOSE, it accepts a single connection and then stops listening,
and closes after TIMEOUT seconds if nobody connects. Established
connections are unaffected by either."
  (ensure-slynk)
  (ensure-repl-secret)
  (when (repl-listening-p port)
    (nucleotide-error "a REPL is already listening on port ~D" port))
  (uiop:symbol-call '#:slynk '#:create-server :port port :dont-close dont-close)
  ;; Slynk registers the listener asynchronously; wait for it so that an
  ;; immediate toggle sees the listener.
  (loop repeat 100
        until (repl-listening-p port)
        do (sleep 0.01))
  (when (and timeout (not dont-close))
    (schedule-repl-timeout port timeout))
  port)

(defun stop-repl-server (&key (port *repl-port*))
  "Stop listening on PORT. Established connections stay open."
  (cancel-repl-timeout)
  (when (repl-listening-p port)
    (uiop:symbol-call '#:slynk '#:stop-server port)))

(defun toggle-repl-server ()
  (if (repl-listening-p)
      (progn
        (stop-repl-server)
        (notify "REPL port ~D closed" *repl-port*))
      (handler-case
          (progn
            (start-repl-server)
            (notify "REPL listening on port ~D~@[ for ~Ds~]"
                    *repl-port* *repl-idle-timeout*))
        (error (c)
          (notify "REPL not started: ~A" c)))))

(defun start-repl-from-env ()
  (when (uiop:getenvp "NUCLEOTIDE_REPL")
    (handler-case (start-repl-server :dont-close t)
      (error (c)
        (notify "REPL not started: ~A" c)))))
