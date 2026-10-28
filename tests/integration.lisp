;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide/integration
  (:use #:cl #:fiveam)
  (:local-nicknames (#:n #:nucleotide)))

(in-package #:nucleotide/integration)

(def-suite :nucleotide/integration)
(in-suite :nucleotide/integration)

(defparameter *programs* '("river" "weston-simple-shm" "wlr-randr"))

(defvar *runtime-directory*)

(defun environment ()
  (list (format nil "XDG_RUNTIME_DIR=~A" *runtime-directory*)
        "WLR_BACKENDS=headless" "WLR_HEADLESS_OUTPUTS=2"
        "WLR_LIBINPUT_NO_DEVICES=1" "WLR_RENDERER=pixman"))

(defun wayland-socket ()
  (first (directory (merge-pathnames "wayland-*" *runtime-directory*))))

(defun launch (&rest command)
  "Start COMMAND for the river under test, logging to a file named after it."
  (let ((log (merge-pathnames (format nil "~A.log" (first command)) *runtime-directory*)))
    (with-open-file (out log :direction :output :if-exists :append :if-does-not-exist :create))
    (uiop:launch-program (append '("env" "-u" "WAYLAND_DISPLAY" "-u" "DISPLAY")
                                 (environment)
                                 (let ((socket (wayland-socket)))
                                   (and socket (list (format nil "WAYLAND_DISPLAY=~A"
                                                             (uiop:native-namestring socket)))))
                                 command)
                         :output log
                         :if-output-exists :append
                         :error-output :output)))

(defun await (predicate &key (seconds 5))
  "The first true value of PREDICATE within SECONDS, else NIL."
  (loop repeat (* 20 seconds)
        thereis (funcall predicate)
        do (sleep 0.05)))

(defun in-wm (function)
  (let ((semaphore (sb-thread:make-semaphore))
        (result nil))
    (n:in-wm (setf result (funcall function))
             (sb-thread:signal-semaphore semaphore))
    (unless (sb-thread:wait-on-semaphore semaphore :timeout 5)
      (error "the WM did not answer"))
    result))

(defun connected-p ()
  (and n:*wm*
       (sb-thread:thread-alive-p (n::wm-thread n:*wm*))
       (not (n::loop-failure (n::wm-loop n:*wm*)))))

(defvar *manages* 0)

(defun roundtrip ()
  "Wait until river handled every request sent so far. A protocol error arrives
before the answer, so a broken connection is visible afterwards."
  (let ((done (sb-thread:make-semaphore)))
    (in-wm (lambda ()
             (push (lambda (&rest event)
                     (declare (ignore event))
                     (sb-thread:signal-semaphore done))
                   (n::proxy-hooks (n::wl-display.sync (n::wm-display n:*wm*))))))
    (sb-thread:wait-on-semaphore done :timeout 5)))

(defun settle ()
  "Wait until a manage sequence ran and river handled its requests."
  (let ((before *manages*))
    (when (connected-p)
      (in-wm (lambda () (n:mark-dirty n:*wm*)))
      (await (lambda () (or (> *manages* before) (not (connected-p)))))
      (when (connected-p)
        (roundtrip)))))

(defun call-with-environment (variables function)
  (let ((previous (mapcar (lambda (variable) (uiop:getenv (car variable))) variables)))
    (unwind-protect
         (progn
           (loop for (name . value) in variables
                 do (sb-posix:setenv name value 1))
           (funcall function))
      (loop for (name) in variables
            for value in previous
            do (if value
                   (sb-posix:setenv name value 1)
                   (sb-posix:unsetenv name))))))

(defun call-with-river (function)
  (let* ((*runtime-directory* (uiop:ensure-directory-pathname
                               (sb-posix:mkdtemp "/tmp/nucleotide-it-XXXXXX")))
         (config (ensure-directories-exist (merge-pathnames "config/" *runtime-directory*)))
         (river (launch "river" "-no-xwayland" "-c" "true"))
         (notify (fdefinition 'n::notify))
         (manage (fdefinition 'n::manage))
         (n::*autostarted* t))
    (setf *manages* 0)
    (unwind-protect
         (call-with-environment
          `(("XDG_RUNTIME_DIR" . ,(uiop:native-namestring *runtime-directory*))
            ("XDG_CONFIG_HOME" . ,(uiop:native-namestring config)))
          (lambda ()
            (unless (await #'wayland-socket)
              (error "river did not start, see ~A" *runtime-directory*))
            (setf (fdefinition 'n::notify) (lambda (&rest args) (declare (ignore args)))
                  (fdefinition 'n::manage) (lambda (wm)
                                             (funcall manage wm)
                                             (incf *manages*)))
            (n:start-wm :display-name (uiop:native-namestring (wayland-socket)))
            (funcall function)))
      (ignore-errors (n:stop-wm))
      (setf (fdefinition 'n::notify) notify
            (fdefinition 'n::manage) manage)
      (ignore-errors (uiop:terminate-process river))
      (uiop:wait-process river)
      (uiop:delete-directory-tree *runtime-directory* :validate t))))

(defmacro with-river (&body body)
  `(call-with-river (lambda () ,@body)))

(defun open-windows (count)
  "Whether COUNT new clients opened their windows."
  (loop repeat count
        do (launch "weston-simple-shm"))
  (await (lambda ()
           (= count (in-wm (lambda () (length (n:all-windows n:*wm*))))))))

(defun installed-p (program)
  (some (lambda (directory) (probe-file (merge-pathnames program directory)))
        (uiop:getenv-absolute-directories "PATH")))

(defun skip-without-programs ()
  (let ((missing (remove-if #'installed-p *programs*)))
    (when missing
      (skip "~{~A~^, ~} not found" missing))
    (null missing)))

(test squeezed-windows-keep-the-connection
  (when (skip-without-programs)
    (with-river
        (is-true (open-windows 3))
      (in-wm (lambda () (setf n:*gap* 700)))
      (settle)
      (is-true (connected-p)))))

(test fullscreen-requests-survive-their-output
  (when (skip-without-programs)
    (with-river
        (is-true (open-windows 1))
      ;; As if the window asked for fullscreen there just before river removed it.
      (in-wm (lambda ()
               (let ((wm n:*wm*))
                 (n::request wm :fullscreen (first (n:all-windows wm))
                             (find "HEADLESS-2" (n::wm-outputs wm)
                                   :key #'n::output-name :test #'equal)))))
      (uiop:wait-process (launch "wlr-randr" "--output" "HEADLESS-2" "--off"))
      (await (lambda ()
               (or (not (connected-p))
                   (= 1 (in-wm (lambda () (length (n::wm-outputs n:*wm*))))))))
      (settle)
      (is-true (connected-p)))))

(test exiting-the-session-ends-river
  (when (skip-without-programs)
    (with-river
        (in-wm (lambda () (n::exit-session n:*wm*)))
      (is-true (await (lambda () (not (connected-p))))))))
