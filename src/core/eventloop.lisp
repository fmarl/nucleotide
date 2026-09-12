;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *debug-on-error* nil
  "When true, errors in the event loop invoke the debugger (useful with SLY
connected). Otherwise they are logged and the loop keeps running, so a bug
in policy code can never freeze the session.")

(defstruct (event-loop (:constructor %make-event-loop)
                       (:conc-name loop-))
  display
  (mailbox (sb-concurrency:make-mailbox))
  (lock (sb-thread:make-mutex :name "nucleotide event loop"))
  wake-read-fd
  wake-write-fd
  (running t))

(defun make-event-loop (display)
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (%make-event-loop :display display
                      :wake-read-fd read-fd
                      :wake-write-fd write-fd)))

(defun event-loop-post (loop thunk)
  "Run THUNK inside the event loop's thread. Safe to call from any thread."
  (sb-thread:with-mutex ((loop-lock loop))
    (unless (loop-wake-write-fd loop)
      (nucleotide-error "the event loop is not running"))
    (sb-concurrency:send-message (loop-mailbox loop) thunk)
    (let ((byte (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      (sb-sys:with-pinned-objects (byte)
        (sb-posix:write (loop-wake-write-fd loop)
                        (sb-sys:vector-sap byte) 1))))
  (values))

(defun event-loop-stop (loop)
  (event-loop-post loop (lambda () (setf (loop-running loop) nil))))

(defun report-or-debug (condition)
  "Skip the failing event, or with *DEBUG-ON-ERROR* enter the debugger with
the failing frames still on the stack. Wayland connection errors are left to
end the loop."
  (unless (typep condition 'wl-error)
    (if *debug-on-error*
        (invoke-debugger condition)
        (progn
          (format *error-output* "~&nucleotide: error in event loop: ~A~%" condition)
          (invoke-restart 'skip)))))

(defun call-reporting-errors (thunk)
  (restart-case (handler-bind ((error #'report-or-debug))
                  (funcall thunk))
    (skip ()
      :report "Skip this event and keep the event loop running."
      nil)))

(defun %dispatch-buffered (display)
  (loop while (buffered-message-size (display-connection display))
        do (call-reporting-errors (lambda () (dispatch-event display)))))

(defun %run-posted-thunks (loop)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buf)
      (sb-posix:read (loop-wake-read-fd loop) (sb-sys:vector-sap buf) 64)))
  (loop for thunk = (sb-concurrency:receive-message-no-hang (loop-mailbox loop))
        while thunk
        do (call-reporting-errors thunk)))

(defun run-event-loop (loop)
  (let* ((display (loop-display loop))
         (socket-fd (sb-bsd-sockets:socket-file-descriptor
                     (connection-socket (display-connection display))))
         (handlers
          (list (sb-sys:add-fd-handler
                 socket-fd :input
                 (lambda (fd)
                   (declare (ignore fd))
                   (%fill-read-buffer (display-connection display))
                   (%dispatch-buffered display)))
                (sb-sys:add-fd-handler
                 (loop-wake-read-fd loop) :input
                 (lambda (fd)
                   (declare (ignore fd))
                   (%run-posted-thunks loop))))))
    (unwind-protect
         (loop while (loop-running loop)
               do (handler-case (progn (%dispatch-buffered display)
                                       (sb-sys:serve-event))
                    (wl-error (c)
                      (unless (typep c 'wl-disconnected)
                        (format *error-output* "~&nucleotide: ~A~%" c))
                      (setf (loop-running loop) nil))
                    (error (c)
                      (format *error-output*
                              "~&nucleotide: error in event loop: ~A~%" c))))
      (mapc #'sb-sys:remove-fd-handler handlers)
      (sb-thread:with-mutex ((loop-lock loop))
        (setf (loop-running loop) nil)
        (sb-posix:close (loop-wake-read-fd loop))
        (sb-posix:close (loop-wake-write-fd loop))
        (setf (loop-wake-read-fd loop) nil
              (loop-wake-write-fd loop) nil)))))
