;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defstruct (mailbox (:constructor make-mailbox))
  (lock (sb-thread:make-mutex :name "nucleotide mailbox"))
  (items '()))

(defun mailbox-send (mailbox item)
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (setf (mailbox-items mailbox)
	  (nconc (mailbox-items mailbox) (list item)))))

(defun mailbox-receive (mailbox)
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (pop (mailbox-items mailbox))))

(defvar *debug-on-error* nil
  "When true, errors in the event loop invoke the debugger (useful with SLY
connected). Otherwise they are logged and the loop keeps running, so a bug
in policy code can never freeze the session.")

(defstruct (event-loop (:constructor %make-event-loop)
                       (:conc-name loop-))
  display
  (mailbox (sb-concurrency:make-mailbox))
  wake-read-fd
  wake-write-fd
  (running t))

(defun make-event-loop (display)
  (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
    (%make-event-loop :display display
                      :wake-read-fd read-fd
                      :wake-write-fd write-fd)))

(defun event-loop-post (loop thunk)
  "Run THUNK inside the event loop's thread. Safe to call from any thread."
  (sb-concurrency:send-message (loop-mailbox loop) thunk)
  (let ((byte (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (sb-sys:with-pinned-objects (byte)
      (sb-unix:unix-write (loop-wake-write-fd loop)
                          (sb-sys:vector-sap byte) 0 1)))
  (values))

(defun event-loop-stop (loop)
  (event-loop-post loop (lambda () (setf (loop-running loop) nil))))

(defun %run-posted-thunks (loop)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buf)
      (sb-unix:unix-read (loop-wake-read-fd loop) (sb-sys:vector-sap buf) 64)))
  (loop for thunk = (sb-concurrency:receive-message-no-hang (loop-mailbox loop))
        while thunk
        do (funcall thunk)))

(defun run-event-loop (loop)
  (let* ((display (loop-display loop))
         (socket-fd (sb-bsd-sockets:socket-file-descriptor
                     (connection-socket (display-connection display))))
         (handlers
           (list (sb-sys:add-fd-handler
                  socket-fd :input
                  (lambda (fd)
                    (declare (ignore fd))
                    (dispatch-pending display)))
                 (sb-sys:add-fd-handler
                  (loop-wake-read-fd loop) :input
                  (lambda (fd)
                    (declare (ignore fd))
                    (%run-posted-thunks loop))))))
    (unwind-protect
         (loop initially
	   (loop while (buffered-message-size
			(display-connection display))
		 do (dispatch-event display))
               while (loop-running loop)
               do (handler-case (sb-sys:serve-event)
                    (wl-disconnected ()
                      (setf (loop-running loop) nil))
                    (error (c)
                      (if *debug-on-error*
                          (invoke-debugger c)
                          (format *error-output*
                                  "~&nucleotide: error in event loop: ~A~%" c)))))
      (mapc #'sb-sys:remove-fd-handler handlers)
      (sb-unix:unix-close (loop-wake-read-fd loop))
      (sb-unix:unix-close (loop-wake-write-fd loop)))))
