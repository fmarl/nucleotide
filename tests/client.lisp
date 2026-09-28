;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite client :in :nucleotide)
(in-suite client)

(n:define-interface test-object "test_object")

(n:define-event (test-object :everything 0)
  (object (:object))
  (number :int)
  (count :uint)
  (scale :fixed)
  (text :string))

(defun make-test-display ()
  (let ((display (make-instance 'n:wl-display
                                :id 1
                                :connection (n::make-wire-connection nil))))
    (setf (gethash 1 (n::display-proxies display)) display)
    display))

(defun feed-message (display sender opcode &rest words-and-strings)
  "Put a message into DISPLAY's read buffer, as if the server had sent it."
  (let* ((conn (n::display-connection display))
         (buf (n::connection-rbuf conn))
         (start (n::connection-rend conn))
         (off (+ start 8)))
    (dolist (arg words-and-strings)
      (setf off (if (stringp arg)
                    (n::put-wl-string buf off arg)
                    (progn (setf (n::u32ref buf off) (ldb (byte 32 0) arg))
                           (+ off 4)))))
    (setf (n::u32ref buf start) sender
          (n::u32ref buf (+ start 4)) (logior (ash (- off start) 16) opcode)
          (n::connection-rend conn) off)))

(defun record-events (proxy)
  (let ((events '()))
    (push (lambda (&rest event) (push event events)) (n:proxy-hooks proxy))
    (lambda () (reverse events))))

(test decode-event-arguments
  (let* ((display (make-test-display))
         (object (n:make-proxy display 'test-object))
         (events (record-events object)))
    (feed-message display (n:proxy-id object) 0
                  (n:proxy-id object) -5 7 (* 3/2 256) "hi")
    (n:dispatch-event display)
    (is (equal `((:everything ,object -5 7 3/2 "hi")) (funcall events)))))

(test unknown-objects-decode-as-nil
  (let* ((display (make-test-display))
         (object (n:make-proxy display 'test-object))
         (events (record-events object)))
    (feed-message display (n:proxy-id object) 0 999 0 0 0 "")
    (n:dispatch-event display)
    (is (null (second (first (funcall events)))))))

(test destroyed-proxies-get-no-events
  (let* ((display (make-test-display))
         (object (n:make-proxy display 'test-object))
         (events (record-events object)))
    (setf (n:proxy-destroyed-p object) t)
    (feed-message display (n:proxy-id object) 0 0 0 0 0 "")
    (n:dispatch-event display)
    (is (null (funcall events)))
    (is (null (n::buffered-message-size (n::display-connection display))))))

(test failing-hooks-do-not-block-later-messages
  (let* ((display (make-test-display))
         (object (n:make-proxy display 'test-object))
         (events (progn
                   (push (lambda (&rest event)
                           (declare (ignore event))
                           (error "boom"))
                         (n:proxy-hooks object))
                   (record-events object))))
    (feed-message display (n:proxy-id object) 0 0 1 0 0 "")
    (feed-message display (n:proxy-id object) 0 0 2 0 0 "")
    (let ((*error-output* (make-broadcast-stream)))
      (n::%dispatch-buffered display))
    (is (equal '(1 2) (mapcar #'third (funcall events))))))

(test delete-id-frees-ids
  (let* ((display (make-test-display))
         (object (n:make-proxy display 'test-object))
         (id (n:proxy-id object)))
    (n::handle-display-event display :delete-id id)
    (is (null (gethash id (n::display-proxies display))))
    (is (= id (n:proxy-id (n:make-proxy display 'test-object))))))

(test malformed-messages-end-dispatching
  (let* ((display (make-test-display))
         (conn (n::display-connection display))
         (buf (n::connection-rbuf conn)))
    (setf (n::u32ref buf 0) 1
          (n::u32ref buf 4) 0
          (n::connection-rend conn) 8)
    (signals n:wl-protocol-error (n::%dispatch-buffered display))))

(test only-connection-errors-escape-the-event-loop
  (is (null (let ((*error-output* (make-broadcast-stream)))
              (n::call-reporting-errors (lambda () (error "policy bug"))))))
  (signals n:wl-protocol-error
           (n::call-reporting-errors
            (lambda () (error 'n:wl-protocol-error :format-control "broken")))))
