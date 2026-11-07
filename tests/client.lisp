;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite client :in :nucleotide)
(in-suite client)

(w::define-interface test-object "test_object")

(w::define-event (test-object :everything 0)
  (object (:object))
  (number :int)
  (count :uint)
  (scale :fixed)
  (text :string))

(defun make-test-display ()
  (let ((display (make-instance 'w::wl-display
                                :id 1
                                :connection (w::make-wire-connection nil))))
    (setf (gethash 1 (w::display-proxies display)) display)
    display))

(defun feed-message (display sender opcode &rest words-and-strings)
  "Put a message into DISPLAY's read buffer, as if the server had sent it."
  (let* ((conn (w::display-connection display))
         (buf (w::connection-rbuf conn))
         (start (w::connection-rend conn))
         (off (+ start 8)))
    (dolist (arg words-and-strings)
      (setf off (if (stringp arg)
                    (w::put-wl-string buf off arg)
                    (progn (setf (w::u32ref buf off) (ldb (byte 32 0) arg))
                           (+ off 4)))))
    (setf (w::u32ref buf start) sender
          (w::u32ref buf (+ start 4)) (logior (ash (- off start) 16) opcode)
          (w::connection-rend conn) off)))

(defun record-events (proxy)
  (let ((events '()))
    (push (lambda (&rest event) (push event events)) (w::proxy-hooks proxy))
    (lambda () (reverse events))))

(test decode-event-arguments
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (events (record-events object)))
    (feed-message display (w::proxy-id object) 0
                  (w::proxy-id object) -5 7 (* 3/2 256) "hi")
    (w::dispatch-event display)
    (is (equal `((:everything ,object -5 7 3/2 "hi")) (funcall events)))))

(test unknown-objects-decode-as-nil
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (events (record-events object)))
    (feed-message display (w::proxy-id object) 0 999 0 0 0 "")
    (w::dispatch-event display)
    (is (null (second (first (funcall events)))))))

(test destroyed-proxies-get-no-events
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (events (record-events object)))
    (setf (w::proxy-destroyed-p object) t)
    (feed-message display (w::proxy-id object) 0 0 0 0 0 "")
    (w::dispatch-event display)
    (is (null (funcall events)))
    (is (null (w::buffered-message-size (w::display-connection display))))))

(test failing-hooks-do-not-block-later-messages
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (events (progn
                   (push (lambda (&rest event)
                           (declare (ignore event))
                           (error "boom"))
                         (w::proxy-hooks object))
                   (record-events object))))
    (feed-message display (w::proxy-id object) 0 0 1 0 0 "")
    (feed-message display (w::proxy-id object) 0 0 2 0 0 "")
    (let ((*error-output* (make-broadcast-stream)))
      (w::dispatch-buffered display))
    (is (equal '(1 2) (mapcar #'third (funcall events))))))

(test delete-id-frees-ids
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (id (w::proxy-id object)))
    (w::handle-display-event display :delete-id id)
    (is (null (gethash id (w::display-proxies display))))
    (is (= id (w::proxy-id (w::make-proxy display 'test-object))))))

(test malformed-messages-end-dispatching
  (let* ((display (make-test-display))
         (conn (w::display-connection display))
         (buf (w::connection-rbuf conn)))
    (setf (w::u32ref buf 0) 1
          (w::u32ref buf 4) 0
          (w::connection-rend conn) 8)
    (signals w::wl-protocol-error (w::dispatch-buffered display))))

(test arguments-must-end-within-their-message
  (let* ((display (make-test-display))
         (object (w::make-proxy display 'test-object))
         (events (record-events object))
         (conn (w::display-connection display)))
    (feed-message display (w::proxy-id object) 0 0 0 0 0 40 #x41414141)
    (feed-message display (w::proxy-id object) 0 0 1 0 0 "")
    (signals w::wl-protocol-error (w::dispatch-event display))
    (is (= 32 (w::connection-rstart conn)))
    (feed-message display (w::proxy-id object) 0 0)
    (w::dispatch-event display)
    (signals w::wl-protocol-error (w::dispatch-event display))
    (is (equal '(1) (mapcar #'third (funcall events))))))

(test only-connection-errors-escape-the-event-loop
  (is (null (let ((*error-output* (make-broadcast-stream)))
              (w::call-reporting-errors (lambda () (error "policy bug"))))))
  (signals w::wl-protocol-error
           (w::call-reporting-errors
            (lambda () (error 'w::wl-protocol-error :format-control "broken")))))

(test requests-come-only-from-the-thread-of-the-display
  (let* ((display (make-test-display))
         (proxy (w::make-proxy display 'test-object)))
    (is (typep (sb-thread:join-thread
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case (w::send-request proxy 0 '() '())
                     (w::nucleotide-error (c) c)))))
               'w::nucleotide-error))))

(test messages-below-the-log-level-are-dropped
  (let ((w::*log-level* :warn))
    (is (equal "" (with-output-to-string (*error-output*)
                    (w::log-message :info "hidden"))))
    (is (search "warn: shown" (with-output-to-string (*error-output*)
                                (w::log-message :warn "shown"))))))

(test requests-never-name-destroyed-objects
  (let* ((display (make-test-display))
         (proxy (w::make-proxy display 'test-object))
         (gone (w::make-proxy display 'test-object)))
    (setf (w::proxy-destroyed-p gone) t)
    (signals w::nucleotide-error (w::send-request proxy 0 '(:object) (list gone)))))

(w::define-request (test-object newer 0 :since 2))

(test requests-need-the-version-that-introduced-them
  (let ((display (make-test-display))
        (sent '()))
    (with-stubs ((w::send-request (lambda (proxy &rest request)
                                    (declare (ignore request))
                                    (push proxy sent))))
      (let ((old (w::make-proxy display 'test-object :version 1))
            (new (w::make-proxy display 'test-object :version 2)))
        (signals w::nucleotide-error (test-object.newer old))
        (test-object.newer new)
        (is (equal (list new) sent))))))
