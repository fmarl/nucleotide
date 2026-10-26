;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite ipc :in :nucleotide)
(in-suite ipc)

(test json-output
  (is (equal "{\"a\":1,\"b\":true,\"c\":false,\"d\":null,\"e\":[1,\"f\"],\"g\":[]}"
             (n::json-string '(:object ("a" . 1) ("b" . :true) ("c" . :false) ("d" . :null)
                               ("e" 1 "f") ("g")))))
  (is (equal "\"x\\\"y\\\\z\"" (n::json-string "x\"y\\z")))
  (is (equal "\"a\\u000Ab\"" (n::json-string (format nil "a~%b"))))
  (is (equal "\"grüße\"" (n::json-string "grüße"))))

(test json-input
  (is (equal "EventStream" (n::parse-json "\"EventStream\"")))
  (is (= 7 (n::json-get (n::parse-json
                         "{\"Action\":{\"FocusWorkspace\":{\"reference\":{\"Id\":7}}}}")
                        "Action" "FocusWorkspace" "reference" "Id")))
  (is (equal '(1 "a\"b" :true :null (:object))
             (n::parse-json "[1, \"a\\\"b\", true, null, {}]")))
  (signals n::nucleotide-error (n::parse-json "{\"a\":"))
  (signals n::nucleotide-error (n::parse-json "1 2"))
  (signals n::nucleotide-error (n::parse-json "[1.5]")))

(test deeply-nested-json-is-rejected
  (signals n::nucleotide-error
           (n::parse-json (make-string 60000 :initial-element #\[))))

(defun workspace-objects (wm)
  (mapcar #'rest (n::niri-workspaces wm)))

(defun field (object key)
  (cdr (assoc key object :test #'string=)))

(test only-workspaces-with-windows-or-shown-are-reported
  (let* ((wm (make-test-wm))
         (left (add-output wm 0 :name "eDP-1"))
         (right (add-output wm 1000 :name "HDMI-A-1"))
         (win (open-window-on (nth 2 (n::output-workspaces left)))))
    (declare (ignore right))
    (let ((objects (workspace-objects wm)))
      (is (equal '(("eDP-1" 1) ("eDP-1" 3) ("HDMI-A-1" 1))
                 (mapcar (lambda (ws) (list (field ws "output") (field ws "idx"))) objects)))
      (is (equal '(:true :false :true) (mapcar (lambda (ws) (field ws "is_active")) objects)))
      (is (equal '(:true :false :false) (mapcar (lambda (ws) (field ws "is_focused")) objects)))
      (is (equal (list :null (n::window-id win) :null)
                 (mapcar (lambda (ws) (field ws "active_window_id")) objects))))))

(test windows-know-their-workspace-and-focus
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1"))
         (ws (n::output-workspace output))
         (win (open-window-on ws)))
    (setf (n::window-title win) "title")
    (is (equal `(("id" . ,(n::window-id win)) ("title" . "title") ("app_id" . :null)
                 ("workspace_id" . ,(n::workspace-id ws)) ("is_focused" . :true))
               (rest (first (n::niri-windows wm)))))))

(defun focus-request (id)
  (n::parse-json
   (format nil "{\"Action\":{\"FocusWorkspace\":{\"reference\":{\"Id\":~D}}}}" id)))

(test focus-workspace-requests-switch-workspaces
  (let* ((wm (make-test-wm))
         (left (add-output wm 0 :name "eDP-1"))
         (right (add-output wm 1000 :name "HDMI-A-1"))
         (target (second (n::output-workspaces right)))
         (client (n::make-ipc-client nil)))
    (declare (ignore left))
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (is (equal (list n::*ipc-ok*)
                 (n::ipc-replies wm client (focus-request (n::workspace-id target)))))
      (is (eq target (n::wm-active-workspace wm)))
      (is (search "Err" (first (n::ipc-replies wm client (focus-request 0)))))
      (is (search "Err" (first (n::ipc-replies wm client (n::parse-json "\"Version\"")))))
      (is (not (n::ipc-client-subscribed client))))))

(test event-streams-start-with-ok-and-the-state
  (let* ((wm (make-test-wm))
         (client (n::make-ipc-client nil)))
    (add-output wm 0 :name "eDP-1")
    (let ((replies (n::ipc-replies wm client "EventStream")))
      (is (equal n::*ipc-ok* (first replies)))
      (is (= 3 (length replies)))
      (is (search "WindowsChanged" (second replies)))
      (is (search "WorkspacesChanged" (third replies)))
      (is (n::ipc-client-subscribed client)))))

(test requests-with-wrong-types-or-bytes-get-errors
  (let ((wm (make-test-wm))
        (client (n::make-ipc-client nil))
        (replies '()))
    (add-output wm 0 :name "eDP-1")
    (with-stubs ((n::ipc-send (lambda (client reply)
                                (declare (ignore client))
                                (push reply replies))))
      (dolist (request '("{\"Action\":{\"FocusWorkspace\":{\"reference\":{\"Index\":0}}}}"
                         "{\"Action\":{\"FocusWorkspace\":{\"reference\":{\"Index\":\"1\"}}}}"
                         "[]"))
        (n::ipc-handle-line wm client (sb-ext:string-to-octets request)))
      (n::ipc-handle-line wm client (coerce #(34 #xff 34) 'n::octets)))
    (is (= 4 (length replies)))
    (is (every (lambda (reply) (search "\"Err\"" reply)) replies))))

(test the-runtime-directory-must-be-private
  (let ((dir (format nil "/tmp/nucleotide-runtime-~D/" (sb-posix:getpid)))
        (old (sb-posix:getenv "XDG_RUNTIME_DIR")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_RUNTIME_DIR" dir 1)
           (sb-posix:chmod dir #o755)
           (signals n::nucleotide-error (n::ipc-socket-path))
           (sb-posix:chmod dir #o700)
           (is (search dir (n::ipc-socket-path))))
      (if old
          (sb-posix:setenv "XDG_RUNTIME_DIR" old 1)
          (sb-posix:unsetenv "XDG_RUNTIME_DIR"))
      (uiop:delete-directory-tree (pathname dir) :validate t))))

(defun serve (times)
  (loop repeat times do (sb-sys:serve-event 0.05)))

(defun read-reply (stream)
  "The next line from the IPC, or NIL if none arrives instead of blocking."
  (serve 5)
  (and (listen stream) (read-line stream nil)))

(defun server-clients-closed-p (ipc)
  (every (lambda (client) (null (n::ipc-client-socket client))) (n::ipc-clients ipc)))

(defmacro with-ipc-connection ((stream wm ipc) &body body)
  "Run BODY with STREAM connected to an IPC of WM that serves in this thread."
  (let ((path (gensym)) (handler (gensym)) (socket (gensym)))
    `(let* ((,path (format nil "/tmp/nucleotide-ipc-test-~D.sock" (sb-posix:getpid)))
            (,ipc (n::open-ipc ,path))
            (,handler (n::ipc-listen ,wm ,ipc))
            (,socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
       (setf (n::wm-ipc ,wm) ,ipc)
       (n::ipc-update ,wm)
       (unwind-protect
            (let ((,stream (progn (sb-bsd-sockets:socket-connect ,socket ,path)
                                  (sb-bsd-sockets:socket-make-stream
                                   ,socket :input t :output t :external-format :utf-8))))
              ,@body)
         (sb-sys:remove-fd-handler ,handler)
         (ignore-errors (sb-bsd-sockets:socket-close ,socket))
         (n::close-ipc ,ipc))
       (is (not (probe-file ,path))))))

(defun subscribe (stream)
  (write-line "\"EventStream\"" stream)
  (finish-output stream)
  (list (read-reply stream) (read-reply stream) (read-reply stream)))

(test subscribers-get-only-changes-over-the-socket
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1")))
    (with-ipc-connection (stream wm ipc)
      (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (n::ipc-path ipc))))))
      (destructuring-bind (ok windows workspaces) (subscribe stream)
        (is (equal n::*ipc-ok* ok))
        (is (search "WindowsChanged" windows))
        (is (search "WorkspacesChanged" workspaces)))
      (let ((win (open-window-on (n::output-workspace output))))
        (n::ipc-update wm)
        (is (search (format nil "\"id\":~D" (n::window-id win)) (read-reply stream)))
        (is (search (format nil "\"active_window_id\":~D" (n::window-id win))
                    (read-reply stream))))
      (n::ipc-update wm)
      (is (null (read-reply stream)))
      (setf (n::window-title (first (n::all-windows wm))) "new title")
      (n::ipc-update wm)
      (is (search "new title" (read-reply stream)))
      (is (null (read-reply stream))))))

(test clients-that-stop-reading-are-dropped
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1")))
    (with-ipc-connection (stream wm ipc)
      (subscribe stream)
      (close stream :abort t)
      (open-window-on (n::output-workspace output))
      (n::ipc-update wm)
      (is (server-clients-closed-p ipc)))))

(test oversized-requests-close-the-connection
  (let ((wm (make-test-wm)))
    (add-output wm 0 :name "eDP-1")
    (with-ipc-connection (stream wm ipc)
      (write-string (make-string 70000 :initial-element #\a) stream)
      (finish-output stream)
      (serve 30)
      (is (server-clients-closed-p ipc)))))

(test connections-are-limited
  (let ((wm (make-test-wm))
        (sockets '()))
    (add-output wm 0 :name "eDP-1")
    (with-ipc-connection (stream wm ipc)
      (declare (ignore stream))
      (unwind-protect
           (progn
             (loop repeat (+ 5 n::+ipc-max-clients+)
                   do (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                        (sb-bsd-sockets:socket-connect socket (n::ipc-path ipc))
                        (push socket sockets)
                        (serve 1)))
             (serve 20)
             (is (= n::+ipc-max-clients+ (length (n::ipc-clients ipc)))))
        (mapc #'sb-bsd-sockets:socket-close sockets)))))
