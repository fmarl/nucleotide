;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite ipc :in :nucleotide)
(in-suite ipc)

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
    (setf (n::%window-title win) "title")
    (is (equal `(("id" . ,(n::window-id win)) ("title" . "title") ("app_id" . :null)
                 ("workspace_id" . ,(n::workspace-id ws)) ("is_focused" . :true))
               (rest (first (n::niri-windows wm)))))))

(defun focus-request (id)
  (json:parse
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
                 (n::handle-ipc-request wm client (focus-request (n::workspace-id target)))))
      (is (eq target (n::wm-active-workspace wm)))
      (is (search "Err" (first (n::handle-ipc-request wm client (focus-request 0)))))
      (is (search "Err" (first (n::handle-ipc-request wm client (json:parse "\"Version\"")))))
      (is (not (n::ipc-client-subscribed-p client))))))

(test focus-workspace-requests-ignore-workspaces-without-output
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1"))
         (removed (add-output wm 1000 :name "HDMI-A-1"))
         (stashed (first (n::output-workspaces removed)))
         (client (n::make-ipc-client nil)))
    (n::unassign-output wm removed)
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (is (search "Err" (first (n::handle-ipc-request wm client
                                                      (focus-request (n::workspace-id stashed))))))
      (is (eq output (n::wm-focused-output wm))))))

(test socket-paths-take-the-name-of-an-absolute-wayland-display
  (with-private-directory (runtime)
    (with-environment (("XDG_RUNTIME_DIR" runtime)
                       ("WAYLAND_DISPLAY" "/run/user/1000/wayland-1"))
      (is (equal (format nil "~A/nucleotide.wayland-1.~D.sock" runtime (sb-posix:getpid))
                 (n::ipc-socket-path))))))

(test event-streams-start-with-ok-and-the-state
  (let* ((wm (make-test-wm))
         (client (n::make-ipc-client nil)))
    (add-output wm 0 :name "eDP-1")
    (let ((replies (n::handle-ipc-request wm client "EventStream")))
      (is (equal n::*ipc-ok* (first replies)))
      (is (= 3 (length replies)))
      (is (search "WorkspacesChanged" (second replies)))
      (is (search "WindowsChanged" (third replies)))
      (is (n::ipc-client-subscribed-p client)))))

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
  (with-private-directory (runtime)
    (with-environment (("XDG_RUNTIME_DIR" runtime))
      (sb-posix:chmod runtime #o755)
      (signals n::nucleotide-error (n::ipc-socket-path))
      (sb-posix:chmod runtime #o700)
      (is (search runtime (n::ipc-socket-path))))))

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
  (let ((directory (gensym)) (path (gensym)) (handler (gensym)) (socket (gensym)))
    `(with-private-directory (,directory)
       (let* ((,path (format nil "~A/ipc.sock" ,directory))
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
           (n::without-socket-errors (sb-bsd-sockets:socket-close ,socket))
           (n::close-ipc ,ipc))
         (is (not (probe-file ,path)))))))

(defun subscribe (stream)
  (write-line "\"EventStream\"" stream)
  (finish-output stream)
  (list (read-reply stream) (read-reply stream) (read-reply stream)))

(test the-socket-is-only-for-its-owner
  (let ((wm (make-test-wm)))
    (add-output wm 0 :name "eDP-1")
    (with-ipc-connection (stream wm ipc)
      (declare (ignore stream))
      (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (n::ipc-path ipc)))))))))

(test subscribers-get-only-changes-over-the-socket
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1")))
    (with-ipc-connection (stream wm ipc)
      (destructuring-bind (ok workspaces windows) (subscribe stream)
        (is (equal n::*ipc-ok* ok))
        (is (search "WindowsChanged" windows))
        (is (search "WorkspacesChanged" workspaces)))
      (let ((win (open-window-on (n::output-workspace output))))
        (n::ipc-update wm)
        (is (search (format nil "\"active_window_id\":~D" (n::window-id win))
                    (read-reply stream)))
        (is (search (format nil "\"id\":~D" (n::window-id win)) (read-reply stream))))
      (n::ipc-update wm)
      (is (null (read-reply stream)))
      (setf (n::%window-title (first (n::all-windows wm))) "new title")
      (n::ipc-update wm)
      (is (search "new title" (read-reply stream)))
      (is (null (read-reply stream))))))

(test focus-changes-send-the-active-window-before-the-windows
  (let* ((wm (make-test-wm))
         (ws (n::output-workspace (add-output wm 0 :name "eDP-1")))
         (first-win (open-window-on ws)))
    (open-window-on ws)
    (with-ipc-connection (stream wm ipc)
      (subscribe stream)
      (setf (n::workspace-focus ws) first-win)
      (n::ipc-update wm)
      (let ((workspaces (read-reply stream))
            (windows (read-reply stream)))
        (is (search (format nil "\"active_window_id\":~D" (n::window-id first-win)) workspaces))
        (is (search "WindowsChanged" windows))
        (is (null (read-reply stream)))))))

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

(test highlighted-windows-appear-on-the-active-workspace
  (let* ((wm (make-test-wm))
         (output (add-output wm 0 :name "eDP-1"))
         (active (n::output-workspace output))
         (home (ws wm 2))
         (highlighted (open-window-on home)))
    (open-window-on active)
    (setf (n::wm-highlight wm) highlighted)
    (is (eql (n::workspace-id active)
             (json:lookup (find (n::window-id highlighted) (n::niri-windows wm)
                                :key (lambda (win) (json:lookup win "id")))
                          "workspace_id")))
    (is (eql (n::window-id highlighted)
             (json:lookup (n::niri-workspace wm output active 1) "active_window_id")))
    (is (eq :null (json:lookup (n::niri-workspace wm output home 2) "active_window_id")))))
