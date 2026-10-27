;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; The part of niri's IPC that waybar's niri/workspaces and niri/window
;;; modules use: JSON lines on a Unix socket, which the programs nucleotide
;;; starts find in NIRI_SOCKET.

(defstruct (ipc (:constructor make-ipc (path listener)))
  path
  listener
  (clients '())
  (last-events '()))

(defstruct (ipc-client (:constructor make-ipc-client (socket)))
  "A client is closed once its SOCKET is NIL."
  socket
  handler
  (input (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  subscribed)

(defconstant +ipc-max-request+ 65536)

(defconstant +ipc-max-clients+ 32)

(defparameter *ipc-ok* "{\"Ok\":\"Handled\"}")

(defun ipc-socket-path ()
  (let ((dir (or (uiop:getenv "XDG_RUNTIME_DIR")
                 (nucleotide-error "XDG_RUNTIME_DIR is not set"))))
    (check-file-permissions dir #o077)
    (format nil "~A/nucleotide.~A.~D.sock"
            dir (file-namestring (or (uiop:getenv "WAYLAND_DISPLAY") "wayland-0"))
            (sb-posix:getpid))))

(defun shown-workspace (wm win)
  "The workspace WIN appears on: the active one while it is highlighted."
  (if (eq win (wm-highlight wm))
      (wm-active-workspace wm)
      (window-workspace win)))

(defun niri-workspace (wm output ws index)
  (let* ((focus (if (eq ws (wm-active-workspace wm))
                    (wm-focus wm)
                    (workspace-focus ws)))
         (active (and focus (eq ws (shown-workspace wm focus)) focus)))
    `(:object ("id" . ,(workspace-id ws))
              ("idx" . ,index)
              ("name" . :null)
              ("output" . ,(output-name output))
              ("is_active" . ,(json-bool (eq ws (output-workspace output))))
              ("is_focused" . ,(json-bool (eq ws (wm-active-workspace wm))))
              ("is_urgent" . :false)
              ("active_window_id" . ,(if active (window-id active) :null)))))

(defun niri-workspaces (wm)
  "Like niri, only the workspaces that have windows or are shown."
  (loop for output in (wm-outputs wm)
        when (output-name output)
        append (loop for ws in (output-workspaces output)
                     for index from 1
                     when (or (workspace-windows ws) (eq ws (output-workspace output)))
                     collect (niri-workspace wm output ws index))))

(defun niri-window (wm win focused)
  `(:object ("id" . ,(window-id win))
            ("title" . ,(or (window-title win) :null))
            ("app_id" . ,(or (window-app-id win) :null))
            ("workspace_id" . ,(workspace-id (shown-workspace wm win)))
            ("is_focused" . ,(json-bool (eq win focused)))))

(defun niri-windows (wm)
  (let ((focused (wm-focus wm)))
    (loop for output in (wm-outputs wm)
          append (loop for ws in (output-workspaces output)
                       append (loop for win in (workspace-windows ws)
                                    collect (niri-window wm win focused))))))

(defun niri-events (wm)
  "Workspaces first: waybar's niri/window module redraws only on window events
and then shows the window that its workspace's active_window_id names."
  (list (json-string `(:object ("WorkspacesChanged"
                                :object ("workspaces" . ,(niri-workspaces wm)))))
        (json-string `(:object ("WindowsChanged" :object ("windows" . ,(niri-windows wm)))))))

(defun ipc-error (message)
  (json-string `(:object ("Err" . ,message))))

(defun close-client (client)
  (when (ipc-client-socket client)
    (sb-sys:remove-fd-handler (ipc-client-handler client))
    (ignore-errors (sb-bsd-sockets:socket-close (ipc-client-socket client)))
    (setf (ipc-client-socket client) nil)))

(defun ipc-send (client line)
  "Send LINE, or close CLIENT if it does not take it at once."
  (let ((octets (sb-ext:string-to-octets (format nil "~A~%" line) :external-format :utf-8))
        (socket (ipc-client-socket client)))
    (when (and socket
               (not (eql (length octets)
                         (ignore-errors
                           (sb-bsd-sockets:socket-send socket octets (length octets)
                                                       :nosignal t :dontwait t)))))
      (close-client client))))

(defun referenced-workspace (wm reference)
  (let ((id (json-get reference "Id"))
        (index (json-get reference "Index")))
    (cond (id (find id (mappend #'output-workspaces (wm-outputs wm)) :key #'workspace-id))
          ((typep index '(integer 1)) (nth-workspace wm (1- index))))))

(defun ipc-replies (wm client request)
  (let* ((reference (json-get request "Action" "FocusWorkspace" "reference"))
         (ws (and reference (referenced-workspace wm reference))))
    (cond ((equal request "EventStream")
           (setf (ipc-client-subscribed client) t)
           (cons *ipc-ok* (niri-events wm)))
          ((null reference) (list (ipc-error "unsupported request")))
          ((null ws) (list (ipc-error "no such workspace")))
          (t (end-highlight wm)
             (switch-to-workspace wm ws)
             (list *ipc-ok*)))))

(defun ipc-handle-line (wm client octets)
  (multiple-value-bind (request error)
      (ignore-errors (parse-json (sb-ext:octets-to-string octets :external-format :utf-8)))
    (dolist (reply (if error
                       (list (ipc-error "invalid request"))
                       (ipc-replies wm client request)))
      (ipc-send client reply))))

(defun ipc-read (wm client)
  (let* ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
         (length (nth-value 1 (ignore-errors
                                (sb-bsd-sockets:socket-receive (ipc-client-socket client)
                                                               buffer nil)))))
    (if (member length '(nil 0))
        (close-client client)
        (let ((input (concatenate 'octets (ipc-client-input client) (subseq buffer 0 length))))
          (loop for newline = (position 10 input)
                while (and newline (ipc-client-socket client))
                do (ipc-handle-line wm client (subseq input 0 newline))
                (setf input (subseq input (1+ newline))))
          (if (> (length input) +ipc-max-request+)
              (close-client client)
              (setf (ipc-client-input client) input))))))

(defun watch-fd (fd function)
  "Call FUNCTION when FD is readable. Only the thread that adds the handler
runs it: fd handlers are thread-local in SBCL."
  (sb-sys:add-fd-handler fd :input (lambda (fd)
                                     (declare (ignore fd))
                                     (call-reporting-errors function))))

(defun ipc-accept (wm ipc)
  (let ((socket (sb-bsd-sockets:socket-accept (ipc-listener ipc))))
    (setf (ipc-clients ipc) (remove-if-not #'ipc-client-socket (ipc-clients ipc)))
    (cond ((null socket))
          ((>= (length (ipc-clients ipc)) +ipc-max-clients+)
           (sb-bsd-sockets:socket-close socket))
          (t (let ((client (make-ipc-client socket)))
               (setf (ipc-client-handler client)
                     (watch-fd (sb-bsd-sockets:socket-file-descriptor socket)
                               (lambda () (ipc-read wm client))))
               (push client (ipc-clients ipc)))))))

(defun ipc-listen (wm ipc)
  (watch-fd (sb-bsd-sockets:socket-file-descriptor (ipc-listener ipc))
            (lambda () (ipc-accept wm ipc))))

(defun open-ipc (path)
  "An IPC listening on PATH, only for its owner; clients queue up until
IPC-LISTEN."
  (let ((listener (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (umask (sb-posix:umask #o177)))
    (unwind-protect
         (progn (when (probe-file path)
                  (delete-file path))
                (sb-bsd-sockets:socket-bind listener path))
      (sb-posix:umask umask))
    (sb-bsd-sockets:socket-listen listener 8)
    (make-ipc path listener)))

(defun close-ipc (ipc)
  (mapc #'close-client (ipc-clients ipc))
  (ignore-errors (sb-bsd-sockets:socket-close (ipc-listener ipc)))
  (ignore-errors (delete-file (ipc-path ipc))))

(defun start-ipc (wm)
  (let ((ipc (open-ipc (ipc-socket-path))))
    (setf (wm-ipc wm) ipc)
    (sb-posix:setenv "NIRI_SOCKET" (ipc-path ipc) 1)
    (event-loop-post (wm-loop wm) (lambda () (ipc-listen wm ipc)))))

(defun stop-ipc (wm)
  (when (wm-ipc wm)
    (close-ipc (wm-ipc wm))
    (setf (wm-ipc wm) nil)))

(defun ipc-update (wm)
  "Send subscribers every event from the first one that changed."
  (let ((ipc (wm-ipc wm)))
    (when ipc
      (let* ((events (niri-events wm))
             (changed (loop for tail on events
                            for last in (or (ipc-last-events ipc) '(nil nil))
                            unless (equal (first tail) last)
                            return tail)))
        (setf (ipc-last-events ipc) events
              (ipc-clients ipc) (remove-if-not #'ipc-client-socket (ipc-clients ipc)))
        (dolist (client (ipc-clients ipc))
          (when (ipc-client-subscribed client)
            (dolist (event changed)
              (ipc-send client event))))))))
