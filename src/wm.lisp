;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *wm* nil)

(defparameter *bound-globals*
  '((river-window-manager-v1 5 wm-river)
    (river-layer-shell-v1 1 wm-layer-shell)
    (river-xkb-bindings-v1 3 wm-xkb)
    (river-input-manager-v1 2 wm-input-manager)
    (river-libinput-config-v1 2 wm-libinput-config))
  "The globals we bind: proxy class, highest version we support, and the WM
accessor that holds the proxy.")

(define-setting *num-of-workspaces* 9
  :documentation "Workspaces per output. Read at startup."
  :check (type-check '(integer 1) "a number of workspaces like 9"))

(defun make-workspaces ()
  (loop for i from 1 to *num-of-workspaces*
        collect (make-instance 'workspace :name (princ-to-string i))))

(define-command exit-session (wm)
  "End the river session."
  (if (supports-p (wm-river wm) 4)
      (river-window-manager-v1.exit-session (wm-river wm))
      (notify "this river cannot end the session, quit it yourself")))

(define-handler (wm (river river-window-manager-v1) :window) (proxy)
  (attach-window wm proxy))

(define-handler (wm (river river-window-manager-v1) :output) (proxy)
  (attach-output wm proxy))

(define-handler (wm (river river-window-manager-v1) :seat) (proxy)
  (attach-seat wm proxy))

(define-handler (wm (river river-window-manager-v1) :manage-start) ()
  (unwind-protect (manage wm)
    (river-window-manager-v1.manage-finish river)))

(define-handler (wm (river river-window-manager-v1) :render-start) ()
  (unwind-protect (render wm)
    (river-window-manager-v1.render-finish river)))

(define-handler (wm (river river-window-manager-v1) :session-locked) ()
  (setf (wm-locked wm) t)
  (leave-submap wm)
  (dismiss-which-key wm))

(define-handler (wm (river river-window-manager-v1) :session-unlocked) ()
  (setf (wm-locked wm) nil))

(define-condition wm-role-unavailable (wl-error) ()
  (:report "river reports the WM role as unavailable (is another WM running?)"))

(define-handler (wm (river river-window-manager-v1) :unavailable) ()
  (error 'wm-role-unavailable))

(define-handler (wm (river river-window-manager-v1) :finished) ()
  (river-window-manager-v1.destroy river)
  (event-loop-stop (wm-loop wm)))

(defun check-cursor-theme (value)
  (unless (typep value '(or null (cons string (cons (and wire-int unsigned-byte) null))))
    (nucleotide-error "~S is not NIL or a theme and a size like (\"Adwaita\" 24)" value)))

(define-setting *cursor-theme* nil
  :documentation "NIL for the default of river, or (NAME SIZE): the XCursor theme
and size of the seat and of programs started afterwards."
  :check #'check-cursor-theme)

(defparameter *river-cursor-theme* '("default" 24)
  "The theme river uses until it gets another one.")

(defvar *inherited-cursor-environment* :unread
  "XCURSOR_THEME and XCURSOR_SIZE as nucleotide found them.")

(defun export-cursor-theme ()
  (when (eq *inherited-cursor-environment* :unread)
    (setf *inherited-cursor-environment*
          (mapcar #'uiop:getenv '("XCURSOR_THEME" "XCURSOR_SIZE"))))
  (loop for variable in '("XCURSOR_THEME" "XCURSOR_SIZE")
        for value in (if *cursor-theme*
                         (mapcar #'princ-to-string *cursor-theme*)
                         *inherited-cursor-environment*)
        do (if value
               (sb-posix:setenv variable value 1)
               (sb-posix:unsetenv variable))))

(defun set-cursor-theme (wm)
  (let ((seat (wm-seat wm)))
    (when (and seat (supports-p seat 2))
      (apply #'river-seat-v1.set-xcursor-theme seat
             (or *cursor-theme* *river-cursor-theme*)))))

(defun attach-seat (wm proxy)
  (when (wm-seat wm)
    (log-message :warn "multiple seats are not supported yet")
    (return-from attach-seat))
  (setf (wm-seat wm) proxy)
  (forward-events wm proxy)
  (when (wm-layer-shell wm)
    (let ((layer-shell (river-layer-shell-v1.get-seat (wm-layer-shell wm) proxy)))
      (setf (wm-layer-shell-seat wm) layer-shell)
      (forward-events wm layer-shell)))
  (when (and (wm-xkb wm) (supports-p (wm-xkb wm) 2))
    (let ((xkb-seat (river-xkb-bindings-v1.get-seat (wm-xkb wm) proxy)))
      (setf (wm-xkb-seat wm) xkb-seat)
      (forward-events wm xkb-seat)))
  (set-cursor-theme wm)
  (install-all-keybinds wm))

(define-handler (wm (seat river-seat-v1) :window-interaction) (window)
  (let ((win (find-window wm window)))
    (when (and win (window-visible-p wm win) (not (wm-highlight wm)))
      (focus-window wm win)
      (request wm :interaction)
      (mark-dirty wm))))

(define-handler (wm (seat river-seat-v1) :pointer-position) (x y)
  (setf (wm-pointer-position wm) (point x y)))

(define-handler (wm (seat river-xkb-bindings-seat-v1) :ate-unbound-key) ()
  (leave-submap wm))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-exclusive) ()
  (setf (wm-layer-shell-focus wm) :exclusive))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-non-exclusive) ()
  (setf (wm-layer-shell-focus wm) :non-exclusive))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-none) ()
  (setf (wm-layer-shell-focus wm) nil))

(defun bind-global (wm registry name interface version)
  (let ((entry (find interface *bound-globals*
                     :key (lambda (entry) (interface-wire-name (first entry)))
                     :test #'equal)))
    (when entry
      (destructuring-bind (class max-version accessor) entry
        (let ((proxy (wl-registry.bind registry name class
                                       (min max-version version))))
          (funcall (fdefinition `(setf ,accessor)) proxy wm)
          (forward-events wm proxy))))))

(defun make-wm (display)
  (let* ((workspaces (make-workspaces))
         (wm (make-instance 'wm :display display
                            :orphans workspaces
                            :active-workspace (first workspaces)))
         (registry (wl-display.get-registry display)))
    (setf (wm-registry wm) registry)
    (push (lambda (event &rest args)
            (when (eq event :global)
              (destructuring-bind (name interface version) args
                (if (string= interface "wl_output")
                    (setf (gethash name (wm-wl-output-versions wm)) version)
                    (bind-global wm registry name interface version)))))
          (proxy-hooks registry))
    wm))

(defun start-wm (&key display-name)
  "Run the window manager in a new thread."
  (when (and *wm* (wm-thread *wm*)
             (sb-thread:thread-alive-p (wm-thread *wm*)))
    (nucleotide-error "a WM is already running, call (stop-wm) first"))
  (load-user-config)
  (export-cursor-theme)
  (let* ((display (wl-display-connect display-name))
         (wm (make-wm display)))
    (wl-display-roundtrip display)
    (unless (wm-river wm)
      (wl-display-disconnect display)
      (nucleotide-error "no river_window_manager_v1 global, is WAYLAND_DISPLAY river?"))
    (setf (wm-loop wm) (make-event-loop display)
          (wm-thread wm) (sb-thread:make-thread
                          (lambda () (run-event-loop (wm-loop wm)))
                          :name "nucleotide-wm")
          *wm* wm)
    (handler-case (start-ipc wm)
      (error (c)
        (notify "no IPC: ~A" c)))
    (run-autostart)))

(defmacro in-wm (&body body)
  "Run BODY in the WM thread (asynchronously). REPL threads must never touch
proxies directly, so wrap all proxy access in this."
  `(progn
     (unless *wm*
       (nucleotide-error "nucleotide is not running"))
     (event-loop-post (wm-loop *wm*) (lambda () ,@body))))

(defun stop-wm ()
  "Stop the window manager that START-WM started. River has a second to answer
the stop request, and the event loop may have ended already."
  (when *wm*
    (let ((thread (wm-thread *wm*)))
      (handler-case
          (progn
            (in-wm (river-window-manager-v1.stop (wm-river *wm*)))
            (sb-thread:join-thread thread :default nil :timeout 1)
            (event-loop-stop (wm-loop *wm*)))
        (nucleotide-error ()))
      (sb-thread:join-thread thread :default nil))
    (stop-ipc *wm*)
    (wl-display-disconnect (wm-display *wm*))
    (setf *wm* nil)))
