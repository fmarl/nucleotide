;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defvar *wm* nil
  "This is a window manager. Look at me in the REPL. :-)")

(defstruct (output-state (:conc-name output-))
  proxy (x 0) (y 0) (width 0) (height 0))

(defstruct (win (:conc-name win-))
  proxy node title app-id)

(defstruct (wm (:constructor make-wm-bare)
               (:conc-name wm-))
  display
  river
  seat
  (outputs '())
  (windows '())
  loop
  thread)

(defun handle-river-event (wm event &rest args)
  (case event
    (:window (attach-window wm (first args)))
    (:output (attach-output wm (first args)))
    (:seat (attach-seat wm (first args)))
    (:manage-start
     (manage wm)
     (river-window-manager-v1.manage-finish (wm-river wm)))
    (:render-start
     (render wm)
     (river-window-manager-v1.render-finish (wm-river wm)))
    (:unavailable
     (warn "river reports the WM role as unavailable (another WM running?)"))))

(defun attach-window (wm proxy)
  (let ((win (make-win :proxy proxy
                       :node (river-window-v1.get-node proxy))))
    (push win (wm-windows wm))          ; new windows take focus
    (push (lambda (&rest event) (apply #'handle-window-event wm win event))
          (proxy-hooks proxy))))

(defun handle-window-event (wm win event &rest args)
  (case event
    (:closed
     (setf (wm-windows wm) (remove win (wm-windows wm)))
     (river-node-v1.destroy (win-node win))
     (river-window-v1.destroy (win-proxy win)))
    (:title (setf (win-title win) (first args)))
    (:app-id (setf (win-app-id win) (first args)))))

(defun attach-output (wm proxy)
  (let ((output (make-output-state :proxy proxy)))
    (push output (wm-outputs wm))
    (push (lambda (&rest event) (apply #'handle-output-event wm output event))
          (proxy-hooks proxy))))

(defun handle-output-event (wm output event &rest args)
  (case event
    (:position
     (setf (output-x output) (first args)
           (output-y output) (second args)))
    (:dimensions
     (setf (output-width output) (first args)
           (output-height output) (second args)))
    (:removed
     (setf (wm-outputs wm) (remove output (wm-outputs wm)))
     (river-output-v1.destroy (output-proxy output)))))

(defun attach-seat (wm proxy)
  (if (wm-seat wm)
      (warn "multiple seats are not supported yet")
      (progn
        (setf (wm-seat wm) proxy)
        (push (lambda (&rest event) (apply #'handle-seat-event wm event))
              (proxy-hooks proxy)))))

(defun handle-seat-event (wm event &rest args)
  (case event
    (:window-interaction
     (let ((win (find (first args) (wm-windows wm) :key #'win-proxy)))
       (when win
         (focus-window wm win)
         (river-window-manager-v1.manage-dirty (wm-river wm)))))))

(defun focus-window (wm win)
  (setf (wm-windows wm) (cons win (remove win (wm-windows wm)))))

(defun manage (wm)
  (let ((output (first (wm-outputs wm))))
    (dolist (win (wm-windows wm))
      (river-window-v1.set-tiled (win-proxy win) #b1111)
      (when (and output (plusp (output-width output)))
        (river-window-v1.propose-dimensions (win-proxy win)
                                            (output-width output)
                                            (output-height output))))
    (let ((focused (first (wm-windows wm))))
      (when (and (wm-seat wm) focused)
        (river-seat-v1.focus-window (wm-seat wm) (win-proxy focused))))))

(defun render (wm)
  (let ((output (first (wm-outputs wm))))
    (dolist (win (reverse (wm-windows wm)))
      (river-window-v1.show (win-proxy win))
      (when output
        (river-node-v1.set-position (win-node win)
                                    (output-x output) (output-y output)))
      (river-node-v1.place-top (win-node win)))))

(defun make-wm (display)
  (let ((wm (make-wm-bare :display display))
        (registry (wl-display.get-registry display)))
    (push (lambda (event &rest args)
            (when (eq event :global)
              (destructuring-bind (name interface version) args
                (when (string= interface "river_window_manager_v1")
                  (let ((river (wl-registry.bind registry name
                                                 'river-window-manager-v1
                                                 (min 4 version))))
                    (setf (wm-river wm) river)
                    (push (lambda (&rest event)
                            (apply #'handle-river-event wm event))
                          (proxy-hooks river)))))))
          (proxy-hooks registry))
    wm))

(defun start-wm (&key display-name)
  "Connect to river, take the window manager role, and run in a new thread."
  (when (and *wm* (wm-thread *wm*)
             (sb-thread:thread-alive-p (wm-thread *wm*)))
    (error "a WM is already running; call (stop-wm) first"))
  (let* ((display (wl-display-connect display-name))
         (wm (make-wm display)))
    (wl-display-roundtrip display)
    (unless (wm-river wm)
      (wl-display-disconnect display)
      (error "no river_window_manager_v1 global, is WAYLAND_DISPLAY river?"))
    (setf (wm-loop wm) (make-event-loop display)
          (wm-thread wm) (sb-thread:make-thread
                          (lambda () (run-event-loop (wm-loop wm)))
                          :name "nucleotide-wm")
          *wm* wm)))

(defun stop-wm ()
  (when *wm*
    (event-loop-stop (wm-loop *wm*))
    (sb-thread:join-thread (wm-thread *wm*))
    (wl-display-disconnect (wm-display *wm*))
    (setf *wm* nil)))

(defmacro in-wm (&body body)
  "Run BODY in the WM thread (asynchronously). REPL threads must never touch
proxies directly; wrap all proxy access in this."
  `(event-loop-post (wm-loop *wm*) (lambda () ,@body)))

(defun start-repl-server (&key (port 4005))
  "Start a Slynk (SLY) or Swank (SLIME) server so Emacs can connect."
  (cond
    ((asdf:find-system "slynk" nil)
     (asdf:load-system "slynk")
     (uiop:symbol-call '#:slynk '#:create-server :port port :dont-close t))
    ((asdf:find-system "swank" nil)
     (asdf:load-system "swank")
     (uiop:symbol-call '#:swank '#:create-server :port port :dont-close t))
    (t (error "neither slynk nor swank is loadable"))))
