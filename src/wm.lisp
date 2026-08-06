;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defvar *wm* nil
  "This is a window manager. Look at me in the REPL. :-)")

(defstruct (output-state (:conc-name output-))
  proxy layer-shell (x 0) (y 0) (width 0) (height 0)
  (usable-x 0) (usable-y 0) (usable-width 0) (usable-height 0))

(defstruct (win (:conc-name win-))
  proxy node title app-id workspace (x 0) (y 0))

(defstruct (workspace (:conc-name ws-))
  name
  (windows '())
  focused
  (layout 'tiling)
  output)

(defstruct (wm (:constructor make-wm-bare)
               (:conc-name wm-))
  display
  river
  seat
  layer-shell
  layer-shell-seat
  layer-shell-focus
  (outputs '())
  (workspaces '())
  (pending-bindings '())
  active-workspace
  xkb
  loop
  thread)

(defun wm-windows (wm)
  (ws-windows (wm-active-workspace wm)))

(defun (setf wm-windows) (windows wm)
  (setf (ws-windows (wm-active-workspace wm)) windows))

(defun wm-focused (wm)
  (ws-focused (wm-active-workspace wm)))

(defun (setf wm-focused) (focused wm)
  (setf (ws-focused (wm-active-workspace wm)) focused))

(defun wm-active-layout (wm)
  (ws-layout (wm-active-workspace wm)))

(defun (setf wm-active-layout) (layout wm)
  (setf (ws-layout (wm-active-workspace wm)) layout))

(defun make-workspaces ()
  (loop for i from 1 to 9
	collect (make-workspace :name (princ-to-string i))))

(defun get-usable-output (output)
  (if (plusp (output-usable-width output))
      (values (output-usable-x output) (output-usable-y output)
	      (output-usable-width output) (output-usable-height output))
      (values (output-x output) (output-y output)
	      (output-width output) (output-height output))))

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
     (warn "river reports the WM role as unavailable (is another WM running?)"))))

(defun attach-window (wm proxy)
  (let ((win (make-win :proxy proxy
                       :node (river-window-v1.get-node proxy)
		       :workspace (wm-active-workspace wm))))
    (push win (wm-windows wm))
    (focus-window wm win)
    (push (lambda (&rest event) (apply #'handle-window-event wm win event))
          (proxy-hooks proxy))))

(defun handle-window-event (wm win event &rest args)
  (case event
    (:closed
     (setf (wm-windows wm) (remove win (wm-windows wm)))
     (when (eq (wm-focused wm) win)
       (setf (wm-focused wm) (first (wm-windows wm))))
     (let ((ws (win-workspace win)))
       (setf (ws-windows ws) (remove win (ws-windows ws)))
       (when (eq (ws-focused ws) win)
	 (setf (ws-focused ws) (first (ws-windows ws)))))
     (river-node-v1.destroy (win-node win))
     (river-window-v1.destroy (win-proxy win)))
    (:title (setf (win-title win) (first args)))
    (:app-id (setf (win-app-id win) (first args)))))

(defun attach-output (wm proxy)
  (let ((output (make-output-state :proxy proxy)))
    (push output (wm-outputs wm))
    (push (lambda (&rest event) (apply #'handle-output-event wm output event))
          (proxy-hooks proxy))
    (when (wm-layer-shell wm)
      (let ((ls (river-layer-shell-v1.get-output (wm-layer-shell wm) proxy)))
	(setf (output-layer-shell output) ls)
	(push (lambda (&rest event)
		(apply #'handle-layer-shell-output-event wm output event))
	      (proxy-hooks ls))))))

(defun handle-layer-shell-output-event (wm output event &rest args)
  (declare (ignore wm))
  (when (eq event :non-exclusive-area)
    (destructuring-bind (x y width height) args
      (setf (output-usable-x output) x
	    (output-usable-y output) y
	    (output-usable-width output) width
	    (output-usable-height output) height))))

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
     (when (output-layer-shell output)
       (river-layer-shell-output-v1.destroy (output-layer-shell output)))
     (river-output-v1.destroy (output-proxy output)))))

(defun attach-seat (wm proxy)
  (if (wm-seat wm)
      (warn "multiple seats are not supported yet")
      (progn
        (setf (wm-seat wm) proxy)
        (push (lambda (&rest event) (apply #'handle-seat-event wm event))
              (proxy-hooks proxy))
	(when (wm-layer-shell wm)
	  (let ((ls (river-layer-shell-v1.get-seat (wm-layer-shell wm) proxy)))
	    (setf (wm-layer-shell-seat wm) ls)
	    (push (lambda (&rest event)
		    (apply #'handle-layer-shell-seat-event wm event))
		  (proxy-hooks ls))))
	(keybinds wm))))

(defun handle-layer-shell-seat-event (wm event &rest args)
  (declare (ignore args))
  (case event
    (:focus-exclusive (setf (wm-layer-shell-focus wm) :exclusive))
    (:focus-non-exclusive (setf (wm-layer-shell-focus wm) :non-exclusive))
    (:focus-none (setf (wm-layer-shell-focus wm) nil))))

(defun handle-seat-event (wm event &rest args)
  (case event
    (:window-interaction
     (let ((win (find (first args) (wm-windows wm) :key #'win-proxy)))
       (when win
         (focus-window wm win)
         (river-window-manager-v1.manage-dirty (wm-river wm)))))))

(defun focus-window (wm win)
  (setf (wm-focused wm) win))

(defun set-active-layout (wm layout)
  (setf (wm-active-layout wm) layout)
  (river-window-manager-v1.manage-dirty (wm-river wm)))

(defun manage (wm)
  (dolist (binding (wm-pending-bindings wm))
    (river-xkb-binding-v1.enable binding))
  (setf (wm-pending-bindings wm) '())
  (let ((output (first (wm-outputs wm))))
    (dolist (win (wm-windows wm))
      (river-window-v1.set-tiled (win-proxy win) #b1111)
      (river-window-v1.use-ssd (win-proxy win)))
    (when (and output (plusp (output-width output)))
      (multiple-value-bind (usable-x usable-y usable-width usable-height) (get-usable-output output)
	(loop for win in (wm-windows wm)
	      for (x y width height) in (funcall (wm-active-layout wm) (length (wm-windows wm))
						 usable-x usable-y usable-width usable-height)
	      do (setf (win-x win) x
		       (win-y win) y)
		 (river-window-v1.propose-dimensions (win-proxy win) width height))))
    (when (and output (output-layer-shell output))
      (river-layer-shell-output-v1.set-default (output-layer-shell output)))
    (let ((focused (wm-focused wm)))
      (when (and (wm-seat wm) focused (not (wm-layer-shell-focus wm)))
	(river-seat-v1.focus-window (wm-seat wm) (win-proxy focused))))))

(defun render (wm)
  (dolist (ws (wm-workspaces wm))
    (if (eq ws (wm-active-workspace wm))
	(dolist (win (ws-windows ws))
	  (river-window-v1.show (win-proxy win))
	  (river-window-v1.set-borders (win-proxy win) #b1111 2 0 #xffffffff #xffffffff #xffffffff)
	  (river-node-v1.set-position (win-node win) (win-x win) (win-y win))
	  (river-node-v1.place-top (win-node win)))
	(dolist (win (ws-windows ws))
	  (river-window-v1.hide (win-proxy win)))))
  (let ((focused (wm-focused wm)))
    (when focused
      (river-node-v1.place-top (win-node focused)))))

(defun make-wm (display)
  (let* ((workspaces (make-workspaces))
	 (wm (make-wm-bare :display display
			   :workspaces workspaces
			   :active-workspace (first workspaces)))
	 (registry (wl-display.get-registry display)))
    (push (lambda (event &rest args)
	    (when (eq event :global)
	      (destructuring-bind (name interface version) args
		(cond ((string= interface "river_window_manager_v1")
		       (let ((river (wl-registry.bind registry name
						      'river-window-manager-v1
						      (min 4 version))))
			 (setf (wm-river wm) river)
			 (push (lambda (&rest event)
				 (apply #'handle-river-event wm event))
			       (proxy-hooks river))))
		      ((string= interface "river_layer_shell_v1")
		       (setf (wm-layer-shell wm)
			     (wl-registry.bind registry name
					       'river-layer-shell-v1 1)))
		      ((string= interface "river_xkb_bindings_v1")
		       (setf (wm-xkb wm)
			     (wl-registry.bind registry name
					       'river-xkb-bindings-v1 2)))))))
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
