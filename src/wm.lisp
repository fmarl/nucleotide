;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defvar *wm* nil
  "This is a window manager. Look at me in the REPL. :-)")

(defconstant +window-capabilities+ 4)

(defstruct (output-state (:conc-name output-))
  proxy layer-shell (x 0) (y 0) (width 0) (height 0)
  (usable-x 0) (usable-y 0) (usable-width 0) (usable-height 0))

(defstruct (win (:conc-name win-))
  proxy node title app-id workspace (x 0) (y 0) fullscreen)

(defstruct (workspace (:conc-name ws-))
  name
  (windows '())
  focused
  (layout 'tiling)
  output)

(defstruct (submap (:conc-name submap-))
  (bindings '()))

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
  (pending-closes '())
  (pending-submap-ops '())
  (pending-fullscreens '())
  active-workspace
  active-submap
  highlight
  xkb
  xkb-seat
  loop
  thread)

(defun wm-windows (wm)
  (ws-windows (wm-active-workspace wm)))

(defun wm-windows-global (wm)
  (mapcan (lambda (ws) (copy-list (ws-windows ws))) (wm-workspaces wm)))

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
  (loop for i from 1 to *num-of-workspaces*
	collect (make-workspace :name (princ-to-string i))))

(defun exit (wm)
  (river-window-manager-v1.exit-session (wm-river wm)))

(defun enter-submap (wm submap)
  (setf (wm-active-submap wm) submap)
  (push (cons :enter submap) (wm-pending-submap-ops wm))
  (river-window-manager-v1.manage-dirty (wm-river wm)))

(defun leave-submap (wm)
  (let ((submap (wm-active-submap wm)))
    (when submap
      (setf (wm-active-submap wm) nil)
      (push (cons :leave submap) (wm-pending-submap-ops wm))
      (river-window-manager-v1.manage-dirty (wm-river wm)))))

(defun manage-submap (wm)
  (dolist (op (nreverse (wm-pending-submap-ops wm)))
    (destructuring-bind (kind . submap) op
      (ecase kind
	(:enter
	 (dolist (binding (submap-bindings submap))
	   (river-xkb-binding-v1.enable binding))
	 (river-xkb-bindings-seat-v1.ensure-next-key-eaten (wm-xkb-seat wm)))
	(:leave
	 (dolist (binding (submap-bindings submap))
	   (river-xkb-binding-v1.disable binding))))))
  (setf (wm-pending-submap-ops wm) '()))

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
     (unwind-protect
	  (manage wm)
       (river-window-manager-v1.manage-finish (wm-river wm))))
    (:render-start
     (unwind-protect
	  (render wm)
       (river-window-manager-v1.render-finish (wm-river wm))))
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

(defun detach-window (wm win)
  (let ((ws (win-workspace win)))
    (setf (ws-windows ws) (remove win (ws-windows ws)))
    (when (eq (ws-focused ws) win)
      (setf (ws-focused ws) (first (ws-windows ws)))))
  (when (eq (wm-highlight wm) win)
    (setf (wm-highlight wm) nil))
  (setf (wm-pending-closes wm) (remove win (wm-pending-closes wm))
	(wm-pending-fullscreens wm) (remove win (wm-pending-fullscreens wm) :key #'car))
  (river-node-v1.destroy (win-node win))
  (river-window-v1.destroy (win-proxy win)))

(defun handle-window-event (wm win event &rest args)
  (case event
    (:closed (detach-window wm win))
    (:title (setf (win-title win) (first args)))
    (:app-id (setf (win-app-id win) (first args)))
    (:fullscreen-requested
     (push (cons win (first args)) (wm-pending-fullscreens wm)))
    (:exit-fullscreen-requested
     (push (cons win :exit) (wm-pending-fullscreens wm)))))

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
	(when (and (wm-xkb wm) (>= (proxy-version (wm-xkb wm)) 2))
	  (let ((xkb-seat (river-xkb-bindings-v1.get-seat (wm-xkb wm) proxy)))
	    (setf (wm-xkb-seat wm) xkb-seat)
	    (push (lambda (&rest event)
		    (apply #'handle-xkb-seat-event wm event))
		  (proxy-hooks xkb-seat))))
	(keybinds wm))))

(defun handle-xkb-seat-event (wm event &rest args)
  (declare (ignore args))
  (when (eq event :ate-unbound-key)
    (leave-submap wm)))

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

(defun set-active-layout (wm layout)
  (setf (wm-active-layout wm) layout)
  (river-window-manager-v1.manage-dirty (wm-river wm)))

(defun manage-fullscreens (wm)
  (dolist (entry (nreverse (wm-pending-fullscreens wm)))
    (destructuring-bind (win . request) entry
      (if (eq request :exit)
	  (when (win-fullscreen win)
	    (river-window-v1.exit-fullscreen (win-proxy win))
	    (river-window-v1.inform-not-fullscreen (win-proxy win))
	    (setf (win-fullscreen win) nil))
	  (let ((output (or request
			    (let ((o (first (wm-outputs wm))))
			      (and o (output-proxy o))))))
	    (when output
	      (river-window-v1.fullscreen (win-proxy win) output)
	      (river-window-v1.inform-fullscreen (win-proxy win))
	      (setf (win-fullscreen win) output))))))
  (setf (wm-pending-fullscreens wm) '()))

(defun manage-bindings (wm)
  (dolist (binding (wm-pending-bindings wm))
    (river-xkb-binding-v1.enable binding))
  (setf (wm-pending-bindings wm) '()))

(defun manage-closes (wm)
  (dolist (win (wm-pending-closes wm))
    (river-window-v1.close (win-proxy win)))
  (setf (wm-pending-closes wm) '()))

(defun manage (wm)
  (manage-bindings wm)
  (manage-closes wm)
  (manage-submap wm)
  (manage-fullscreens wm)
  (let ((output (first (wm-outputs wm))))
    (dolist (win (wm-windows wm))
      (river-window-v1.set-tiled (win-proxy win) #b1111)
      (river-window-v1.use-ssd (win-proxy win))
      (river-window-v1.set-capabilities (win-proxy win) +window-capabilities+))
    (when (and output (plusp (output-width output)))
      (multiple-value-bind (usable-x usable-y usable-width usable-height) (get-usable-output output)
	(let ((tiled (remove-if #'win-fullscreen (wm-windows wm))))
	  (loop for win in tiled
		for (x y width height) in (funcall (wm-active-layout wm) (length tiled)
						   usable-x usable-y usable-width usable-height)
		do (setf (win-x win) x
			 (win-y win) y)
		   (river-window-v1.propose-dimensions (win-proxy win) width height)))))
    (when (and output (output-layer-shell output))
      (river-layer-shell-output-v1.set-default (output-layer-shell output)))
    (let ((focused (wm-focused wm)))
      (when (and (wm-seat wm) focused (not (wm-layer-shell-focus wm)))
	(river-seat-v1.focus-window (wm-seat wm) (win-proxy focused))))))

(defun rgba->uint32 (rgba)
  (mapcar (lambda (x)
            (round (* x (/ #xffffffff 255))))
          rgba))

(defun render-show (win wm)
  (river-window-v1.show (win-proxy win))
  (let ((rgba (if (eq (wm-focused wm) win)
		  *focused-border-rgba*
		  *unfocused-border-rgba*)))
    (destructuring-bind (r g b a) (rgba->uint32 rgba)
      (river-window-v1.set-borders (win-proxy win) #b1111 2 r g b a)))
  (river-node-v1.set-position (win-node win) (win-x win) (win-y win))
  (river-node-v1.place-top (win-node win)))

(defun window-visible-p (wm win)
  (let ((highlight (wm-highlight wm)))
    (if highlight
	(eq win highlight)
	(eq (win-workspace win) (wm-active-workspace wm)))))

(defun render (wm)
  (dolist (win (wm-windows-global wm))
    (if (window-visible-p wm win)
	(render-show win wm)
	(river-window-v1.hide (win-proxy win))))
  (let ((focused (wm-focused wm)))
    (when (and focused (window-visible-p wm focused))
      (river-node-v1.place-top (win-node focused))))
  (dolist (win (wm-windows wm))
    (when (and (win-fullscreen win) (window-visible-p wm win))
      (river-node-v1.place-top (win-node win)))))

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
					       'river-layer-shell-v1 (min 1 version))))
		      ((string= interface "river_xkb_bindings_v1")
		       (setf (wm-xkb wm)
			     (wl-registry.bind registry name
					       'river-xkb-bindings-v1 (min 2 version))))))))
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
	  *wm* wm)
    (run-autostart)))

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
