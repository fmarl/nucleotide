;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *wm* nil
  "This is a window manager. Look at me in the REPL. :-)")

(defparameter *bound-globals*
  '((river-window-manager-v1 4 wm-river)
    (river-layer-shell-v1 1 wm-layer-shell)
    (river-xkb-bindings-v1 2 wm-xkb)
    (river-input-manager-v1 1 wm-input-manager)
    (river-libinput-config-v1 1 wm-libinput-config)
    (river-xkb-config-v1 1 wm-xkb-config))
  "The globals we bind: proxy class, highest version we support, and the WM
accessor that holds the proxy.")

(defparameter *num-of-workspaces* 9
  "Read once at startup.")

(defun make-workspaces ()
  (loop for i from 1 to *num-of-workspaces*
        collect (make-instance 'workspace :name (princ-to-string i))))

(defun exit-session (wm)
  (river-window-manager-v1.exit-session (wm-river wm)))

(defun enter-submap (wm submap)
  (setf (wm-active-submap wm) submap)
  (push (cons :enter submap) (wm-pending-submap-ops wm))
  (mark-dirty wm))

(defun leave-submap (wm)
  (let ((submap (wm-active-submap wm)))
    (when submap
      (setf (wm-active-submap wm) nil)
      (push (cons :leave submap) (wm-pending-submap-ops wm))
      (mark-dirty wm))))

;;; river_window_manager_v1

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

(define-handler (wm (river river-window-manager-v1) :unavailable) ()
  (warn "river reports the WM role as unavailable (is another WM running?)"))

;;; Windows

(defun attach-window (wm proxy)
  (let ((win (make-instance 'window :proxy proxy
                            :node (river-window-v1.get-node proxy)
                            :workspace (wm-active-workspace wm))))
    (push win (active-windows wm))
    (focus-window wm win)
    (forward-events wm proxy win)))

(defun detach-window (wm win)
  (remove-window (window-workspace win) win)
  (when (eq (wm-highlight wm) win)
    (setf (wm-highlight wm) nil))
  (when (eq (wm-pointer-window wm) win)
    (setf (wm-pointer-window wm) nil))
  (when (and (wm-op wm) (eq (op-win (wm-op wm)) win))
    (setf (op-state (wm-op wm)) :end))
  (setf (wm-pending-closes wm) (remove win (wm-pending-closes wm))
        (wm-pending-fullscreens wm) (remove win (wm-pending-fullscreens wm)
                                            :key #'car))
  (river-node-v1.destroy (window-node win))
  (river-window-v1.destroy (window-proxy win)))

(define-handler (wm (win window) :closed) ()
  (detach-window wm win))

(define-handler (wm (win window) :title) (title)
  (setf (window-title win) title))

(define-handler (wm (win window) :app-id) (app-id)
  (setf (window-app-id win) app-id))

(define-handler (wm (win window) :parent) (parent)
  (setf (window-parent win) (and parent (find-window wm parent))))

(define-handler (wm (win window) :dimensions) (width height)
  (setf (window-width win) width
        (window-height win) height))

(define-handler (wm (win window) :dimensions-hint) (min-width min-height max-width max-height)
  (setf (window-min-width win) min-width
        (window-min-height win) min-height
        (window-max-width win) max-width
        (window-max-height win) max-height))

(define-handler (wm (win window) :fullscreen-requested) (output)
  (push (cons win output) (wm-pending-fullscreens wm)))

(define-handler (wm (win window) :exit-fullscreen-requested) ()
  (push (cons win :exit) (wm-pending-fullscreens wm)))

;;; Outputs

(defun attach-output (wm proxy)
  (let ((output (make-instance 'output :proxy proxy)))
    (setf (wm-outputs wm) (append (wm-outputs wm) (list output)))
    (assign-output wm output)
    (forward-events wm proxy output)
    (when (wm-layer-shell wm)
      (let ((layer-shell (river-layer-shell-v1.get-output (wm-layer-shell wm)
                                                          proxy)))
        (setf (output-layer-shell output) layer-shell)
        (forward-events wm layer-shell output)))))

(define-handler (wm (output output) :wl-output) (global-name)
  (let ((version (gethash global-name (wm-wl-output-versions wm))))
    (when (and version (>= version 4))
      (let ((wl-output (wl-registry.bind (wm-registry wm) global-name 'wl-output 4)))
        (setf (output-wl-output output) wl-output)
        (forward-events wm wl-output output)))))

(define-handler (wm (output output) :name) (name)
  (setf (output-name output) name)
  (restore-output wm output)
  (mark-dirty wm))

(define-handler (wm (output output) :position) (x y)
  (setf (output-x output) x
        (output-y output) y))

(define-handler (wm (output output) :dimensions) (width height)
  (setf (output-width output) width
        (output-height output) height))

(define-handler (wm (output output) :non-exclusive-area) (x y width height)
  (setf (output-usable-x output) x
        (output-usable-y output) y
        (output-usable-width output) width
        (output-usable-height output) height))

(define-handler (wm (output output) :removed) ()
  (unassign-output wm output)
  (dolist (win (all-windows wm))
    (when (eq (window-fullscreen win) (output-proxy output))
      (push (cons win :exit) (wm-pending-fullscreens wm))))
  (mark-dirty wm)
  (when (output-wl-output output)
    (wl-output.release (output-wl-output output)))
  (when (output-layer-shell output)
    (river-layer-shell-output-v1.destroy (output-layer-shell output)))
  (river-output-v1.destroy (output-proxy output)))

;;; Seats

(defun attach-seat (wm proxy)
  (when (wm-seat wm)
    (warn "multiple seats are not supported yet")
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
  (install-all-keybinds wm))

(define-handler (wm (seat river-seat-v1) :window-interaction) (window)
  (let ((win (find-window wm window)))
    (when (and win (window-visible-p wm win) (not (wm-highlight wm)))
      (focus-window wm win)
      (mark-dirty wm))))

(define-handler (wm (seat river-xkb-bindings-seat-v1) :ate-unbound-key) ()
  (leave-submap wm))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-exclusive) ()
  (setf (wm-layer-shell-focus wm) :exclusive))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-non-exclusive) ()
  (setf (wm-layer-shell-focus wm) :non-exclusive))

(define-handler (wm (seat river-layer-shell-seat-v1) :focus-none) ()
  (setf (wm-layer-shell-focus wm) nil))

;;; Lifecycle

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
  "Connect to river, take the window manager role, and run in a new thread."
  (when (and *wm* (wm-thread *wm*)
             (sb-thread:thread-alive-p (wm-thread *wm*)))
    (nucleotide-error "a WM is already running; call (stop-wm) first"))
  (load-user-config)
  (let* ((display (wl-display-connect display-name))
         (wm (make-wm display)))
    (wl-display-roundtrip display)
    (unless (wm-river wm)
      (wl-display-disconnect display)
      (nucleotide-error "no river_window_manager_v1 global, is WAYLAND_DISPLAY river?"))
    (configure-keyboards wm)
    (setf (wm-loop wm) (make-event-loop display)
          (wm-thread wm) (sb-thread:make-thread
                          (lambda () (run-event-loop (wm-loop wm)))
                          :name "nucleotide-wm")
          *wm* wm)
    (run-autostart)))

(defun stop-wm ()
  (when *wm*
    (let ((thread (wm-thread *wm*)))
      (when (sb-thread:thread-alive-p thread)
        (event-loop-stop (wm-loop *wm*))
        (sb-thread:join-thread thread :default nil)))
    (wl-display-disconnect (wm-display *wm*))
    (setf *wm* nil)))

(defmacro in-wm (&body body)
  "Run BODY in the WM thread (asynchronously). REPL threads must never touch
proxies directly; wrap all proxy access in this."
  `(progn
     (unless *wm*
       (nucleotide-error "nucleotide is not running"))
     (event-loop-post (wm-loop *wm*) (lambda () ,@body))))
