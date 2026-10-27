;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; Classes rather than structures: redefining a class in the running WM
;;; updates the existing instances.

(defclass output ()
  ((proxy :initarg :proxy :accessor output-proxy)
   (layer-shell :initform nil :accessor output-layer-shell)
   (wl-output :initform nil :accessor output-wl-output)
   (name :initform nil :accessor output-name)
   (workspaces :initform '() :accessor output-workspaces)
   (workspace :initform nil :accessor output-workspace
              :documentation "The one of its WORKSPACES the output shows.")
   (x :initform 0 :accessor output-x)
   (y :initform 0 :accessor output-y)
   (width :initform 0 :accessor output-width)
   (height :initform 0 :accessor output-height)
   (usable-x :initform 0 :accessor output-usable-x)
   (usable-y :initform 0 :accessor output-usable-y)
   (usable-width :initform 0 :accessor output-usable-width)
   (usable-height :initform 0 :accessor output-usable-height)))

(defmethod print-object ((output output) stream)
  (print-unreadable-object (output stream :type t)
    (format stream "~@[~A ~]~Dx~D+~D+~D" (output-name output)
            (output-width output) (output-height output)
            (output-x output) (output-y output))))

(defvar *next-id* 0)

(defun next-id ()
  (incf *next-id*))

(defclass window ()
  ((id :initform (next-id) :reader window-id)
   (proxy :initarg :proxy :accessor window-proxy)
   (node :initarg :node :accessor window-node)
   (title :initform nil :accessor window-title)
   (app-id :initform nil :accessor window-app-id)
   (workspace :initarg :workspace :accessor window-workspace)
   (parent :initform nil :accessor window-parent)
   (closed :initform nil :accessor window-closed)
   (home :initform nil :accessor window-home
         :documentation "The workspace of a removed output the window returns to.")
   (x :initform 0 :accessor window-x)
   (y :initform 0 :accessor window-y)
   (width :initform 0 :accessor window-width)
   (height :initform 0 :accessor window-height)
   (min-width :initform 0 :accessor window-min-width)
   (min-height :initform 0 :accessor window-min-height)
   (max-width :initform 0 :accessor window-max-width)
   (max-height :initform 0 :accessor window-max-height)
   (clip :initform nil :accessor window-clip
         :documentation "NIL, :HIDDEN, or the (x y width height) box to clip to.")
   (floating :initform nil :accessor window-floating)
   (float-offset :initform nil :accessor window-float-offset
                 :documentation "Position relative to the output's usable area, NIL
until a floating window is placed.")
   (fullscreen :initform nil :accessor window-fullscreen
               :documentation "The output proxy while fullscreen, else NIL.")
   (pending-dimensions :initform nil :accessor window-pending-dimensions)
   (rules-applied :initform nil :accessor window-rules-applied)
   (configured :initform nil :accessor window-configured)))

(defmethod print-object ((win window) stream)
  (print-unreadable-object (win stream :type t :identity t)
    (format stream "~@[~A~]~@[ ~S~]" (window-app-id win) (window-title win))))

;;; Slots that hold windows are read through LIVE, so that closed windows drop
;;; out of them instead of being removed from each one.

(defun live (win)
  (and win (not (window-closed win)) win))

(defun live-windows (windows &key (key #'identity))
  (remove-if (lambda (item) (window-closed (funcall key item))) windows))

(defmethod window-parent :around ((win window))
  (live (call-next-method)))

(define-setting *default-layout* 'tiling
  :documentation "The layout of new workspaces, in the format MAKE-LAYOUT takes. Read at
startup.")

(defclass workspace ()
  ((id :initform (next-id) :reader workspace-id)
   (name :initarg :name :accessor workspace-name)
   (windows :initform '() :accessor workspace-windows)
   (focus :initform nil :accessor workspace-focus
          :documentation "The window that has the focus while the workspace is active and
nothing is highlighted.")
   (output :initform nil :accessor workspace-output)
   (layout :initform (make-layout *default-layout*) :accessor workspace-layout)))

(defmethod print-object ((ws workspace) stream)
  (print-unreadable-object (ws stream :type t)
    (format stream "~A" (workspace-name ws))))

(defclass submap ()
  ((bindings :initform '() :accessor submap-bindings)))

(defun make-submap ()
  (make-instance 'submap))

(defclass wm ()
  ((display :initarg :display :accessor wm-display)
   (registry :initform nil :accessor wm-registry)
   (wl-output-versions :initform (make-hash-table) :accessor wm-wl-output-versions)
   (river :initform nil :accessor wm-river)
   (seat :initform nil :accessor wm-seat)
   (layer-shell :initform nil :accessor wm-layer-shell)
   (layer-shell-seat :initform nil :accessor wm-layer-shell-seat)
   (layer-shell-focus :initform nil :accessor wm-layer-shell-focus)
   (xkb :initform nil :accessor wm-xkb)
   (xkb-seat :initform nil :accessor wm-xkb-seat)
   (input-manager :initform nil :accessor wm-input-manager)
   (libinput-config :initform nil :accessor wm-libinput-config)
   (outputs :initform '() :accessor wm-outputs)
   (orphans :initarg :orphans :accessor wm-orphans
            :documentation "Workspaces without an output, taken over by the next one.")
   (stash :initform '() :accessor wm-stash
          :documentation "Alist from the name of a removed output to its workspaces.")
   (active-workspace :initarg :active-workspace :accessor wm-active-workspace)
   (active-submap :initform nil :accessor wm-active-submap)
   (highlight :initform nil :accessor wm-highlight)
   (focus-history :initform '() :accessor wm-focus-history
                  :documentation "Windows from the most recently focused on.")
   (last-focus :initform nil :accessor wm-last-focus)
   (last-workspace :initform nil :accessor wm-last-workspace)
   (pointer-window :initform nil :accessor wm-pointer-window)
   (op :initform nil :accessor wm-op)
   (bindings :initform '() :accessor wm-bindings)
   ;; Requests that commands and events leave for the next manage sequence,
   ;; lists with the newest first. Every manage sequence carries out the other
   ;; state as it is.
   (pending-warp :initform nil :accessor wm-pending-warp)
   (pending-bindings :initform '() :accessor wm-pending-bindings)
   (pending-closes :initform '() :accessor wm-pending-closes)
   (pending-submap-ops :initform '() :accessor wm-pending-submap-ops)
   (pending-fullscreens :initform '() :accessor wm-pending-fullscreens)
   (event-loop :initform nil :accessor wm-loop)
   (thread :initform nil :accessor wm-thread)
   (ipc :initform nil :accessor wm-ipc)))

(defun active-windows (wm)
  (workspace-windows (wm-active-workspace wm)))

(defun (setf active-windows) (windows wm)
  (setf (workspace-windows (wm-active-workspace wm)) windows))

(defun wm-workspaces (wm)
  (append (mappend #'output-workspaces (wm-outputs wm))
          (wm-orphans wm)
          (mappend #'cdr (wm-stash wm))))

(defun all-windows (wm)
  (loop for ws in (wm-workspaces wm) append (workspace-windows ws)))

(defmethod wm-highlight :around ((wm wm))
  (live (call-next-method)))

(defmethod wm-focus-history :around ((wm wm))
  (live-windows (call-next-method)))

(defmethod wm-last-focus :around ((wm wm))
  (live (call-next-method)))

(defmethod wm-pointer-window :around ((wm wm))
  (live (call-next-method)))

(defmethod wm-pending-closes :around ((wm wm))
  (live-windows (call-next-method)))

(defmethod wm-pending-fullscreens :around ((wm wm))
  (live-windows (call-next-method) :key #'car))

(defun wm-focus (wm)
  "While a highlight is active, the highlighted window is the only visible one
and has the focus."
  (or (wm-highlight wm) (workspace-focus (wm-active-workspace wm))))

(defun wm-active-layout (wm)
  (workspace-layout (wm-active-workspace wm)))

(defun (setf wm-active-layout) (layout wm)
  (setf (workspace-layout (wm-active-workspace wm)) layout))

(defun wm-focused-output (wm)
  (workspace-output (wm-active-workspace wm)))

(defun find-window (wm proxy)
  (find proxy (all-windows wm) :key #'window-proxy))

(defun mark-dirty (wm)
  (river-window-manager-v1.manage-dirty (wm-river wm)))
