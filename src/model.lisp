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

(defclass window ()
  ((proxy :initarg :proxy :accessor window-proxy)
   (node :initarg :node :accessor window-node)
   (title :initform nil :accessor window-title)
   (app-id :initform nil :accessor window-app-id)
   (workspace :initarg :workspace :accessor window-workspace)
   (parent :initform nil :accessor window-parent)
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

(defparameter *default-layout* 'tiling
  "The layout of new workspaces: a layout function or the name of a layout class.")

(defclass workspace ()
  ((name :initarg :name :accessor workspace-name)
   (windows :initform '() :accessor workspace-windows)
   (focused :initform nil :accessor workspace-focused)
   (output :initform nil :accessor workspace-output
           :documentation "The output the workspace belongs to.")
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
   (xkb-config :initform nil :accessor wm-xkb-config)
   (keyboards :initform '() :accessor wm-keyboards)
   (keymap :initform nil :accessor wm-keymap)
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
   (pointer-window :initform nil :accessor wm-pointer-window)
   (op :initform nil :accessor wm-op)
   (pending-warp :initform nil :accessor wm-pending-warp)
   (bindings :initform '() :accessor wm-bindings)
   (pending-bindings :initform '() :accessor wm-pending-bindings)
   (pending-closes :initform '() :accessor wm-pending-closes)
   (pending-submap-ops :initform '() :accessor wm-pending-submap-ops)
   (pending-fullscreens :initform '() :accessor wm-pending-fullscreens)
   (event-loop :initform nil :accessor wm-loop)
   (thread :initform nil :accessor wm-thread)))

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

(defun wm-focused-window (wm)
  (workspace-focused (wm-active-workspace wm)))

(defun (setf wm-focused-window) (focused wm)
  (setf (workspace-focused (wm-active-workspace wm)) focused))

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
