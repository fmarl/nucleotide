;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defclass output ()
  ((proxy :initarg :proxy :accessor output-proxy)
   (layer-shell :initform nil :accessor output-layer-shell)
   (wl-output :initform nil :accessor output-wl-output)
   (name :initform nil :accessor output-name)
   (workspaces :initform '() :accessor output-workspaces)
   (workspace :initform nil :accessor output-workspace
              :documentation "The one of its WORKSPACES the output shows.")
   (rect :initform (rect 0 0 0 0) :accessor output-rect)
   (usable :initform nil :accessor output-usable
           :documentation "The part of RECT that layer shell surfaces leave, once announced.")))

(defmethod print-object ((output output) stream)
  (print-unreadable-object (output stream :type t)
    (destructuring-bind (x y width height) (rect-list (output-rect output))
      (format stream "~@[~A ~]~Dx~D+~D+~D" (output-name output) width height x y))))

(defvar *next-id* 0)

(defun new-id ()
  (incf *next-id*))

(defclass window ()
  ((id :initform (new-id) :reader window-id)
   (proxy :initarg :proxy :reader window-proxy :writer (setf %window-proxy))
   (node :initarg :node :reader window-node :writer (setf %window-node))
   (title :initform nil :reader window-title :writer (setf %window-title))
   (app-id :initform nil :reader window-app-id :writer (setf %window-app-id))
   (workspace :initform nil :reader window-workspace :writer (setf %window-workspace))
   (parent :initform nil :reader window-parent :writer (setf %window-parent))
   (closed :initform nil :reader window-closed-p :writer (setf %window-closed-p))
   (home :initform nil :reader window-home :writer (setf %window-home)
         :documentation "The workspace of a removed output the window returns to.")
   (x :initform 0 :reader window-x :writer (setf %window-x))
   (y :initform 0 :reader window-y :writer (setf %window-y))
   (width :initform 0 :reader window-width :writer (setf %window-width))
   (height :initform 0 :reader window-height :writer (setf %window-height))
   (min-width :initform 0 :reader window-min-width :writer (setf %window-min-width))
   (min-height :initform 0 :reader window-min-height :writer (setf %window-min-height))
   (max-width :initform 0 :reader window-max-width :writer (setf %window-max-width))
   (max-height :initform 0 :reader window-max-height :writer (setf %window-max-height))
   (clip :initform nil :reader window-clip :writer (setf %window-clip)
         :documentation "NIL, :HIDDEN, or the rect to clip to.")
   (floating :initform nil :reader window-floating-p :writer (setf %window-floating-p))
   (float-offset :initform nil :reader window-float-offset :writer (setf %window-float-offset)
                 :documentation "The point relative to the output's usable area, NIL
until a floating window is placed.")
   (anchor :initform nil :reader window-anchor :writer (setf %window-anchor)
           :documentation "The anchor of the last resize. It keeps the edges the resize
did not move in place until the window moves.")
   (fullscreen :initform nil :reader window-fullscreen :writer (setf %window-fullscreen)
               :documentation "The output proxy while fullscreen, else NIL.")
   (maximized :initform nil :reader window-maximized-p :writer (setf %window-maximized-p))
   (restore-size :initform nil :reader window-restore-size :writer (setf %window-restore-size)
                 :documentation "The size a maximized window had, NIL if unknown.")
   (decoration-hint :initform nil :reader window-decoration-hint
                    :writer (setf %window-decoration-hint))
   (pending-size :initform nil :reader window-pending-size :writer (setf %window-pending-size)
                 :documentation "The size to propose to a floating window.")
   (rules-applied :initform nil :reader window-rules-applied-p :writer (setf %window-rules-applied-p))))

(defmethod print-object ((win window) stream)
  (print-unreadable-object (win stream :type t :identity t)
    (format stream "~@[~A~]~@[ ~S~]" (window-app-id win) (window-title win))))

(defun live (win)
  (and win (not (window-closed-p win)) win))

(defun live-windows (windows &key (key #'identity))
  (remove-if (lambda (item) (window-closed-p (funcall key item))) windows))

(defun window-origin (win)
  (point (window-x win) (window-y win)))

(defun window-size (win)
  (size (window-width win) (window-height win)))

(defun window-rect (win)
  (rect-at (window-origin win) (window-size win)))

(defun position-window (win origin clip)
  (setf (%window-x win) (point-x origin)
        (%window-y win) (point-y origin)
        (%window-clip win) clip))

(defun window-placement (win)
  "How WIN is placed: :FULLSCREEN, :MAXIMIZED, :FLOATING or :TILED, the first that
applies. The states are independent, so a window returns to the next one when it
leaves one of them."
  (cond ((window-fullscreen win) :fullscreen)
        ((window-maximized-p win) :maximized)
        ((window-floating-p win) :floating)
        (t :tiled)))

(defmethod window-parent :around ((win window))
  (live (call-next-method)))

(define-setting *default-layout* 'tiling
  :documentation "The layout of new workspaces, in the format MAKE-LAYOUT takes. Read at
startup.")

(defclass workspace ()
  ((id :initform (new-id) :reader workspace-id)
   (name :initarg :name :accessor workspace-name)
   (windows :initform '() :reader workspace-windows :writer (setf %workspace-windows))
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

(defclass proxies ()
  ((display :initarg :display :accessor wm-display)
   (registry :initform nil :accessor wm-registry)
   (wl-output-versions :initform (make-hash-table) :accessor wm-wl-output-versions)
   (river :initform nil :accessor wm-river)
   (seat :initform nil :accessor wm-seat)
   (layer-shell :initform nil :accessor wm-layer-shell)
   (layer-shell-seat :initform nil :accessor wm-layer-shell-seat)
   (xkb :initform nil :accessor wm-xkb)
   (xkb-seat :initform nil :accessor wm-xkb-seat)
   (input-manager :initform nil :accessor wm-input-manager)
   (libinput-config :initform nil :accessor wm-libinput-config)))

(defclass runtime ()
  ((event-loop :initform nil :accessor wm-loop)
   (thread :initform nil :accessor wm-thread)
   (ipc :initform nil :accessor wm-ipc)))

(defclass wm (proxies runtime)
  ((outputs :initform '() :accessor wm-outputs)
   (orphans :initarg :orphans :accessor wm-orphans
            :documentation "Workspaces without an output, taken over by the next one.")
   (stash :initform '() :accessor wm-stash
          :documentation "Alist from the name of a removed output to its workspaces.")
   (active-workspace :initarg :active-workspace :accessor wm-active-workspace)
   (active-submap :initform nil :accessor wm-active-submap)
   (layer-shell-focus :initform nil :accessor wm-layer-shell-focus)
   (highlight :initform nil :accessor wm-highlight)
   (focus-history :initform '() :accessor wm-focus-history
                  :documentation "Windows from the most recently focused on.")
   (last-focus :initform nil :accessor wm-last-focus)
   (last-workspace :initform nil :accessor wm-last-workspace)
   (pointer-window :initform nil :accessor wm-pointer-window)
   (pointer-position :initform nil :accessor wm-pointer-position)
   (op :initform nil :accessor wm-op)
   (bindings :initform '() :accessor wm-bindings
             :documentation "The bindings outside of SUBMAPS.")
   (submaps :initform '() :accessor wm-submaps)
   (locked :initform nil :accessor wm-locked)
   (bindings-locked :initform nil :accessor wm-bindings-locked
                    :documentation "Whether the bindings are disabled for a locked session.
Manage sequences bring this in line with LOCKED.")
   (requests :initform '() :accessor wm-requests
             :documentation "What the next manage sequence does, the newest first.")))

(defun wm-workspaces (wm)
  (append (mappend #'output-workspaces (wm-outputs wm))
          (wm-orphans wm)
          (mappend #'cdr (wm-stash wm))))

(defun all-windows (wm)
  "The windows of all workspaces."
  (loop for ws in (wm-workspaces wm) append (workspace-windows ws)))

(defmethod wm-highlight :around ((wm wm))
  (live (call-next-method)))

(defmethod wm-focus-history :around ((wm wm))
  (live-windows (call-next-method)))

(defmethod wm-last-focus :around ((wm wm))
  (live (call-next-method)))

(defmethod wm-pointer-window :around ((wm wm))
  (live (call-next-method)))

(defun request (wm kind &rest args)
  (push (cons kind args) (wm-requests wm)))

(defun requests (wm kinds)
  "The requests of KINDS, (kind . arguments), the oldest first."
  (reverse (remove-if-not (lambda (request) (member (car request) kinds))
                          (wm-requests wm))))

(defun drop-requests (wm predicate)
  (let* ((requests (wm-requests wm))
         (kept (remove-if predicate requests)))
    (setf (wm-requests wm) kept)
    (/= (length kept) (length requests))))

(defun cancel-requests (wm kinds &key about)
  "Drop the requests of KINDS, only those about the window ABOUT if given.
Returns whether there were any."
  (drop-requests wm (lambda (request)
                      (and (member (car request) kinds)
                           (or (null about) (member about (cdr request)))))))

(defun forget-window-requests (wm win)
  (drop-requests wm (lambda (request) (member win (cdr request)))))

(defun take-requests (wm kinds)
  (prog1 (requests wm kinds)
    (cancel-requests wm kinds)))

(defmacro drain-requests ((wm) &body clauses)
  "Take the requests of the kinds that CLAUSES, (kind lambda-list . body), name
and run the clause of each with its arguments, the oldest first."
  (let ((request (gensym "REQUEST")))
    `(dolist (,request (take-requests ,wm ',(mapcar #'first clauses)))
       (ecase (car ,request)
         ,@(loop for (kind lambda-list . body) in clauses
                 collect `(,kind (destructuring-bind ,lambda-list (cdr ,request)
                                   ,@body)))))))

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
  "Ask river for a manage sequence, in which changes take effect."
  (river-window-manager-v1.manage-dirty (wm-river wm)))
