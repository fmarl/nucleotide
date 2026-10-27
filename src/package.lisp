;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(defpackage #:nucleotide.xml
  (:use #:cl)
  (:export
   #:parse
   #:xml-error
   #:node-name
   #:node-attrs
   #:node-children
   #:attribute
   #:children-named))

(defpackage #:nucleotide.json
  (:use #:cl)
  (:export
   #:parse
   #:encode
   #:bool
   #:lookup
   #:json-error))

(defpackage #:nucleotide
  (:use #:cl)
  (:local-nicknames (#:xml #:nucleotide.xml)
                    (#:json #:nucleotide.json))
  (:export
   ;; Configuration
   #:bind
   #:unbind
   #:set-binding
   #:rule
   #:contains
   #:on
   #:define-command
   #:use-defaults
   ;; Settings
   #:*autostart-programs*
   #:*border-focused*
   #:*border-unfocused*
   #:*border-width*
   #:*column-widths*
   #:*default-column-width*
   #:*default-layout*
   #:*gap*
   #:*libinput-settings*
   #:*menu-command*
   #:*move-button*
   #:*num-of-workspaces*
   #:*pointer-modifiers*
   #:*repl-port*
   #:*resize-button*
   #:*warp-pointer*
   #:*window-heights*
   ;; Commands
   #:center-column
   #:close-window
   #:consume-or-expel
   #:cycle-column-width
   #:cycle-window-height
   #:exit-session
   #:focus
   #:focus-output
   #:move
   #:move-workspace-to-output
   #:reload-config
   #:reset-window-height
   #:send-to-output
   #:send-to-workspace
   #:set-column-width
   #:set-layout
   #:set-window-height
   #:spawn
   #:switch-window
   #:switch-workspace
   #:toggle-floating
   #:toggle-full-width
   #:toggle-fullscreen
   #:toggle-highlight
   #:toggle-repl
   ;; Layouts
   #:layout
   #:tiling
   #:monocle
   #:scrolling
   #:layout-update
   #:layout-arrange
   #:layout-neighbor
   #:layout-move
   #:layout-successor
   #:rect
   #:rect-x
   #:rect-y
   #:rect-width
   #:rect-height
   ;; Windows and workspaces
   #:*wm*
   #:in-wm
   #:mark-dirty
   #:notify
   #:all-windows
   #:focus-window
   #:wm-focus
   #:wm-active-workspace
   #:window-app-id
   #:window-title
   #:window-workspace
   #:workspace-name
   #:workspace-windows
   #:workspace-focus
   ;; Running
   #:start-wm
   #:stop-wm
   #:start-repl-server
   #:stop-repl-server
   #:*debug-on-error*
   #:*log-level*))

(defpackage #:nucleotide.user
  (:use #:cl #:nucleotide)
  (:documentation "The package of the init file."))
