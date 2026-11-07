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

(defpackage #:nucleotide.wayland
  (:use #:cl)
  (:local-nicknames (#:xml #:nucleotide.xml))
  (:documentation "A Wayland client with an event loop, and the scanner that
generates protocol bindings from XML.")
  (:export
   ;; Errors and logging
   #:nucleotide-error
   #:environment-error
   #:event-loop-stopped
   #:wl-error
   #:*log-level*
   #:log-message
   #:*debug-on-error*
   #:call-reporting-errors
   #:skip
   ;; Wire format
   #:octets
   #:u32ref
   #:wire-int
   #:wire-fixed
   ;; Display and proxies
   #:wl-display-connect
   #:wl-display-roundtrip
   #:wl-display-disconnect
   #:wl-display.get-registry
   #:wl-registry.bind
   #:wl-output
   #:wl-output.release
   #:supports-p
   #:proxy-hooks
   #:interface-wire-name
   ;; Protocol bindings
   #:define-protocol
   #:enum-constant-string
   ;; Event loop
   #:event-loop
   #:make-event-loop
   #:run-event-loop
   #:event-loop-post
   #:event-loop-schedule
   #:event-loop-cancel
   #:event-loop-stop
   #:loop-failure))

(defpackage #:nucleotide
  (:use #:cl #:nucleotide.wayland)
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
   #:*bindings-layout*
   #:*border-focused*
   #:*border-unfocused*
   #:*border-width*
   #:*column-widths*
   #:*cursor-theme*
   #:*default-column-width*
   #:*default-layout*
   #:*focus-follows-pointer*
   #:*gap*
   #:*key-repeat*
   #:*libinput-settings*
   #:*move-button*
   #:*num-of-workspaces*
   #:*open-on-pointer-output*
   #:*pointer-modifiers*
   #:*repl-port*
   #:*resize-button*
   #:*scroll-factor*
   #:*warp-pointer*
   #:*which-key-delay*
   #:*which-key-modifier*
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
   #:switch-workspace
   #:toggle-floating
   #:toggle-full-width
   #:toggle-fullscreen
   #:toggle-highlight
   #:toggle-maximize
   #:toggle-overview
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
   #:*log-level*
   ;; Errors
   #:nucleotide-error
   #:config-error
   #:config-error-context
   #:invalid-value
   #:invalid-value-value
   #:invalid-value-expected
   #:unknown-name
   #:unknown-name-kind
   #:unknown-name-name
   #:unknown-name-known
   #:environment-error
   #:event-loop-stopped
   #:wm-not-running))

(defpackage #:nucleotide.user
  (:use #:cl #:nucleotide)
  (:documentation "The package of the init file."))
