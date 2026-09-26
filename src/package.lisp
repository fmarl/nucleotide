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

(defpackage #:nucleotide
  (:use #:cl)
  (:local-nicknames (#:xml #:nucleotide.xml))
  (:export
   ;; connection
   #:wl-display-connect
   #:wl-display-disconnect
   #:wl-display-roundtrip
   #:with-open-display
   #:dispatch-event
   ;; proxy
   #:wl-proxy
   #:proxy-id
   #:proxy-version
   #:proxy-display
   #:proxy-hooks
   #:proxy-destroyed-p
   #:make-proxy
   ;; conditions
   #:nucleotide-error
   #:wl-error
   #:wl-protocol-error
   #:wl-server-error
   #:wl-error-object
   #:wl-error-code
   #:wl-error-text
   #:wl-disconnected
   ;; protocol definition DSL
   #:define-interface
   #:define-request
   #:define-event
   #:define-protocol
   #:load-protocol
   ;; core protocol
   #:wl-display
   #:wl-registry
   #:wl-callback
   #:wl-display.sync
   #:wl-display.get-registry
   #:wl-registry.bind
   ;; event loop
   #:event-loop-post
   #:*debug-on-error*
   ;; model
   #:wm
   #:window
   #:workspace
   #:output
   #:output-name
   #:output-workspace
   #:output-workspaces
   #:wm-focused-output
   #:window-floating
   #:window-parent
   #:active-windows
   #:all-windows
   #:wm-focused-window
   #:wm-workspaces
   #:wm-active-workspace
   #:wm-outputs
   #:wm-highlight
   #:window-title
   #:window-app-id
   #:window-workspace
   #:workspace-name
   #:workspace-windows
   #:workspace-layout
   ;; events
   #:handle-event
   #:define-handler
   #:forward-events
   ;; window manager
   #:*wm*
   #:start-wm
   #:stop-wm
   #:in-wm
   #:reload-config
   #:cycle-focus
   #:move-window
   #:nth-workspace
   #:switch-workspace
   #:send-to-workspace
   #:close-focused
   #:set-active-layout
   #:focus-output
   #:send-to-output
   #:move-workspace-to-output
   #:toggle-floating
   #:window-switcher
   #:define-window-rule
   #:add-window-rule
   #:contains
   #:toggle-highlight
   #:toggle-fullscreen
   #:exit-session
   ;; layouts
   #:layout-arrange
   #:layout-neighbor
   #:layout-move
   #:layout-successor
   #:make-layout
   #:scrolling
   #:consume-or-expel-window
   #:cycle-column-width
   #:toggle-full-width
   #:center-column
   #:tiling
   #:monocle
   ;; keybinds
   #:spawn
   #:define-keybinds
   #:define-key-chords
   #:define-workspace-keybinds
   #:set-keybind
   #:remove-keybind
   #:set-key-chord
   #:remove-key-chord
   #:clear-keybinds
   #:bind-key
   #:bind-key-chord
   #:make-submap
   #:enter-submap
   #:leave-submap
   ;; configuration
   #:*focused-border-rgba*
   #:*unfocused-border-rgba*
   #:*border-width*
   #:*num-of-workspaces*
   #:*default-layout*
   #:*column-widths*
   #:*default-column-width*
   #:*libinput-settings*
   #:*keyboard-layout*
   #:*keyboard-variant*
   #:*keyboard-options*
   #:*keyboard-model*
   #:*numlock*
   #:*warp-pointer*
   #:*pointer-modifiers*
   #:*move-button*
   #:*resize-button*
   #:*menu-command*
   #:*autostart-programs*
   ;; repl
   #:*repl-port*
   #:*repl-idle-timeout*
   #:start-repl-server
   #:stop-repl-server
   #:toggle-repl-server
   ;; debug
   #:wl-debug-info))
