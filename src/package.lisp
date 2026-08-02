;; SPDX-License-Identifier: GPL-3.0-or-later

(defpackage #:nucleotide.xml
  (:use #:cl)
  (:export
   #:parse
   #:node-name
   #:node-attrs
   #:node-children
   #:attr
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
   ;; condition
   #:wl-server-error
   #:wl-error-object
   #:wl-error-code
   #:wl-error-text
   #:wl-disconnected
   ;; protocol definition DSL
   #:define-interface
   #:define-request
   #:define-event
   ;; core protocol
   #:wl-display
   #:wl-registry
   #:wl-callback
   #:wl-display.sync
   #:wl-display.get-registry
   #:wl-registry.bind
   ;; protocol scanner
   #:load-protocol
   ;; event loop
   #:event-loop-post
   #:*debug-on-error*
   ;; window manager
   #:*wm*
   #:start-wm
   #:stop-wm
   #:in-wm
   #:start-repl-server
   #:cycle-focus
   #:move-window
   ;; keybinds
   #:spawn
   #:bind-key
   ;; debug
   #:wl-debug-info))
