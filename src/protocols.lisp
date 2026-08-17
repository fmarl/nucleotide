;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(setf (gethash 'wl-display *interface-info*) (cons "wl_display" 1)
      (gethash "wl_display" *interface-by-wire-name*) 'wl-display)

(define-interface wl-registry "wl_registry" 1)
(define-interface wl-callback "wl_callback" 1)

(define-request (wl-display sync 0)
    (callback (:new-id wl-callback)))

(define-request (wl-display get-registry 1)
    (registry (:new-id wl-registry)))

(define-event (wl-display :error 0)
    (object (:object))
  (code :uint)
  (message :string))

(define-event (wl-display :delete-id 1)
    (id :uint))

(define-request (wl-registry bind 0)
    (name :uint)
  (id :new-id))

(define-event (wl-registry :global 0)
    (name :uint)
  (interface :string)
  (version :uint))

(define-event (wl-registry :global-remove 1)
    (name :uint))

(define-event (wl-callback :done 0)
    (callback-data :uint))
