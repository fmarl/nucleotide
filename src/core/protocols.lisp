;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(setf (gethash 'wl-display *interface-wire-names*) "wl_display")

(define-interface wl-registry "wl_registry")
(define-interface wl-callback "wl_callback")

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

(define-interface wl-output "wl_output")

(define-request (wl-output release 0 :destructor t))

(define-event (wl-output :geometry 0)
  (x :int)
  (y :int)
  (physical-width :int)
  (physical-height :int)
  (subpixel :int)
  (make :string)
  (model :string)
  (transform :int))

(define-event (wl-output :mode 1)
  (flags :uint)
  (width :int)
  (height :int)
  (refresh :int))

(define-event (wl-output :done 2))

(define-event (wl-output :scale 3)
  (factor :int))

(define-event (wl-output :name 4)
  (name :string))

(define-event (wl-output :description 5)
  (description :string))
