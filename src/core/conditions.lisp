;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-condition nucleotide-error (simple-error) ())

(defun nucleotide-error (control &rest args)
  (error 'nucleotide-error :format-control control :format-arguments args))

(define-condition wl-error (nucleotide-error) ())

(define-condition wl-protocol-error (wl-error) ()
  (:documentation "The server sent something we cannot parse."))

(define-condition wl-disconnected (wl-error) ()
  (:report "the Wayland server closed the connection"))

(define-condition wl-server-error (wl-error)
  ((object :initarg :object :reader wl-error-object)
   (code :initarg :code :reader wl-error-code)
   (text :initarg :text :reader wl-error-text))
  (:report (lambda (c stream)
             (format stream "Wayland server error on ~S (code ~D): ~A"
                     (wl-error-object c)
                     (wl-error-code c)
                     (wl-error-text c)))))
