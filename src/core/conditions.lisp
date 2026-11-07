;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-condition nucleotide-error (error) ()
  (:documentation "The base class of the errors nucleotide signals."))

(define-condition simple-nucleotide-error (nucleotide-error simple-condition) ()
  (:documentation "A function of nucleotide was called in a way it does not support."))

(defun nucleotide-error (control &rest args)
  "Signal a NUCLEOTIDE-ERROR whose message is CONTROL formatted with ARGS."
  (error 'simple-nucleotide-error :format-control control :format-arguments args))

(define-condition environment-error (nucleotide-error simple-condition) ()
  (:documentation "Something nucleotide needs from the system is missing or unsafe, like
an environment variable, a private file or a free port."))

(defun environment-error (control &rest args)
  "Signal an ENVIRONMENT-ERROR whose message is CONTROL formatted with ARGS."
  (error 'environment-error :format-control control :format-arguments args))

(define-condition event-loop-stopped (nucleotide-error) ()
  (:report "the event loop is not running"))

(define-condition wl-error (nucleotide-error) ())

(define-condition wl-protocol-error (wl-error simple-condition) ()
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
