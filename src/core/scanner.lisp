;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defun lisp-name (wire-name)
  (substitute #\- #\_ (string-upcase wire-name)))

(defun lispify (wire-name)
  (intern (lisp-name wire-name) '#:nucleotide))

(defun lispify-keyword (wire-name)
  (intern (lisp-name wire-name) '#:keyword))

(defun enum-constant-name (interface enum entry)
  (intern (format nil "+~A-~A-~A+"
                  (lisp-name interface) (lisp-name enum) (lisp-name entry))
          '#:nucleotide))

(defun parse-enum-value (string)
  (if (and (> (length string) 2) (string-equal "0x" string :end2 2))
      (parse-integer string :start 2 :radix 16)
      (parse-integer string)))

(defparameter *wire-types*
  '(("uint" . :uint) ("int" . :int) ("fixed" . :fixed)
    ("string" . :string) ("array" . :array) ("fd" . :fd)))

(defun wire-type (type)
  (or (cdr (assoc type *wire-types* :test #'string=))
      (nucleotide-error "unknown arg type ~S" type)))

(defun request-arg-spec (arg)
  (let ((type (xml:attribute arg "type"))
        (interface (xml:attribute arg "interface")))
    (list (lispify (xml:attribute arg "name"))
          (cond ((string= type "new_id")
                 (if interface (list :new-id (lispify interface)) :new-id))
                ((string= type "object") :object)
                (t (wire-type type))))))

(defun event-arg-spec (arg)
  (let ((type (xml:attribute arg "type"))
        (interface (xml:attribute arg "interface")))
    (list (lispify (xml:attribute arg "name"))
          (cond ((string= type "new_id") (list :new-id (lispify interface)))
                ((string= type "object") '(:object))
                ((string= type "fd")
                 (nucleotide-error "fd arguments in events are not supported"))
                (t (wire-type type))))))

(defun enum-forms (interface-name enum)
  (loop for entry in (xml:children-named enum "entry")
        collect `(defconstant ,(enum-constant-name interface-name
                                                   (xml:attribute enum "name")
                                                   (xml:attribute entry "name"))
                   ,(parse-enum-value (xml:attribute entry "value")))))

(defun request-form (class request opcode)
  (let ((header (list* class (lispify (xml:attribute request "name")) opcode
                       (when (equal (xml:attribute request "type") "destructor")
                         '(:destructor t)))))
    `(define-request ,header
       ,@(mapcar #'request-arg-spec (xml:children-named request "arg")))))

(defun event-form (class event opcode)
  `(define-event (,class ,(lispify-keyword (xml:attribute event "name")) ,opcode)
     ,@(mapcar #'event-arg-spec (xml:children-named event "arg"))))

(defun interface-forms (interface)
  (let* ((wire-name (xml:attribute interface "name"))
         (class (lispify wire-name)))
    (append
     (loop for request in (xml:children-named interface "request")
           for opcode from 0
           collect (request-form class request opcode))
     (loop for event in (xml:children-named interface "event")
           for opcode from 0
           collect (event-form class event opcode))
     (mappend (lambda (enum) (enum-forms wire-name enum))
              (xml:children-named interface "enum")))))

(defun protocol-forms (root)
  (unless (string= "protocol" (xml:node-name root))
    (nucleotide-error "not a Wayland protocol: root element is <~A>"
                      (xml:node-name root)))
  (let ((interfaces (xml:children-named root "interface")))
    (append
     (mapcar (lambda (interface)
               `(define-interface ,(lispify (xml:attribute interface "name"))
                    ,(xml:attribute interface "name")))
             interfaces)
     (mappend #'interface-forms interfaces))))

(defun protocol-file-forms (pathname)
  (protocol-forms (xml:parse (uiop:read-file-string pathname))))

(defmacro define-protocol (file)
  "Define proxy classes, requests, events and enum constants for the Wayland
protocol in FILE at compile time. A relative FILE is looked up in the
protocol/ directory of the nucleotide system."
  `(progn
     ,@(protocol-file-forms
        (merge-pathnames file (asdf:system-relative-pathname
                               "nucleotide" "protocol/")))))

(defun load-protocol (pathname)
  "Like DEFINE-PROTOCOL, but at runtime, e.g. from the REPL."
  (eval `(progn ,@(protocol-file-forms pathname)))
  (values))
