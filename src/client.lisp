;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defvar *interface-info* (make-hash-table))
(defvar *interface-by-wire-name* (make-hash-table :test #'equal))
(defvar *event-defs* (make-hash-table :test #'equal))

(defclass wl-proxy ()
  ((id :initarg :id :reader proxy-id)
   (%display :initarg :display :initform nil)
   (version :initarg :version :initform 1 :reader proxy-version)
   (hooks :initform nil :accessor proxy-hooks)
   (destroyedp :initform nil :accessor proxy-destroyed-p)))

(defmethod proxy-display ((proxy wl-proxy))
  (slot-value proxy '%display))

(defmethod print-object ((proxy wl-proxy) stream)
  (print-unreadable-object (proxy stream :type t)
    (format stream ":ID ~D" (proxy-id proxy))))

(defclass wl-display (wl-proxy)
  ((connection :initarg :connection :reader display-connection)
   (proxies :initform (make-hash-table) :reader display-proxies)
   (next-id :initform 2 :accessor display-next-id)
   (free-ids :initform nil :accessor display-free-ids)))

(defmethod proxy-display ((display wl-display))
  display)

(define-condition wl-server-error (error)
  ((object :initarg :object :reader wl-error-object)
   (code :initarg :code :reader wl-error-code)
   (text :initarg :text :reader wl-error-text))
  (:report (lambda (c stream)
             (format stream "Wayland server error on ~S (code ~D): ~A"
                     (wl-error-object c)
                     (wl-error-code c)
                     (wl-error-text c)))))

(defun interface-wire-name (class-name)
  (car (gethash class-name *interface-info*)))

(defun allocate-id (display)
  (or (pop (display-free-ids display))
      (let ((id (display-next-id display)))
        (assert (< id #xff000000))
        (incf (display-next-id display))
        id)))

(defun make-proxy (display class &key id (version 1))
  (let* ((id (or id (allocate-id display)))
         (proxy (make-instance class :id id :display display :version version)))
    (setf (gethash id (display-proxies display)) proxy)
    proxy))

(defun send-request (proxy opcode signature args)
  (let* ((conn (display-connection (proxy-display proxy)))
         (buf (connection-wbuf conn))
         (off 8))
    (loop for type in signature
          for value in args
          do (setf off
                   (ecase type
                     (:uint (setf (u32ref buf off) value) (+ off 4))
                     (:int (setf (i32ref buf off) value) (+ off 4))
                     (:fixed (setf (i32ref buf off) (round (* value 256)))
		      (+ off 4))
                     (:object (setf (u32ref buf off)
                                    (if value (proxy-id value) 0))
		      (+ off 4))
                     (:string (put-wl-string buf off value))
                     (:array (put-wl-array buf off value))
                     (:fd (error "fd arguments are not implemented yet")))))
    (setf (u32ref buf 0) (proxy-id proxy)
          (u32ref buf 4) (logior (ash off 16) opcode))
    (send-raw conn off)))

(defun decode-arg (display parent buf off spec)
  (cond
    ((eq spec :uint) (values (u32ref buf off) (+ off 4)))
    ((eq spec :int) (values (i32ref buf off) (+ off 4)))
    ((eq spec :fixed) (values (/ (i32ref buf off) 256) (+ off 4)))
    ((eq spec :string) (get-wl-string buf off))
    ((eq spec :array) (get-wl-array buf off))
    ((and (consp spec) (eq (first spec) :object))
     (let ((id (u32ref buf off)))
       (values (cond ((zerop id) nil)
                     ((gethash id (display-proxies display)))
                     (t id))
               (+ off 4))))
    ((and (consp spec) (eq (first spec) :new-id))
     (values (make-proxy display (second spec)
                         :id (u32ref buf off)
                         :version (proxy-version parent))
             (+ off 4)))
    (t (error "unsupported event arg spec ~S" spec))))

(defun dispatch-event (display)
  (let ((conn (display-connection display)))
    (multiple-value-bind (sender opcode body-off body-size) (peek-message conn)
      (let* ((proxy (gethash sender (display-proxies display)))
             (def (and proxy
                       (gethash (cons (class-name (class-of proxy)) opcode)
                                *event-defs*)))
             (event nil))
        (when def
          (let ((buf (connection-rbuf conn))
                (off body-off)
                (args '()))
            (dolist (spec (cdr def))
              (multiple-value-bind (value next)
                  (decode-arg display proxy buf off spec)
                (push value args)
                (setf off next)))
            (setf event (cons (car def) (nreverse args)))))
        (consume-message conn (+ 8 body-size))
        (when (and proxy (null def))
          (warn "no event definition for opcode ~D on ~S" opcode proxy))
        (when event
          (dolist (hook (proxy-hooks proxy))
            (apply hook event)))
        (values)))))

(defun dispatch-pending (display)
  (let ((conn (display-connection display)))
    (%fill-read-buffer conn)
    (loop while (buffered-message-size conn)
          do (dispatch-event display))
    (values)))

(defmacro define-interface (name wire-name version)
  `(progn
     (defclass ,name (wl-proxy) ())
     (setf (gethash ',name *interface-info*) (cons ,wire-name ,version)
           (gethash ,wire-name *interface-by-wire-name*) ',name)
     ',name))

(defmacro define-event ((interface event-name opcode) &body arg-specs)
  `(setf (gethash (cons ',interface ,opcode) *event-defs*)
         (cons ,event-name ',(mapcar #'second arg-specs))))

(defmacro define-request ((interface name opcode &key destructor) &body arg-specs)
  (let ((fname (intern (concatenate 'string
                                    (symbol-name interface) "."
                                    (symbol-name name))))
        (new-var (gensym "NEW-PROXY"))
        (lambda-args '())
        (sig '())
        (vals '())
        (new-proxy-form nil))
    (dolist (spec arg-specs)
      (destructuring-bind (arg-name type) spec
        (declare (ignorable arg-name))
        (cond
          ((eq type :new-id)
           (when new-proxy-form (error "only one new_id arg is supported"))
           (setf new-proxy-form
                 '(make-proxy (proxy-display proxy) interface-class
		   :version version))
           (setf lambda-args (append lambda-args '(interface-class version))
                 sig (append sig '(:string :uint :uint))
                 vals (append vals `((interface-wire-name interface-class)
                                     version
                                     (proxy-id ,new-var)))))
          ((and (consp type) (eq (first type) :new-id))
           (when new-proxy-form (error "only one new_id arg is supported"))
           (setf new-proxy-form
                 `(make-proxy (proxy-display proxy) ',(second type)
                              :version (proxy-version proxy)))
           (setf sig (append sig '(:uint))
                 vals (append vals `((proxy-id ,new-var)))))
          ((or (eq type :object)
               (and (consp type) (eq (first type) :object)))
           (setf lambda-args (append lambda-args (list arg-name))
                 sig (append sig '(:object))
                 vals (append vals (list arg-name))))
          (t
           (setf lambda-args (append lambda-args (list arg-name))
                 sig (append sig (list type))
                 vals (append vals (list arg-name)))))))
    `(defun ,fname (proxy ,@lambda-args)
       (let ((,new-var ,new-proxy-form))
         (declare (ignorable ,new-var))
         (send-request proxy ,opcode ',sig (list ,@vals))
         ,@(when destructor
             '((setf (proxy-destroyed-p proxy) t)))
         ,new-var))))

(defun handle-display-event (display event &rest args)
  (case event
    (:error
     (destructuring-bind (object code message) args
       (error 'wl-server-error :object object :code code :text message)))
    (:delete-id
     (destructuring-bind (id) args
       (remhash id (display-proxies display))
       (when (< id #xff000000)
         (push id (display-free-ids display)))))))

(defun %display-from-socket (socket)
  (let* ((conn (make-wire-connection socket))
         (display (make-instance 'wl-display :id 1 :version 1
                                             :connection conn)))
    (setf (gethash 1 (display-proxies display)) display)
    (push (lambda (&rest event) (apply #'handle-display-event display event))
          (proxy-hooks display))
    display))

(defun wl-socket-path (&optional name)
  (let ((name (or name (sb-ext:posix-getenv "WAYLAND_DISPLAY") "wayland-0")))
    (if (char= #\/ (char name 0))
        name
        (let ((dir (or (sb-ext:posix-getenv "XDG_RUNTIME_DIR")
                       (error "XDG_RUNTIME_DIR is not set"))))
          (concatenate 'string dir "/" name)))))

(defun wl-display-connect (&optional name)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket (wl-socket-path name))
    (%display-from-socket socket)))

(defun wl-display-disconnect (display)
  (close-connection (display-connection display))
  (clrhash (display-proxies display))
  (values))

(defmacro with-open-display ((var &rest connect-args) &body body)
  `(let ((,var (wl-display-connect ,@connect-args)))
     (unwind-protect (progn ,@body)
       (wl-display-disconnect ,var))))

(defun wl-display-roundtrip (display)
  (let ((done nil)
        (callback (wl-display.sync display)))
    (push (lambda (event &rest args)
            (declare (ignore args))
            (when (eq event :done)
              (setf done t)))
          (proxy-hooks callback))
    (loop until done do (dispatch-event display))
    (values)))
