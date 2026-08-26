;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defvar *interface-wire-names* (make-hash-table))
(defvar *event-definitions* (make-hash-table :test #'equal))

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

(defun interface-wire-name (class-name)
  (gethash class-name *interface-wire-names*))

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


(defun encode-arg (buf off type value)
  (ecase type
    (:uint (setf (u32ref buf off) value) (+ off 4))
    (:int (setf (i32ref buf off) value) (+ off 4))
    (:fixed (setf (i32ref buf off) (round (* value 256))) (+ off 4))
    (:object (setf (u32ref buf off) (if value (proxy-id value) 0)) (+ off 4))
    (:string (put-wl-string buf off value))
    (:array (put-wl-array buf off value))
    (:fd off)))

(defun send-request (proxy opcode signature args)
  (when (proxy-destroyed-p proxy)
    (nucleotide-error "request (opcode ~D) -> destroyed proxy ~S" opcode proxy))
  (let* ((conn (display-connection (proxy-display proxy)))
         (buf (connection-wbuf conn))
         (end (reduce (lambda (off type-and-value)
                        (encode-arg buf off (car type-and-value) (cdr type-and-value)))
                      (mapcar #'cons signature args)
                      :initial-value 8)))
    (setf (u32ref buf 0) (proxy-id proxy)
          (u32ref buf 4) (logior (ash end 16) opcode))
    (send-raw conn end (loop for type in signature
                             for value in args
                             when (eq type :fd) collect value))))

(defun decode-arg (display parent buf off spec)
  (if (consp spec)
      (ecase (first spec)
        (:object
         (values (gethash (u32ref buf off) (display-proxies display))
                 (+ off 4)))
        (:new-id
         (values (make-proxy display (second spec)
                             :id (u32ref buf off)
                             :version (proxy-version parent))
                 (+ off 4))))
      (ecase spec
        (:uint (values (u32ref buf off) (+ off 4)))
        (:int (values (i32ref buf off) (+ off 4)))
        (:fixed (values (/ (i32ref buf off) 256) (+ off 4)))
        (:string (get-wl-string buf off))
        (:array (get-wl-array buf off)))))

(defun decode-args (display parent buf off specs)
  (when specs
    (multiple-value-bind (value next) (decode-arg display parent buf off (first specs))
      (cons value (decode-args display parent buf next (rest specs))))))

(defun event-definition (proxy opcode)
  (and proxy
       (gethash (cons (class-name (class-of proxy)) opcode) *event-definitions*)))

(defun decode-event (display proxy definition body-offset)
  (when definition
    (cons (car definition)
          (decode-args display proxy (connection-rbuf (display-connection display))
                       body-offset (cdr definition)))))

(defun run-hooks (proxy event)
  (unless (proxy-destroyed-p proxy)
    (dolist (hook (proxy-hooks proxy))
      (apply hook event))))

(defun dispatch-event (display)
  (let ((conn (display-connection display)))
    (multiple-value-bind (sender opcode body-offset body-size) (peek-message conn)
      (let* ((proxy (gethash sender (display-proxies display)))
             (definition (event-definition proxy opcode))
             ;; Consume the message even if decoding fails, otherwise the
             ;; same broken message would be dispatched again and again.
             (event (unwind-protect (decode-event display proxy definition body-offset)
                      (consume-message conn (+ 8 body-size)))))
        (cond (event (run-hooks proxy event))
              (proxy (warn "no event definition for opcode ~D on ~S" opcode proxy)))
        (values)))))

(defmacro define-interface (name wire-name)
  `(progn
     (defclass ,name (wl-proxy) ())
     (setf (gethash ',name *interface-wire-names*) ,wire-name)
     ',name))

(defmacro define-event ((interface event-name opcode) &body arg-specs)
  `(setf (gethash (cons ',interface ,opcode) *event-definitions*)
         (cons ,event-name ',(mapcar #'second arg-specs))))

(defun mappend (function list)
  (loop for x in list append (funcall function x)))

(defun request-arg-parts (spec proxy new-var interface-class version)
  "Parameters, wire types, values and new-proxy form for one request arg."
  (destructuring-bind (name type) spec
    (cond ((eq type :new-id)
           (list (list interface-class version)
                 '(:string :uint :uint)
                 `((interface-wire-name ,interface-class) ,version (proxy-id ,new-var))
                 `(make-proxy (proxy-display ,proxy) ,interface-class :version ,version)))
          ((and (consp type) (eq (first type) :new-id))
           (list '()
                 '(:uint)
                 `((proxy-id ,new-var))
                 `(make-proxy (proxy-display ,proxy) ',(second type)
                              :version (proxy-version ,proxy))))
          ((or (eq type :object) (and (consp type) (eq (first type) :object)))
           (list (list name) '(:object) (list name) nil))
          (t
           (list (list name) (list type) (list name) nil)))))

(defmacro define-request ((interface name opcode &key destructor) &body arg-specs)
  (let* ((proxy (gensym "PROXY"))
         (new-var (gensym "NEW-PROXY"))
         (interface-class (gensym "INTERFACE-CLASS"))
         (version (gensym "VERSION"))
         (parts (mapcar (lambda (spec)
                          (request-arg-parts spec proxy new-var interface-class version))
                        arg-specs))
         (new-proxy-forms (remove nil (mapcar #'fourth parts))))
    (when (rest new-proxy-forms)
      (nucleotide-error "only one new_id arg is supported"))
    `(defun ,(intern (concatenate 'string (symbol-name interface) "." (symbol-name name)))
         (,proxy ,@(mappend #'first parts))
       (let ((,new-var ,(first new-proxy-forms)))
         (declare (ignorable ,new-var))
         (send-request ,proxy ,opcode ',(mappend #'second parts)
                       (list ,@(mappend #'third parts)))
         ,@(when destructor
             `((setf (proxy-destroyed-p ,proxy) t)))
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
                       (nucleotide-error "XDG_RUNTIME_DIR is not set"))))
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

(defun supports-p (proxy version)
  (>= (proxy-version proxy) version))
