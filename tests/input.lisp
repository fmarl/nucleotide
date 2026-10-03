;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite input :in :nucleotide)
(in-suite input)

(test libinput-values
  (is (= 1 (n::libinput-value "tap_state" t)))
  (is (= 0 (n::libinput-value "natural_scroll_state" nil)))
  (is (= 2 (n::libinput-value "click_method" :clickfinger)))
  (is (= 1 (n::libinput-value "scroll_method" :two-finger)))
  (is (= 1 (n::libinput-value "accel_profile" :flat))))

(test accel-speed-is-a-little-endian-double
  (is (equalp (octets 0 0 0 0 0 0 #xe0 #x3f) (n::double-octets 0.5)))
  (is (equalp (octets 0 0 0 0 0 0 #xf0 #xbf) (n::double-octets -1))))

(sb-alien:define-alien-type nil
    (sb-alien:struct test-msghdr
                     (name sb-alien:system-area-pointer)
                     (namelen sb-alien:unsigned-int)
                     (iov (* (sb-alien:struct n::iovec)))
                     (iovlen sb-alien:unsigned-long)
                     (control sb-alien:system-area-pointer)
                     (controllen sb-alien:unsigned-long)
                     (flags sb-alien:int)))

(defun receive-fd (socket-fd)
  "recvmsg one message and return the first fd passed with it."
  (let ((data (buffer 64))
        (control (buffer 64)))
    (sb-alien:with-alien ((iov (sb-alien:struct n::iovec))
                          (message (sb-alien:struct test-msghdr)))
      (sb-sys:with-pinned-objects (data control)
        (setf (sb-alien:slot iov 'n::base) (sb-sys:vector-sap data)
              (sb-alien:slot iov 'n::len) 64
              (sb-alien:slot message 'name) (sb-sys:int-sap 0)
              (sb-alien:slot message 'namelen) 0
              (sb-alien:slot message 'iov) (sb-alien:addr iov)
              (sb-alien:slot message 'iovlen) 1
              (sb-alien:slot message 'control) (sb-sys:vector-sap control)
              (sb-alien:slot message 'controllen) 64
              (sb-alien:slot message 'flags) 0)
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "recvmsg"
                                (function sb-alien:long sb-alien:int
                                          (* (sb-alien:struct test-msghdr))
                                          sb-alien:int))
         socket-fd (sb-alien:addr message) 0)
        (values (n::u32ref control 16) (subseq data 0 4))))))

(test fds-are-passed-with-the-message
  (let* ((path (format nil "/tmp/nucleotide-test-~D" (sb-posix:getpid)))
         (server (make-instance 'sb-bsd-sockets:local-socket :type :stream))
         (client (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind server path)
           (sb-bsd-sockets:socket-listen server 1)
           (sb-bsd-sockets:socket-connect client path)
           (let ((accepted (sb-bsd-sockets:socket-accept server))
                 (fd (n::keymap-fd "keymap")))
             (unwind-protect
                  (progn
                    (n::send-with-fds (sb-bsd-sockets:socket-file-descriptor client)
                                      (octets 1 2 3 4) 4 (list fd))
                    (multiple-value-bind (received data)
                        (receive-fd (sb-bsd-sockets:socket-file-descriptor accepted))
                      (is (equalp (octets 1 2 3 4) data))
                      (let ((content (buffer 7)))
                        (sb-posix:lseek received 0 sb-posix:seek-set)
                        (sb-sys:with-pinned-objects (content)
                          (sb-posix:read received (sb-sys:vector-sap content) 7))
                        (is (equalp (sb-ext:string-to-octets "keymap" :null-terminate t)
                                    content)))
                      (sb-posix:close received)))
               (sb-posix:close fd)
               (sb-bsd-sockets:socket-close accepted))))
      (sb-bsd-sockets:socket-close client)
      (sb-bsd-sockets:socket-close server)
      (ignore-errors (delete-file path)))))
