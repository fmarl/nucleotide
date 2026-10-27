;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite config :in :nucleotide)
(in-suite config)

(defmacro with-config-home ((file) &body body)
  (let ((home (gensym)))
    `(with-private-directory (,home)
       (let ((,file (format nil "~A/nucleotide/init.lisp" ,home)))
         (ensure-directories-exist ,file)
         (sb-posix:chmod (directory-namestring ,file) #o755)
         (with-environment (("XDG_CONFIG_HOME" (format nil "~A/" ,home)))
           (with-fresh-config ,@body))))))

(defun write-config (file &rest forms)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((*package* (find-package '#:nucleotide)))
      (dolist (form forms)
        (print form out))))
  (sb-posix:chmod file #o644))

(test without-an-init-file-the-defaults-apply
  (with-config-home (file)
    (is (null (n::load-user-config)))
    (is (< 50 (length n::*bindings*)))))

(test reloading-starts-from-scratch
  (with-config-home (file)
    (write-config file
                  '(n::bind "Super+A" n::close-window)
                  '(n::rule (:app-id "mpv") :float t)
                  '(setf n::*gap* 3))
    (is (eq t (n::load-user-config)))
    (is (= 1 (length n::*bindings*)))
    (is (= 1 (length n::*window-rules*)))
    (is (= 3 n::*gap*))
    (write-config file '(n::use-defaults))
    (n::load-user-config)
    (is (null (action "Super+A")))
    (is (null n::*window-rules*))
    (is (= 8 n::*gap*))))

(test a-broken-init-file-keeps-the-configuration
  (with-config-home (file)
    (write-config file '(n::bind "Super+A" n::close-window))
    (n::load-user-config)
    (write-config file '(setf n::*gap* 3) '(n::bind "Super+B" (n::no-such-command)))
    (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
      (is (eq :error (let ((*error-output* (make-broadcast-stream)))
                       (n::load-user-config)))))
    (is (equal '(n::close-window) (action "Super+A")))
    (is (= 8 n::*gap*))))

(test settings-are-checked
  (with-config-home (file)
    (write-config file '(setf n::*border-focused* "blue"))
    (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
      (is (eq :error (n::load-user-config))))
    (is (equal "#6f8cab" n::*border-focused*))))

(test reloading-forgets-the-commands-of-the-init-file
  (unwind-protect
       (with-config-home (file)
         (write-config file
                       '(n::define-command n::test-config-command (wm) (declare (ignore wm)))
                       '(n::bind "Super+A" n::test-config-command))
         (is (eq t (n::load-user-config)))
         (is (member 'n::test-config-command n::*commands*))
         (write-config file '(n::bind "Super+A" n::test-config-command))
         (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
           (is (eq :error (let ((*error-output* (make-broadcast-stream)))
                            (n::load-user-config)))))
         (write-config file '(n::use-defaults))
         (n::load-user-config)
         (is (not (member 'n::test-config-command n::*commands*)))
         (is (member 'n::close-window n::*commands*)))
    (fmakunbound 'n::test-config-command)))

(test an-init-file-in-a-writable-directory-is-ignored
  (with-config-home (file)
    (write-config file '(n::bind "Super+A" n::close-window))
    (sb-posix:chmod (namestring (uiop:pathname-directory-pathname file)) #o775)
    (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
      (is (eq :error (n::load-user-config))))
    (is (null (action "Super+A")))))

(test init-files-may-belong-to-root
  (if (zerop (sb-posix:getuid))
      (skip "running as root")
      (progn
        (signals n::nucleotide-error (n::check-file-permissions "/etc/passwd" #o022))
        (finishes (n::check-file-permissions "/etc/passwd" #o022 :allow-root t))
        (signals n::nucleotide-error
                 (n::check-file-permissions "/etc/passwd" #o044 :allow-root t)))))

(test (bad-sizes-fail-when-they-are-bound :fixture fresh-config)
  (signals n::nucleotide-error (n::set-binding "Super+Minus" '(n::set-column-width "-10")))
  (is (equal "Super+Minus" (n::set-binding "Super+Minus" '(n::set-column-width "-10%")))))
