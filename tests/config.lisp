;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite config :in :nucleotide)
(in-suite config)

(defmacro with-config-home ((file) &body body)
  (let ((dir (gensym)) (old (gensym)))
    `(let* ((,dir (format nil "/tmp/nucleotide-test-~D-~D/" (sb-posix:getpid) (random 100000)))
            (,file (format nil "~Anucleotide/init.lisp" ,dir))
            (,old (sb-posix:getenv "XDG_CONFIG_HOME")))
       (ensure-directories-exist ,file)
       (unwind-protect
            (progn (sb-posix:setenv "XDG_CONFIG_HOME" ,dir 1)
                   (with-fresh-config ,@body))
         (if ,old
             (sb-posix:setenv "XDG_CONFIG_HOME" ,old 1)
             (sb-posix:unsetenv "XDG_CONFIG_HOME"))
         (uiop:delete-directory-tree (pathname ,dir) :validate t)))))

(defun write-config (file &rest forms)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((*package* (find-package '#:nucleotide)))
      (dolist (form forms)
        (print form out)))))

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

(test colors
  (is (equal '(111 140 171 255) (n::parse-color "#6f8cab")))
  (is (equal '(111 140 171 128) (n::parse-color "#6f8cab80")))
  (signals n::nucleotide-error (n::parse-color "6f8cab"))
  (signals error (n::parse-color "#zzzzzz")))

(test hooks
  (with-fresh-config
    (let ((seen '()))
      (n::on :window-title (lambda (win title) (declare (ignore win title)) (error "boom")))
      (n::on :window-title (lambda (win title) (push (list win title) seen)))
      (with-stubs ((n::notify (lambda (&rest args) (declare (ignore args)))))
        (n::run-event-hooks :window-title :win "title"))
      (is (equal '((:win "title")) seen))
      (signals n::nucleotide-error (n::on :no-such-event #'identity)))))

(test focus-changes-run-hooks-and-fill-the-history
  (with-fresh-config
    (let* ((wm (make-test-wm))
           (a (make-instance 'n::window :workspace (ws wm 1)))
           (b (make-instance 'n::window :workspace (ws wm 1)))
           (seen '()))
      (n::on :focus-changed (lambda (win) (push win seen)))
      (n::note-focus wm a)
      (n::note-focus wm a)
      (n::note-focus wm b)
      (n::note-focus wm a)
      (is (equal (list a b a) seen))
      (is (equal (list a b) (n::wm-focus-history wm))))))

(test highlight-prefers-the-last-focused-match
  (let* ((wm (make-test-wm))
         (old (open-window-on (ws wm 1)))
         (new (open-window-on (ws wm 2))))
    (setf (n::window-app-id old) "emacs"
          (n::window-app-id new) "emacs"
          (n::wm-focus-history wm) (list new old))
    (with-stubs ((n::mark-dirty (lambda (wm) (declare (ignore wm)))))
      (n::toggle-highlight wm :app-id "emacs")
      (is (eq new (n::wm-highlight wm)))
      (n::toggle-highlight wm :app-id "emacs")
      (is (null (n::wm-highlight wm))))))

(test init-files-may-belong-to-root
  (if (zerop (sb-posix:getuid))
      (skip "running as root")
      (progn
        (signals n::nucleotide-error (n::check-file-permissions "/etc/passwd" #o022))
        (finishes (n::check-file-permissions "/etc/passwd" #o022 :allow-root t))
        (signals n::nucleotide-error
                 (n::check-file-permissions "/etc/passwd" #o044 :allow-root t)))))
