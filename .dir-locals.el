;;; Directory Local Variables  -*- no-byte-compile: t -*-
;;; Match the formatting of scripts/fmt.el.

((nil . ((indent-tabs-mode . nil)
         (fill-column . 100)))
 (lisp-mode . ((lisp-indent-function . common-lisp-indent-function)
               (eval . (progn
                         (require 'cl-indent)
                         ;; Keep in sync with scripts/fmt.el.
                         (dolist (spec '((defsystem . (4 &body))
                                         (define-command . (4 4 &body))
                                         (define-handler . (4 4 &body))
                                         (define-request . (4 &body))
                                         (define-event . (4 &body))
                                         (define-interface . (4 4 4))
                                         (test . (4 &body))
                                         (with-fresh-config . (&body))
                                         (with-rules . (&body))
                                         (with-environment . (4 &body))
                                         (with-private-directory . (4 &body))
                                         (with-config-home . (4 &body))
                                         (with-scrolling-wm . (4 &body))
                                         (with-ipc-connection . (4 &body))))
                           (put (car spec) 'common-lisp-indent-function (cdr spec)))
                         (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))))
