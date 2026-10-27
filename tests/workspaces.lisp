;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite workspaces :in :nucleotide)
(in-suite workspaces)

(test the-focus-is-a-window-of-the-workspace
  (let ((a (make-instance 'n::workspace :name "a"))
        (b (make-instance 'n::workspace :name "b")))
    (let ((win (open-window-on a)))
      (signals error (setf (n::workspace-focus b) win))
      (signals error (n::add-window b win))
      (n::move-to-workspace win b)
      (is (eq b (n::window-workspace win)))
      (is (null (n::workspace-windows a)))
      (is (null (n::workspace-focus a)))
      (is (eq win (n::workspace-focus b))))))

(test removed-windows-belong-to-no-workspace
  (let* ((ws (make-instance 'n::workspace :name "test"))
         (win (open-window-on ws)))
    (n::remove-window win)
    (is (null (n::window-workspace win)))
    (is (null (n::workspace-windows ws)))))

(test migrated-windows-remember-their-home
  (let* ((from (make-instance 'n::workspace :name "from"))
         (to (make-instance 'n::workspace :name "to"))
         (win (open-window-on from)))
    (n::migrate-windows from to :home from)
    (is (eq to (n::window-workspace win)))
    (is (eq from (n::window-home win)))
    (is (eq win (n::workspace-focus to)))
    (n::return-home win)
    (is (eq from (n::window-workspace win)))
    (is (null (n::window-home win)))))
