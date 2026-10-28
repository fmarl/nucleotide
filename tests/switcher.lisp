;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite switcher :in :nucleotide)
(in-suite switcher)

(test window-labels-are-single-lines
  (let ((win (open-window-on (make-instance 'n::workspace :name "1"))))
    (setf (n::%window-app-id win) "foot"
          (n::%window-title win) (format nil "a~%2: b~Cc" #\Tab))
    (is (equal "1: [1] foot - a 2: b c" (n::window-label win 1)))))
