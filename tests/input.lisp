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

(test init-files-may-belong-to-root
  (signals n:nucleotide-error (n::check-file-permissions "/etc/passwd" #o022))
  (finishes (n::check-file-permissions "/etc/passwd" #o022 :allow-root t))
  (signals n:nucleotide-error
           (n::check-file-permissions "/etc/passwd" #o044 :allow-root t)))
