;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide/tests)

(def-suite libinput :in :nucleotide)
(in-suite libinput)

(test libinput-values
  (is (= 1 (n::libinput-value "tap_state" t)))
  (is (= 0 (n::libinput-value "natural_scroll_state" nil)))
  (is (= 2 (n::libinput-value "click_method" :clickfinger)))
  (is (= 1 (n::libinput-value "scroll_method" :two-finger)))
  (is (= 1 (n::libinput-value "accel_profile" :flat))))

(test accel-speed-is-a-little-endian-double
  (is (equalp (octets 0 0 0 0 0 0 #xe0 #x3f) (n::double-octets 0.5)))
  (is (equalp (octets 0 0 0 0 0 0 #xf0 #xbf) (n::double-octets -1))))
