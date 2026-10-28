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
  (is (= 1 (n::libinput-value "accel_profile" :flat)))
  (signals n::nucleotide-error (n::libinput-value "accel_profile" :steep))
  (is (null (find-symbol "+RIVER-LIBINPUT-DEVICE-V1-ACCEL-PROFILE-STEEP+" '#:nucleotide))))

(test accel-speed-is-a-little-endian-double
  (is (equalp (octets 0 0 0 0 0 0 #xe0 #x3f) (n::double-octets 0.5)))
  (is (equalp (octets 0 0 0 0 0 0 #xf0 #xbf) (n::double-octets -1))))

(test repeat-and-scroll-settings-fit-the-wire
  (is (null (n::check-key-repeat nil)))
  (is (null (n::check-key-repeat '(25 600))))
  (signals n::nucleotide-error (n::check-key-repeat '(25 3000000000)))
  (signals n::nucleotide-error (n::check-key-repeat '(-1 600)))
  (signals n::nucleotide-error (n::check-key-repeat '(25)))
  (is (null (n::check-scroll-factor 0.5)))
  (signals n::nucleotide-error (n::check-scroll-factor 1e9))
  (signals n::nucleotide-error (n::check-scroll-factor -1)))
