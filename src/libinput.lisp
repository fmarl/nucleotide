;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(defparameter *libinput-settings*
  '(:tap t)
  "Plist of settings applied to every device that supports them: :TAP,
:NATURAL-SCROLL, :LEFT-HANDED, :MIDDLE-EMULATION and :DISABLE-WHILE-TYPING take
T or NIL; :CLICK-METHOD takes :BUTTON-AREAS or :CLICKFINGER; :SCROLL-METHOD
:NO-SCROLL, :TWO-FINGER, :EDGE or :ON-BUTTON-DOWN; :ACCEL-PROFILE :FLAT or
:ADAPTIVE; :ACCEL-SPEED a number from -1 to 1.")

(defparameter *libinput-requests*
  '((:tap :tap-support river-libinput-device-v1.set-tap "tap_state")
    (:natural-scroll :natural-scroll-support
     river-libinput-device-v1.set-natural-scroll "natural_scroll_state")
    (:left-handed :left-handed-support
     river-libinput-device-v1.set-left-handed "left_handed_state")
    (:middle-emulation :middle-emulation-support
     river-libinput-device-v1.set-middle-emulation "middle_emulation_state")
    (:disable-while-typing :dwt-support river-libinput-device-v1.set-dwt "dwt_state")
    (:click-method :click-method-support
     river-libinput-device-v1.set-click-method "click_method")
    (:scroll-method :scroll-method-support
     river-libinput-device-v1.set-scroll-method "scroll_method")
    (:accel-profile :accel-profiles-support
     river-libinput-device-v1.set-accel-profile "accel_profile")
    (:accel-speed :accel-profiles-support
     river-libinput-device-v1.set-accel-speed :double))
  "Setting, the event announcing support for it, the request setting it, and
how to encode its value: the name of an enum, or :DOUBLE.")

(defun enum-value (class enum entry)
  (symbol-value (enum-constant-name (interface-wire-name class) enum entry)))

(defun double-octets (number)
  (let ((double (coerce number 'double-float))
        (octets (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (u32ref octets 0) (sb-kernel:double-float-low-bits double)
          (u32ref octets 4) (ldb (byte 32 0) (sb-kernel:double-float-high-bits double)))
    octets))

(defun libinput-value (encoding value)
  (cond ((eq encoding :double) (double-octets value))
        ((keywordp value)
         (enum-value 'river-libinput-device-v1 encoding
                     (substitute #\_ #\- (string-downcase value))))
        (t
         (enum-value 'river-libinput-device-v1 encoding
                     (if value "enabled" "disabled")))))

(defun watch-libinput-result (setting result)
  (push (lambda (event &rest args)
          (declare (ignore args))
          (unless (eq event :success)
            (warn "libinput setting ~S rejected: ~A" setting event)))
        (proxy-hooks result)))

(defun apply-libinput-settings (device event support)
  (when (plusp support)
    (loop for (setting announcement request encoding) in *libinput-requests*
          when (eq event announcement)
          do (multiple-value-bind (present value)
                 (get-properties *libinput-settings* (list setting))
               (when present
                 (watch-libinput-result
                  setting
                  (funcall request device (libinput-value encoding value))))))))

(defmethod handle-event (wm (device river-libinput-device-v1) event &rest args)
  (declare (ignore wm))
  (when (and args (integerp (first args)))
    (apply-libinput-settings device event (first args))))

(define-handler (wm (config river-libinput-config-v1) :libinput-device) (device)
  (forward-events wm device))

(define-handler (wm (device river-libinput-device-v1) :removed) ()
  (river-libinput-device-v1.destroy device))

(define-handler (wm (manager river-input-manager-v1) :input-device) (device)
  (forward-events wm device))

(define-handler (wm (device river-input-device-v1) :removed) ()
  (river-input-device-v1.destroy device))
