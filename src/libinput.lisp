;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

(define-setting *libinput-settings*
    '(:tap t)
  :documentation "Plist of settings applied to every device that supports them: :TAP,
:NATURAL-SCROLL, :LEFT-HANDED, :MIDDLE-EMULATION and :DISABLE-WHILE-TYPING take
T or NIL. :CLICK-METHOD takes :BUTTON-AREAS or :CLICKFINGER, :SCROLL-METHOD
:NO-SCROLL, :TWO-FINGER, :EDGE or :ON-BUTTON-DOWN, :ACCEL-PROFILE :FLAT or
:ADAPTIVE, and :ACCEL-SPEED a number from -1 to 1. Devices get them when they appear."
  :check 'check-libinput-settings)

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
  (let ((name (find-symbol (enum-constant-string (interface-wire-name class) enum entry)
                           '#:nucleotide)))
    (unless (and name (boundp name))
      (nucleotide-error "~A is not one of the values of ~A" entry enum))
    (symbol-value name)))

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

(defun check-libinput-settings (plist)
  (unless (and (listp plist) (evenp (length plist)))
    (nucleotide-error "~S is not a plist like (:tap t)" plist))
  (loop for (setting value) on plist by #'cddr
        for encoding = (fourth (or (find setting *libinput-requests* :key #'first)
                                   (nucleotide-error "unknown libinput setting ~S" setting)))
        do (if (eq encoding :double)
               (unless (typep value '(real -1 1))
                 (nucleotide-error "~S is not a number from -1 to 1" value))
               (libinput-value encoding value))))

(defun watch-libinput-result (setting result)
  (push (lambda (event &rest args)
          (declare (ignore args))
          (unless (eq event :success)
            (log-message :warn "libinput setting ~S rejected: ~A" setting event)))
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

(defun check-key-repeat (value)
  (unless (typep value '(or null (cons (and wire-int unsigned-byte)
                                  (cons (and wire-int unsigned-byte) null))))
    (nucleotide-error "~S is not NIL or a rate and a delay like (25 600)" value)))

(define-setting *key-repeat* nil
  :documentation "NIL to keep the default of river, or (RATE DELAY): key repeats per
second, 0 for none, and the milliseconds until they start. Keyboards get it when
they appear."
  :check #'check-key-repeat)

(defun check-scroll-factor (value)
  (unless (typep value '(or null (and wire-fixed (real 0))))
    (nucleotide-error "~S is not NIL or a factor like 0.5" value)))

(define-setting *scroll-factor* nil
  :documentation "NIL to keep the default of river, or how much faster pointers scroll,
like 0.5 or 2. Pointers get it when they appear."
  :check #'check-scroll-factor)

(define-handler (wm (manager river-input-manager-v1) :input-device) (device)
  (forward-events wm device))

(define-handler (wm (device river-input-device-v1) :type) (type)
  (cond ((and *key-repeat* (= type +river-input-device-v1-type-keyboard+))
         (apply #'river-input-device-v1.set-repeat-info device *key-repeat*))
        ((and *scroll-factor* (= type +river-input-device-v1-type-pointer+))
         (river-input-device-v1.set-scroll-factor device *scroll-factor*))))

(define-handler (wm (device river-input-device-v1) :removed) ()
  (river-input-device-v1.destroy device))
