;; SPDX-License-Identifier: GPL-3.0-or-later

(in-package #:nucleotide)

(defparameter *focused-border-rgba*
  '(111 140 171 255))

(defparameter *unfocused-border-rgba*
  '(171 171 171 255))

(defparameter *num-of-workspaces*
  9)

(defparameter *default-layout*
  'tiling)

(define-keybinds
    ((:shift +mod-super+) +xk-return+  (spawn "alacritty"))
    (+mod-super+ #\p                   (spawn "bemenu-run"))
  (+mod-super+ #\k                   (cycle-focus wm :prev))
  (+mod-super+ #\l                   (cycle-focus wm :next))
  ((:shift +mod-super+) #\k          (move-window wm :prev))
  ((:shift +mod-super+) #\l          (move-window wm :next))
  ((:shift +mod-super+) #\c          (close-focused wm))
  (+mod-super+ #\f                   (toggle-fullscreen wm))
  (+mod-super+ #\q                   (exit wm)))

(define-key-chords
    (+mod-super+ #\Space
		 (#\t (set-active-layout wm 'tiling))
		 (#\m (set-active-layout wm 'monocle))
		 (#\e (toggle-highlight wm "emacs"))))
