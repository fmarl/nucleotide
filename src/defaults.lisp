;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>

(in-package #:nucleotide)

;;; The bindings nucleotide starts with. The init file can add to them,
;;; replace single ones, or drop them all with CLEAR-KEYBINDS.

(define-workspace-keybinds :super '(:super :shift))

(define-keybinds (wm)
  ((:super :shift) :return  (spawn "alacritty"))
  (:super #\p               (spawn "bemenu-run"))
  (:super #\h               (cycle-focus wm :prev))
  (:super #\l               (cycle-focus wm :next))
  (:super #\k               (cycle-focus wm :up))
  (:super #\j               (cycle-focus wm :down))
  ((:super :shift) #\h      (move-window wm :prev))
  ((:super :shift) #\l      (move-window wm :next))
  ((:super :shift) #\k      (move-window wm :up))
  ((:super :shift) #\j      (move-window wm :down))
  (:super #\[               (consume-or-expel-window wm :prev))
  (:super #\]               (consume-or-expel-window wm :next))
  (:super #\r               (cycle-column-width wm))
  ((:super :ctrl) #\f       (toggle-full-width wm))
  (:super #\c               (center-column wm))
  ((:super :shift) :space   (toggle-floating wm))
  (:super :tab              (window-switcher wm))
  (:super #\,               (focus-output wm :prev))
  (:super #\.               (focus-output wm :next))
  ((:super :shift) #\,      (send-to-output wm :prev))
  ((:super :shift) #\.      (send-to-output wm :next))
  ((:super :ctrl) #\,       (move-workspace-to-output wm :prev))
  ((:super :ctrl) #\.       (move-workspace-to-output wm :next))
  (:super #\f               (toggle-fullscreen wm))
  (:super #\q               (close-focused wm)))

(define-key-chords (wm)
  ((:super :space)
   (#\t (set-active-layout wm 'tiling))
   (#\m (set-active-layout wm 'monocle))
   (#\s (set-active-layout wm 'scrolling))
   (#\e (toggle-highlight wm "emacs"))
   (#\r (toggle-repl-server))
   (#\c (reload-config wm))
   (#\q (exit-session wm))))
