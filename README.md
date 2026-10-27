# Nucleotide

A hackable Wayland window manager in Common Lisp for the
[river](https://github.com/riverwm/river) compositor.

## Features
- Tiling (master and stack), monocle and niri-like scrolling layouts
- Workspaces per monitor that come back when you plug the monitor in again
- Floating dialogs, moved and resized with Super and the mouse
- Window rules, a window switcher, and a highlight that shows a window on the
  current workspace without moving it
- Key bindings and chords, autostart, fullscreen, libinput settings
- Workspaces and window title in waybar through its niri modules
- An init file you can reload at runtime, and a Slynk REPL that opens on demand
  and is protected by a secret

## Build and start
Inside `guix shell -m manifest.scm`, `make` builds `./nucleotide`, which you start with `river -c ./nucleotide`.

## Layouts
`Super+Space` followed by `T`, `M` or `S` switches the workspace to tiling,
monocle or scrolling. Scrolling works similar to
[niri](https://github.com/YaLTeR/niri): every window opens as a new column to
the right of the focused one, and the view scrolls just far enough to show the
focused column.

| Keys | Action |
|---|---|
| `Super+H` / `Super+L` | Focus the column to the left / right |
| `Super+K` / `Super+J` | Focus the window above / below in the column |
| `Super+Shift+H/L/K/J` | Move the column, or the window within its column |
| `Super+[` / `Super+]` | Move the window into the column to the left / right, or out of its column |
| `Super+R` | Cycle the column width through `*column-widths*` (1/3, 1/2, 2/3) |
| `Super+-` / `Super+=` | Make the column 10% narrower / wider |
| `Super+Shift+R` | Cycle the window height through `*window-heights*` (1/3, 1/2, 2/3) |
| `Super+Shift+-` / `Super+Shift+=` | Make the window 10% lower / higher |
| `Super+Ctrl+R` | Let the window share the column height again |
| `Super+F` | Toggle full width |
| `Super+Shift+F` | Toggle fullscreen |
| `Super+C` | Center the focused column |
| `Super+Q` | Close the focused window |
| `Super+Space Q` | Quit the session |

In tiling and monocle, `H`/`K` and `L`/`J` step through the windows instead.
Set `*default-layout*` to `'scrolling` or `'(tiling :ratio 1/2)` to change the
layout of new workspaces, and `*gap*` for the gaps of all layouts.

## Monitors
Every monitor has workspaces 1–9. `Super+1…9` switches the workspace on the
focused monitor and `Super+Ctrl+1…9` sends the focused window there. Both also
work with Shift held, for keyboards that send the digits shifted. `Super+,`
and `Super+.` focus the monitor to the left or right; add Shift to send the
window there, or Ctrl to swap the workspace with the one shown there.

## Floating windows
Dialogs and windows with a fixed size float, centered on their parent or on
the monitor. `Super+Shift+Space` toggles floating. Drag a window with `Super`
and the left mouse button, also onto another monitor, or resize it with the
right one.

## Configuration
At startup nucleotide loads `$XDG_CONFIG_HOME/nucleotide/init.lisp` into the
`nucleotide` package. Without it you get the default keys from
`src/defaults.lisp`. The file is ignored if it or its directory is writable by
others.

```lisp
(use-defaults)

(setf *border-focused* "#00bcff"
      *gap* 4
      *libinput-settings* '(:tap t :natural-scroll t))

(bind "Super+Shift+Return" (spawn "foot"))
(bind "Super+Space W" (spawn "firefox --private-window"))
(bind "XF86AudioPlay" (spawn "playerctl" "play-pause"))
(unbind "Super+P")

(rule (:app-id "pavucontrol") :float t)
(rule (:title (contains "Picture-in-Picture")) :float t :workspace 2)

(define-command focus-firefox (wm)
  (let ((win (find "firefox" (all-windows wm) :key #'window-app-id :test #'equal)))
    (when win
      (focus-window wm win))))

(bind "Super+Space F" focus-firefox)

(on :focus-changed (lambda (win) (format t "~&focus: ~A~%" win)))
```

`Super+Space C` reloads the init file from scratch: whatever it no longer
defines disappears, and settings it no longer sets return to their defaults.
If loading fails, the old configuration stays, except for functions and
handlers the file redefined before the error. A few settings, like
`*num-of-workspaces*`, only take effect after a restart.

## REPL
`Super+Space R` opens a Slynk listener on `localhost:4005`. It accepts one
connection and then stops listening; pressing the key again closes it early
without ending an open SLY session. Only clients that know the secret in
`~/.sly-secret` get in. nucleotide creates that file with mode 600 and won't
open the port if others can read it. SLY reads the same file, so
`M-x sly-connect` works without further setup.

For development, `sbcl --load dev.lisp` runs nucleotide from source with a
listener that stays open. REPL code that touches the window manager has to run
in its thread, so wrap it in `in-wm`, e.g. `(in-wm (reload-config *wm*))`.
