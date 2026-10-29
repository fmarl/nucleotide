# Nucleotide

A hackable, multi-paradigm Wayland window manager in Common Lisp for the
[river](https://github.com/riverwm/river) compositor.

## Features
- Tiling (master and stack), monocle and niri-like scrolling layouts
- Workspaces per monitor that come back when you plug the monitor in again
- Floating dialogs, moved and resized with Super and the mouse
- Window rules, a window switcher, and a highlight that shows a window on the
  current workspace without moving it
- Key bindings and chords, autostart, fullscreen, libinput settings
- Workspaces and window title in waybar
- An init file you can reload at runtime, and a Slynk REPL that opens on demand

## Installation
nucleotide needs river 0.4.7 or newer. Install it with
`guix package -f guix.scm` and start it with `river -c nucleotide`. The default
keys start alacritty and bemenu, and waybar starts with the session.

river takes the keyboard layout from `XKB_DEFAULT_LAYOUT`,
`XKB_DEFAULT_VARIANT` and `XKB_DEFAULT_OPTIONS`, e.g.
`XKB_DEFAULT_LAYOUT=de river -c nucleotide`. With several layouts, set
`*bindings-layout*` to `0` to keep the bindings on the first one.

## Keys
| Keys | Action |
|---|---|
| `Super+Shift+Return` | Start alacritty |
| `Super+P` | Start a program with bemenu |
| `Super+Tab` | Pick a window with bemenu and focus it |
| `Super+Q` | Close the focused window |
| `Super+Shift+F` | Toggle fullscreen |
| `Super+M` | Toggle maximized: cover the monitor except for bars |
| `Super+Shift+Space` | Toggle floating |
| `Super+Space E` | Show Emacs alone on the focused monitor, or stop |
| `Super+Space C` | Reload the init file |
| `Super+Space R` | Open or close the REPL |
| `Super+Space Q` | Quit the session |

## Layouts
`Super+Space` followed by `T`, `M` or `S` switches the workspace to tiling,
monocle or scrolling. Scrolling works like
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
| `Super+F` | Let the column fill the monitor, or give it the default width |
| `Super+C` | Center the focused column |

In tiling and monocle, `H`/`K` and `L`/`J` step through the windows instead.
Set `*default-layout*` to `'scrolling` or `'(tiling :ratio 1/2)` to change the
layout of new workspaces, and `*gap*` for the gaps of all layouts.

Your own layouts subclass `layout` and define `layout-arrange`. Their initargs
are the options of `set-layout`.

## Monitors
Every monitor has workspaces 1 to 9. `Super+1` to `Super+9` switch the
workspace on the focused monitor, and with Ctrl they send the focused window
there. `Super+,` and `Super+.` focus the monitor to the left or right. Add
Shift to send the window there, or Ctrl to swap the workspace with the one
shown there.

New windows open on the monitor under the pointer, or on the focused one with
`*open-on-pointer-output*` set to `nil`.

## Floating windows
Dialogs and windows with a fixed size float. Drag a window with `Super` and the
left mouse button to move it, or with the right button to resize it. Windows
get a border instead of their own title bar.

## Waybar
nucleotide speaks the IPC of niri and sets `NIRI_SOCKET` for the programs it
starts. waybar's `niri/workspaces` and `niri/window` modules then show the
workspaces and the title of the focused window.

## Configuration
At startup nucleotide loads `$XDG_CONFIG_HOME/nucleotide/init.lisp` into the
`nucleotide.user` package, which sees the symbols that `nucleotide` exports.
Without it you get the default keys from `src/defaults.lisp`. The file is
ignored if it or its directory is writable by others.

```lisp
(use-defaults)

(setf *border-focused* "#00bcff"
      *cursor-theme* '("Adwaita" 32)
      *gap* 4
      *libinput-settings* '(:tap t :natural-scroll t)
      *key-repeat* '(40 300)   ; repeats per second, delay in ms
      *scroll-factor* 0.5)

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

Reloading starts from scratch: whatever the file no longer defines disappears,
and settings it no longer sets return to their defaults. If loading fails, a
notification says why and the old configuration stays. A few settings, like
`*num-of-workspaces*`, only take effect after a restart.

## REPL
`Super+Space R` opens a Slynk listener on `localhost:4005` for one connection.
Only clients that know the secret in `~/.sly-secret` get in, so
`M-x sly-connect` works without further setup.

## Development
Inside `guix shell -m manifest.scm`, `make` builds `./nucleotide`, which you
start with `river -c ./nucleotide`. `make check` runs the tests and `make lint`
the linter. `make integration` runs nucleotide in a headless river and needs
river, weston and wlr-randr.

`sbcl --load dev.lisp` runs nucleotide from source with a listener that stays
open. REPL code that touches the window manager has to run in its thread, so
wrap it in `in-wm`, e.g. `(in-wm (reload-config *wm*))`.
