# Nucleotide

A hackable, multi-paradigm Wayland window manager in Common Lisp for the
[river](https://github.com/riverwm/river) compositor.

## Features

- Tiling (master and stack), monocle and niri-like scrolling layouts
- Workspaces per monitor
- Floating dialogs
- Highlight, which teleports a window to the current workspace without actually moving it
- Key bindings and chords
- Autostart
- A mostly niri-compatible IPC
- An init file you can reload at runtime, and a Slynk REPL that opens on demand

## Installation

If you use GNU Guix, install it with `guix package -f guix.scm`.

### Requirements

| Package | Version |
|---------|---------|
| `river` | 0.4.8 |
| `sbcl`  | 2.6.4 |
| `sbcl-slynk` | 1.0.43-9.9c43bf6 |

### Optional requirements

The default settings configure `alacritty`, `bemenu` and `waybar`.

## Keyboard layout

`nucleotide` does not configure the keyboard layout itself.
Instead you can set `XKB_DEFAULT_LAYOUT`, `XKB_DEFAULT_VARIANT`
and `XKB_DEFAULT_OPTIONS` in your environment, as `river` respects these variables.

## Default keybinds

| Keys | Action |
|---|---|
| `Super+Shift+Return` | Start alacritty |
| `Super+P` | Start a program with bemenu |
| `Super+O` | Open or close the overview, shown by an IPC client (see IPC) |
| `Super+Q` | Close the focused window |
| `Super+Shift+F` | Toggle fullscreen |
| `Super+M` | Toggle maximized: cover the monitor except for bars |
| `Super+Shift+Space` | Toggle floating |
| `Super+Space E` | Highlight Emacs |
| `Super+Space C` | Reload the init file |
| `Super+Space R` | Open or close the REPL |
| `Super+Space Q` | Quit the session |

Movement keys are bound as well, see Layouts and Monitors.

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
| `Super+Shift+-` / `Super+Shift+=` | Make the window 10% shorter / taller |
| `Super+Ctrl+R` | Let the window share the column height again |
| `Super+F` | Let the column fill the monitor, or give it the default width |
| `Super+C` | Center the focused column |

In tiling and monocle, `H`/`K` and `L`/`J` step through the windows instead.
Set `*default-layout*` to `'scrolling` or `'(tiling :ratio 1/2)` to change the
layout of new workspaces, and `*gap*` for the gaps of all layouts.

## Monitors
Every monitor has workspaces 1 to 9, or as many as `*num-of-workspaces*` says.
`Super+1` to `Super+9` switch the workspace on the focused monitor, and with
Ctrl they send the focused window there. `Super+,` and `Super+.` focus the
monitor to the left or right. Add Shift to send the window there, or Ctrl to
swap the workspace with the one shown there.

New windows open on the monitor under the pointer, or on the focused one with
`*open-on-pointer-output*` set to `nil`.

## Floating windows
Dialogs and windows with a fixed size float. Drag a window with `Super` and the
left mouse button to move it, or with the right button to resize it.

## IPC
`nucleotide` provides an IPC socket that publishes its state to other programs
and accepts requests to change it.
The most notable features are `which-key` and `overview` support.
It also provides niri-compatible `workspace` and `window` state,
so you can reuse `waybar`'s `niri/workspaces` and `niri/window` modules.

### which-key
The `which-key` feature, known to many Emacs users, publishes the possible next
key presses for the keys pressed so far.

Example: If you press `Super+Space`, which starts a key chord, `which-key` publishes
all keybinds within this chord with their descriptions:

`(bind "Super+Space T" (set-layout tiling) :description "tiling")`

It also works for normal keybinds, not just key chords, but you need to set `*which-key-delay*`.

I provide a `waybar` module for `which-key` in `examples/waybar/which-key`:
```json
"custom/which-key": {
    "exec": "/path/to/which-key",
    "return-type": "json"
}
```

The `waybar` module is easy to use, but fairly limited in formatting possibilities.
Therefore I recommend checking out [nucleotide-tools](https://github.com/fmarl/nucleotide-tools).
It has `nucleotide-which-key`, which shows `which-key` as a panel, and
`nucleotide-overview`, which shows the overview. In the overview, `/` starts a
search through the titles and app ids of all windows.

## Configuration
At startup nucleotide loads `~/.config/nucleotide/init.lisp`. Without it you
get the default keys. The file is not loaded if it or its directory is writable
by group or others.

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

For working on nucleotide itself, see [HACKING.md](HACKING.md).
