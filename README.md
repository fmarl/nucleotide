# Nucleotide: a hackable Wayland window manager

Nucleotide is a hackable Wayland window manager, written in Common Lisp and running
on top of the [river](https://github.com/riverwm/river) compositor.

It is very early WIP for now and alpha software.

### Features
- Tiling layout with master and stacking area
- Monocle layout
- Scrolling layout like niri: columns on an endless strip that scrolls to the focused one
- Multiple monitors, with workspaces that can be pinned to outputs by name
- Floating dialogs, moved and resized with Super and the mouse
- Window rules
- Keyboard layout and libinput settings
- Window switcher
- Keybinds & Keychord support
- Autostart support
- Highlight support: Teleport a window temporarily to the current workspace without actually moving it
- Fullscreen support
- Live configuration via an init file that can be reloaded at runtime
- Live REPL (slynk), opened on demand and protected by a secret

### Currently WIP
- UI for Keychords: Show available commands when a keychord is pressed with a little overlay in a corner

### How to build
You must have `sbcl` and `sbcl-slynk` installed. Setting the keyboard layout
needs `xkbcli` from libxkbcommon, the window switcher `bemenu`.
Just run `make` in the root directory, which creates a `nucleotide` executable.

`make no-repl` builds an executable without any REPL support.
`make check` runs the tests (needs `sbcl-fiveam`), `make lint` compiles everything
and fails on any warning, `make fmt` formats the code and `make fmt-check` only
checks it.

### How to start
```
river -c ./nucleotide
```

### Layouts
`Super+q` closes the focused window, `Super+Space q` quits the session.

`Super+Space` followed by `t`, `m` or `s` switches the current workspace to
tiling, monocle or scrolling.

The scrolling layout works like [niri](https://github.com/YaLTeR/niri): every
window opens as a new column right of the focused one, and the view scrolls
just far enough to show the focused column.

| Keys | Action |
|---|---|
| `Super+h` / `Super+l` | Focus the column to the left / right |
| `Super+k` / `Super+j` | Focus the window above / below in the column |
| `Super+Shift+h/l/k/j` | Move the column, or the window within its column |
| `Super+[` / `Super+]` | Move the window into the column to the left / right, or out of its column |
| `Super+r` | Cycle the column width through `*column-widths*` (1/3, 1/2, 2/3) |
| `Super+Ctrl+f` | Toggle full width |
| `Super+c` | Center the focused column |

In tiling and monocle, `h`/`k` and `l`/`j` cycle through the windows.
`(defparameter *default-layout* 'scrolling)` in the init file makes every
workspace scroll.

### Multiple monitors
Every monitor has its own workspaces 1–9. `Super+1…9` switches the workspace of
the focused monitor, `Super+Shift+1…9` sends the focused window to one of them.

| Keys | Action |
|---|---|
| `Super+,` / `Super+.` | Focus the monitor to the left / right |
| `Super+Shift+,` / `Super+Shift+.` | Send the focused window there |
| `Super+Ctrl+,` / `Super+Ctrl+.` | Exchange the workspace with the one shown there |

When a monitor is unplugged, its windows move to the same workspaces of
another monitor. Plugging it in again brings them back, unless they were moved
in the meantime.

### Floating windows
Dialogs (windows with a parent) and windows with a fixed size float above the
layout, centered on their parent or monitor. `Super+Shift+Space` toggles
floating. `Super` with the left mouse button moves a window, also onto another
monitor; with the right one it resizes the window. Both make a tiled window
float.

### Window rules and switcher
```lisp
(define-window-rule (:app-id "pavucontrol") :float t)
(define-window-rule (:app-id "firefox") :workspace 2)
(define-window-rule (:title (contains "Picture-in-Picture")) :float t)
```

Patterns are strings or predicates; actions are `:float`, `:workspace`,
`:output` (a monitor name) and `:fullscreen`. `Super+Tab` lists all windows in
`*menu-command*` (bemenu) and focuses the chosen one.

### Input
```lisp
(defparameter *keyboard-layout* "de")
(defparameter *keyboard-options* "caps:escape")
(defparameter *libinput-settings*
  '(:tap t :natural-scroll t :accel-profile :flat :disable-while-typing t))
```

The keymap is compiled with `xkbcli compile-keymap` and applied to every
keyboard. `*libinput-settings*` also takes `:left-handed`, `:middle-emulation`,
`:click-method`, `:scroll-method` and `:accel-speed`.

### Configuration
On start nucleotide loads `$XDG_CONFIG_HOME/nucleotide/init.lisp`
(usually `~/.config/nucleotide/init.lisp`) in the `nucleotide` package.
The default keybindings live in `src/defaults.lisp`, each setting next to the code
that uses it; the init file adds to or overrides both.
It must not be writable by other users, otherwise it is ignored.

```lisp
(defparameter *border-width* 3)

;; Adds keybinds, or replaces those on the same keys.
(define-keybinds (wm)
  ((:super :shift) :return (spawn "foot"))
  (:super #\b              (spawn "firefox")))

;; Super+Space, then a key.
(define-key-chords (wm)
  ((:super :space)
   (#\w (spawn "firefox"))
   ((:shift #\w) (spawn "firefox --private-window"))))

(remove-keybind :super #\p)
```

Modifiers are `:shift`, `:ctrl`, `:alt`, `:super`, `:mod3` and `:mod5`, alone or
as a list. Keys are characters or keywords like `:return`, `:space`, `:escape`,
`:left` or `:f1`. `(clear-keybinds)` drops all defaults if you prefer to start
from scratch.

`Super+Space c` reloads the init file and reinstalls all keybindings.
Settings that only take effect at startup (like `*num-of-workspaces*`) need a restart.

Window manager behaviour can be changed live too: events are dispatched to the
generic function `handle-event`, so redefining a handler takes effect
immediately, e.g.

```lisp
(define-handler (wm (win window) :title) (title)
  (setf (window-title win) title)
  (format t "~&title: ~A~%" title))
```

### REPL
The REPL port is closed by default. `Super+Space r` opens a Slynk listener on
`localhost:4005` that accepts a single connection and closes itself again if
nobody connects within 5 minutes (`*repl-idle-timeout*`). Press it again to
close the listener early; an established SLY session stays open.

Slynk only accepts clients that know the secret in `~/.sly-secret`. Nucleotide
creates that file on first use (mode 600) and refuses to open the port if it is
readable by others. SLY reads the same file, so `M-x sly-connect` needs no
further setup.

Start nucleotide with `NUCLEOTIDE_REPL=1` to keep a listener open the whole
time, e.g. while developing. From the REPL, wrap everything that touches the
window manager in `in-wm`, e.g. `(in-wm (reload-config *wm*))`.
