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
- libinput settings
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
You must have `sbcl`, `sbcl-slynk` and libxkbcommon installed; nucleotide looks
key names up in libxkbcommon, which it loads as `*libxkbcommon*`. The window
switcher needs `bemenu`. Just run `make` in the root directory, which creates a
`nucleotide` executable.

`make check` runs the tests (needs `sbcl-fiveam`), `make lint` compiles everything
and fails on any warning, `make fmt` formats the code and `make fmt-check` only
checks it. Run `make` and these targets in `guix shell -m manifest.scm`, so that
they find libxkbcommon.

### How to start
```
river -c ./nucleotide
```

### Layouts
`Super+Q` closes the focused window, `Super+Space Q` quits the session.

`Super+Space` followed by `T`, `M` or `S` switches the current workspace to
tiling, monocle or scrolling.

The scrolling layout works like [niri](https://github.com/YaLTeR/niri): every
window opens as a new column right of the focused one, and the view scrolls
just far enough to show the focused column.

| Keys | Action |
|---|---|
| `Super+H` / `Super+L` | Focus the column to the left / right |
| `Super+K` / `Super+J` | Focus the window above / below in the column |
| `Super+Shift+H/L/K/J` | Move the column, or the window within its column |
| `Super+[` / `Super+]` | Move the window into the column to the left / right, or out of its column |
| `Super+R` | Cycle the column width through `*column-widths*` (1/3, 1/2, 2/3) |
| `Super+Ctrl+F` | Toggle full width |
| `Super+C` | Center the focused column |

In tiling and monocle, `H`/`K` and `L`/`J` cycle through the windows.
`(setf *default-layout* 'scrolling)` makes every workspace scroll, and
`(setf *default-layout* '(tiling :ratio 1/2))` gives the master window half
the width. `*gap*` sets the gaps of all layouts.

### Multiple monitors
Every monitor has its own workspaces 1–9. `Super+1…9` switches the workspace of
the focused monitor, `Super+Ctrl+1…9` sends the focused window to one of them.
Both also work with Shift, for keyboards that send the digits shifted.

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

### Window rules, switcher and highlight
```lisp
(rule (:app-id "pavucontrol") :float t)
(rule (:app-id "firefox") :workspace 2)
(rule (:title (contains "Picture-in-Picture")) :float t)
```

Patterns are strings or predicates; actions are `:float`, `:workspace`,
`:output` (a monitor name) and `:fullscreen`. `Super+Tab` lists all windows in
`*menu-command*` (bemenu) and focuses the chosen one.

`Super+Space E` shows the most recently focused Emacs frame alone on the
current monitor without moving it, and hides it again; the command
`(toggle-highlight :app-id "emacs")` takes any patterns a rule takes. Any
other key binding ends the highlight before it runs.

### Input
```lisp
(setf *libinput-settings* '(:tap t :natural-scroll t :accel-profile :flat
                            :disable-while-typing t))
```

`*libinput-settings*` also takes `:left-handed`, `:middle-emulation`,
`:click-method`, `:scroll-method` and `:accel-speed`. The keyboard layout is
river's, which follows `XKB_DEFAULT_LAYOUT` and the other `XKB_DEFAULT_*`
variables.

### Configuration
On start nucleotide loads `$XDG_CONFIG_HOME/nucleotide/init.lisp`
(usually `~/.config/nucleotide/init.lisp`) in the `nucleotide` package, or
binds the default keys of `src/defaults.lisp` if there is none. It must not be
writable by other users, otherwise it is ignored.

```lisp
(use-defaults)

(setf *border-width* 3
      *border-focused* "#00bcff"
      *gap* 4)

(bind "Super+Shift+Return" (spawn "foot"))
(bind "Super+B" (spawn "firefox"))
(bind "Super+Space W" (spawn "firefox --private-window"))
(bind "XF86AudioPlay" (spawn "playerctl" "play-pause"))
(unbind "Super+P")

(define-command focus-firefox (wm)
  (let ((win (find "firefox" (all-windows wm) :key #'window-app-id :test #'equal)))
    (when win
      (focus-window wm win))))

(bind "Super+Space F" focus-firefox)
```

Keys are written like in niri: modifiers (`Shift`, `Ctrl`, `Alt`, `Super` or
`Mod`, `Mod3`, `Mod5`) and a character or xkb key name like `Return`, `Comma`
or `XF86AudioPlay`, joined with `+`. A prefix and a key separated by a space
make a key chord. Bindings run commands: a command name, or a list of one and
its arguments. `(command-names)` lists them; `define-command` adds more. A
command is a function of the window manager and its arguments, so the REPL can
call it too.

`Super+Space C` reloads the init file from scratch: bindings, rules and hooks
it no longer has are gone and settings it no longer sets get their default
again. If it fails to load, everything stays as it was. A few settings, like
`*num-of-workspaces*`, are only read at startup.

Hooks run functions on events, after nucleotide handled them:

```lisp
(on :focus-changed (lambda (win) (format t "~&focus: ~A~%" win)))
```

The events are `:window-opened`, `:window-closed`, `:window-title`,
`:focus-changed` and `:workspace-changed`. For deeper changes, events are
dispatched to the generic function `handle-event`, and redefining a handler
with `define-handler` takes effect immediately.

### REPL
The REPL port is closed by default. `Super+Space R` opens a Slynk listener on
`localhost:4005` that stops listening after the first connection. Press it
again to close the listener early; an established SLY session stays open.

Slynk only accepts clients that know the secret in `~/.sly-secret`. Nucleotide
creates that file on first use (mode 600) and refuses to open the port if it is
readable by others. SLY reads the same file, so `M-x sly-connect` needs no
further setup.

For developing, `sbcl --load dev.lisp` starts nucleotide from the sources with
a listener that stays open. From the REPL, wrap everything that touches the
window manager in `in-wm`, e.g. `(in-wm (reload-config *wm*))`.
