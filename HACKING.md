# Hacking on nucleotide

## Getting started
The build and test tools are in `manifest.scm`:

```sh
guix shell -m manifest.scm
make              # builds ./nucleotide, start it with river -c ./nucleotide
make check        # unit tests
make lint         # recompiles everything, fails on any compiler warning
make fmt          # indents all Lisp files the way SLY does
```

`make integration` runs nucleotide in a headless river. It needs river, weston
(for `weston-simple-shm`) and wlr-randr on your `PATH`. These are not in the
manifest, and without them the tests are skipped, not failed.

For live hacking, `sbcl --load dev.lisp` runs nucleotide from source and keeps
a Slynk listener open on port 4005. Connect with `M-x sly-connect`, change a
function, recompile it, and the running window manager picks it up.

Only the WM thread may talk to river. From the REPL, wrap anything that
touches windows, outputs or proxies in `in-wm`:

```lisp
(in-wm (reload-config *wm*))
```

Requests from any other thread raise an error instead of corrupting the
connection.

## How the code is laid out
The files load in the order listed in `nucleotide.asd`.

`src/core/` is a small Wayland client written from scratch. There is no
libwayland and no FFI. `wire.lisp` and `client.lisp` handle the socket and
message encoding, and `scanner.lisp` reads the protocol XML files in
`protocol/` at compile time. `river.lisp` is just five `define-protocol` lines.
From those you get:

- a function per request, named `interface.request`, like
  `river-window-v1.close` or `river-window-manager-v1.manage-dirty`
- events as keywords, like `:manage-start` or `:modifiers-update`
- enum constants like `+river-seat-v1-modifiers-mod4+`

What a request or event does is documented in the XML files in `protocol/`.

`eventloop.lisp` runs the loop on the WM thread: incoming Wayland messages,
thunks posted from other threads (that is what `in-wm` uses) and timers.

Everything else in `src/` is the window manager:

| Files | What's in them |
|---|---|
| `model.lisp` | The `wm` class and the window and output state |
| `events.lisp`, `wm.lisp` | `define-handler`, and the handlers for river's events |
| `manage.lisp`, `render.lisp` | What we send river in each manage and render sequence |
| `workspaces.lisp`, `outputs.lisp` | Workspaces per monitor, and what happens when monitors come and go |
| `layouts.lisp`, `scrolling*.lisp` | Tiling and monocle, and the niri-like scrolling layout |
| `floating.lisp`, `highlight.lisp`, `windows.lisp` | Floating windows, the highlight, window commands |
| `settings.lisp`, `commands.lisp`, `keys.lisp`, `bindings.lisp` | The pieces users configure |
| `hooks.lisp`, `rules.lisp`, `init-file.lisp`, `defaults.lisp` | Hooks, window rules, loading the init file, the default config |
| `ipc.lisp`, `which-key.lisp` | The niri-compatible IPC socket and which-key |
| `repl.lisp`, `main.lisp` | The on-demand Slynk server and the executable's entry point |

## How a change reaches the screen
river accepts changes from the window manager only in sequences:

1. river sends events, like a new window, a key press or a changed modifier.
   Each one lands in a handler defined with `define-handler`.
2. Handlers only update our own state. If something has to be sent to river,
   they queue it with `(request wm :kind args...)`.
3. When river is done sending, it starts a manage sequence with
   `:manage-start`. That runs `manage` in `manage.lisp`, which goes through
   fixed steps that take the queued requests (`take-requests`,
   `drain-requests`) and send the actual river requests. The order of the
   steps matters, and the docstring of `manage` says why.
4. A render sequence follows, where `render.lisp` sets borders and the
   stacking order.

Many river events are followed by a manage sequence anyway. When a change comes
from somewhere river doesn't know about, like a timer, the REPL or an IPC
client, call `(mark-dirty wm)` so river starts one.

If you call a window management request outside of a manage sequence, river
kills the connection. So when you add a feature, the usual shape is a handler
or command that sets some state and queues a request, plus a `manage-...` step
that acts on it.

## The user-facing pieces
These are the macros users see in their init file:

- `define-setting` declares a setting with a default and a `:check` function.
  Every setting is checked after the init file loads, so bad values fail early
  with a readable message.
- `define-command` defines something bindings can run. The `type` declarations
  in its body are checked when a key is bound, not when it is pressed.
- `bind` and `set-binding` add `keybind` structs to `*bindings*`.
  `install-all-keybinds` turns them into river bindings, and chords become
  submaps.
- `on` adds a hook, `rule` adds a window rule.

The init file is loaded with all config variables freshly bound (see
`call-with-default-config`), and the result is installed only if loading
succeeded. That's why reloading starts from scratch and a broken file never
leaves you with half a config. When you add a new config variable that isn't a
setting, add it to `config-variables` in `init-file.lisp`.

Errors in user code (commands, hooks, rules) go through
`call-reporting-failure`: the first failure shows a notification, later ones
only go to the log. Errors in our own event handling skip the event and keep
the loop running. Set `*debug-on-error*` to `t` if you want the debugger
instead.

## Tests
The tests use FiveAM, with one suite per file in `tests/`. Most of them run
against a fake river in `tests/server.lisp`, which speaks the real wire
protocol over a socket pair. The helpers you'll use most:

- `with-fake-server` sets up the socket pair
- `connect-wm` connects a WM and gets it through the registry
- `deliver` sends an event to the WM, like
  `(deliver display socket id "river_window_v1" "closed")`
- `run-sequence` runs a manage or render sequence
- `received` returns what the WM sent back

Tests that touch the config use the `fresh-config` fixture so they don't leak
settings or bindings into each other. Test names are sentences that describe
the behaviour, like `releasing-the-leader-cancels-the-delay`, so the test list
reads as a spec.

The fake river only knows what we taught it, so for anything where timing
between river and nucleotide matters, add an integration test in
`tests/integration.lisp` as well.

## Style
- Every file starts with the SPDX and copyright header.
- Lines stay under 100 columns. `make fmt` handles indentation, not line
  length.
- `make lint` has to report 0 warnings.
- Docstrings say what a function returns or why it does something
  surprising. Skip the ones that only repeat the function name.
- Prefer small functions and plain data. When a list starts getting taken
  apart by position in several places, it probably wants to be a struct.
