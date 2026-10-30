# Hacking on nucleotide

## Getting started
```sh
guix shell -m manifest.scm
make              # builds ./nucleotide, start it with river -c ./nucleotide
make check        # unit tests
make lint         # recompiles everything, fails on any warning
make fmt          # indents the Lisp files like SLY does
```

`make integration` runs nucleotide in a headless river. It needs river, weston
and wlr-randr, which aren't in the manifest. Without them the tests are
skipped.

`sbcl --load dev.lisp` runs nucleotide from source with a Slynk listener on
port 4005. Connect with `M-x sly-connect`, recompile a function and the
running WM uses it.

Only the WM thread may talk to river, so wrap REPL code that touches windows,
outputs or proxies in `in-wm`:

```lisp
(in-wm (reload-config *wm*))
```

## Layout of the code
`src/core/` is a Wayland client written from scratch, no libwayland, no FFI.
`scanner.lisp` turns the XML files in `protocol/` into Lisp at compile time:

- a function per request, like `river-window-v1.close`
- events as keywords, like `:manage-start`
- enum constants, like `+river-seat-v1-modifiers-mod4+`

The XML files are also the documentation for what each request does.
`eventloop.lisp` runs the WM thread: Wayland messages, thunks from `in-wm`
and timers.

The rest of `src/` is the window manager, loaded in the order of
`nucleotide.asd`:

| Files | What's in them |
|---|---|
| `model.lisp` | The `wm` class and the window and output state |
| `events.lisp`, `wm.lisp` | `define-handler` and the handlers for river's events |
| `manage.lisp`, `render.lisp` | What we send river in each manage and render sequence |
| `workspaces.lisp`, `outputs.lisp` | Workspaces, and monitors coming and going |
| `layouts.lisp`, `scrolling*.lisp` | Tiling, monocle and scrolling |
| `floating.lisp`, `highlight.lisp`, `windows.lisp` | Floating, the highlight, window commands |
| `settings.lisp`, `commands.lisp`, `keys.lisp`, `bindings.lisp` | What users configure |
| `hooks.lisp`, `rules.lisp`, `init-file.lisp`, `defaults.lisp` | Hooks, rules, the init file, the defaults |
| `ipc.lisp`, `which-key.lisp` | The niri IPC socket and which-key |
| `repl.lisp`, `main.lisp` | The Slynk server and the entry point |

## How a change reaches the screen
river takes changes from the window manager only during a manage sequence.
Anything else is a protocol error and river drops the connection. So it goes
like this:

1. river sends events. Handlers (`define-handler`) update our state and queue
   what river should do with `(request wm :kind args...)`.
2. river sends `:manage-start`. `manage` in `manage.lisp` takes the queued
   requests and sends them, in a fixed order. Its docstring says why the order
   matters.
3. A render sequence follows, where `render.lisp` sets borders and stacking.

Most river events are followed by a manage sequence on their own. For changes
river doesn't know about, like a timer, the REPL or IPC, call
`(mark-dirty wm)`.

A new feature is usually a handler or command that sets state and queues a
request, plus a `manage-...` step that acts on it.

## Configuration
`define-setting` declares a setting with a `:check` function, which runs after
the init file loads. `define-command` defines something you can bind, and its
`type` declarations are checked when the key is bound. `bind` adds `keybind`
structs to `*bindings*`, and `install-all-keybinds` turns them into river
bindings.

The init file loads with all config variables freshly bound and is installed
only if it loaded without errors. That's why reloading starts from scratch and
a broken file leaves the old config alone. A new config variable that isn't a
setting goes into `config-variables` in `init-file.lisp`.

When user code fails, `call-reporting-failure` shows a notification the first
time and logs after that. A failing event handler skips the event. Set
`*debug-on-error*` to get the debugger instead.

## Tests
FiveAM, one suite per file. Most tests run against the fake river in
`tests/server.lisp`, which speaks the real wire protocol:

- `with-fake-server` and `connect-wm` set up a WM
- `deliver` sends it an event
- `run-sequence` runs a manage or render sequence
- `received` returns what the WM sent back

Use the `fresh-config` fixture when a test touches the config. Name tests as
sentences, like `releasing-the-leader-cancels-the-delay`.

The fake river only knows what we taught it. When timing against the real
river matters, add a test to `tests/integration.lisp` too.

## Style
- SPDX and copyright header in every file
- Lines under 100 columns
- `make lint` reports 0 warnings
- Docstrings only when they say more than the name
- Structs instead of lists taken apart by position
