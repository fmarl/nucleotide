# Nucleotide: a hackable Wayland window manager

Nucleotide is a hackable Wayland window manager, written in Common Lisp and running
on top of the [river](https://github.com/riverwm/river) compositor.

It is very eary WIP for now and alpha software.

### Features
- Tiling layout with master and stacking area
- Monocle layout
- Keybinds & Keychord support
- Autostart support
- Highlight support: Teleport an Window temporarily to the current workspace without actually moving it
- Fullscreen support
- Live REPL (slynk)

### Currently WIP
- Multi-monitor support
- Libinput support
- Floating dialogs
- Refactoring to better split window management logic and typical configuration
- Window rules
- UI for Keychords: Show available commands when a keychord is pressed with a little overlay in a corner

### How to build
You must have `sbcl` and `sbcl-slynk` installed.
Just run `make` in the root directory, which creates a `nucleotide` executable.

### How to start
```
river -c ./nucleotide
```
