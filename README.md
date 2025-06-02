# How to use

## Helix fork w/ steel

Make sure when installing helix from the [fork](https://github.com/mattwparas/helix) with steel enabled, that you
run `cargo xtask steel` from the root. That will install helix w/ steel and also the `forge` CLI for package management,
the steel language server, and the steel repl.

##  Installing this as a library

If you'd like to install all the dependencies that I'm using, as well as this directory as a package, which includes:

* File watcher that reloads files on external changes
* Embedded Terminal

Just run:

```
forge pkg install --git https://github.com/mattwparas/helix-config.git
  
```

To install the `forge` CLI if you don't already have it, you can run the following:

```
cargo install --git https://github.com/mattwparas/steel.git forge
```

Other libraries that can be found under the `cogs` directory include:

* Recent file picker - `(cogs/recentf.scm)`
* File tree (which will soon be moved to its own repo) - `(cogs/file-tree.scm)`
* Keybinding niceties - `(cogs/keymaps.scm)`
* Spacemacs theme - `(cogs/themes/spacemacs.scm)`

## Features

### Terminal

* There is a debug window which will capture output from steel `displayln` and render accordingly, just run `:open-debug-window`
* Embedded terminal - run `:open-term` to open it up
* Embedded `xplr` as a file picker. You'll need the `xplr` binary enabled on your path. Just run `xplr` and then navigate to a file.
Hitting enter will open it in the editor.

The terminal windows at the moment do not resize properly when the terminal is resized. However, they can be dragged around the terminal with your mouse.
