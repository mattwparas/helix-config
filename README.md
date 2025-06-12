# How to use

## Helix fork w/ steel

Make sure when installing helix from the [fork](https://github.com/mattwparas/helix) with steel enabled, that you
run `cargo xtask steel` from the root. That will install helix w/ steel and also the `forge` CLI for package management,
the steel language server, and the steel repl.

## Installing dependencies

After cloning this, you can run this to install the package:

```
forge install
```

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


## How to use this as a library

### Splash screen

In your `init.scm`, require the file and decide when to invoke the splash. In this case, I have the splash run when you open with no arguments:

```scheme
(require "mattwparas-helix-config/splash.scm")

(when (equal? (command-line) '("hx"))
  (show-splash))
```

### Recent file picker

```scheme
(require "mattwparas-helix-config/cogs/recentf.scm")

;; Start the snapshotting in the background.
(recentf-snapshot)

;; To open the recent files, you can run
;; :recentf-open-files
```

### Terminal

```scheme
(require "steel-pty/term.scm")
```

This will bring into scope the following functions:

```
(provide open-term
         new-term
         kill-active-terminal
         switch-term
         term-resize
         (contract/out set-default-terminal-cols! (->/c int? void?))
         (contract/out set-default-terminal-rows! (->/c int? void?))
         (contract/out set-default-shell! (->/c string? void?))
         xplr
         open-debug-window
         close-debug-window
         hide-terminal)
```

You may need to configure the default shell, the default is "/bin/zsh":

```scheme 
(set-default-shell! "/bin/zsh")
```

To open the terminal, type `:open-term`.

## File watcher

To use the file watcher, add this to your init.scm:

```scheme
(require "helix-file-watcher/file-watcher.scm")

(spawn-watcher)
```
