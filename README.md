# Installing the terminal

Make sure when installing helix from the [fork](https://github.com/mattwparas/helix) with steel enabled, that you
run `cargo xtask code-gen` from the root. This will setup all of the necessary steel files in the right spot.

You'll also need to have a matching version of `cargo-steel-lib` or the `steel` executable installed - you can 
then run `cargo-steel-lib` or `steel dylib` from the root of `crates/steel-pty` in order to build the dylib
for the terminal library. 


## Features

* There is a debug window which will capture output from steel `displayln` and render accordingly, just run `:open-debug-window`
* Embedded terminal - run `:open-term` to open it up
* Embedded `xplr` as a file picker. You'll need the `xplr` binary enabled on your path. Just run `xplr` and then navigate to a file.
Hitting enter will open it in the editor.

The terminal windows at the moment do not resize properly when the terminal is resized. However, they can be dragged around the terminal with your mouse.
