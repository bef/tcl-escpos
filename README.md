Tcl-ESCPOS
==========

![alt tag](https://raw.githubusercontent.com/bef/tcl-escpos/logo/logo/tcl-escpos-logo.png)

This Tcl library provides easy access to ESC/POS printer commands.

Author: Ben Fuhrmannek

Installation
------------

First clone the repository:

    git clone https://github.com/bef/tcl-escpos.git

Then either use it in-place `./epprint.tcl ...` or move/copy/link the directory to your Tcl `auto_path`, e.g.

    $ echo 'puts $auto_path' |tclsh
    /usr/share/tcltk/tcl8.6 /usr/share/tcltk /usr/lib /usr/local/lib/tcltk /usr/local/share/tcltk /usr/lib/tcltk/x86_64-linux-gnu /usr/lib/tcltk /usr/lib/tcltk/tcl8.6
    $ mkdir -p /usr/local/lib/tcltk
    $ mv tcl-escpos /usr/local/lib/tcltk/

Templates
---------

All templates are just like regular Tcl-strings with backslash/variable/command substitution, e.g.

    With \$a=[set a "test"], this is a $a.

results in

    With $a=test, this is a test.

See template examples in the `templates` directory for a bunch of examples.

Each ESC/POS command is a proc in `eptmpl.tcl`, annotated with its command name, e.g. `[qrcode "https://example.com"]` (GS ( k), `[pdf417 data]`, `[set_tabs 8 16]` (ESC D) or `[nv_graphics_print A1]` (GS ( L).
PNG images (requires tclgd) can be printed with `[img file.png]` using GS v 0, or with `[img file.png GSL]` using the newer GS ( L graphics commands.

Tests
-----

    tclsh tests/escpos.test

Contributors
============

(in chronological order of contributions)

* Joel: customer display commands
* Sascha Ludwig: templates, fixes, ideas and generally good spirit
