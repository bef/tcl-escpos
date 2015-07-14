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

Contributors
============

(in chronological order of contributions)

* Joel: customer display commands
* Sascha Ludwig: templates, fixes, ideas and generally good spirit
