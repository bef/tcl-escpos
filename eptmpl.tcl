## Author: Ben Fuhrmannek <bef@eventphone.de>
## Date: 2012-04-21
##
## Copyright (c) 2012 Ben Fuhrmannek
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer.
##     * Redistributions in binary form must reproduce the above copyright
##       notice, this list of conditions and the following disclaimer in the
##       documentation and/or other materials provided with the distribution.
##     * Neither the name of the author nor the
##       names of its contributors may be used to endorse or promote products
##       derived from this software without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
## DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


package provide escpos 0.1

package require Tcl 8.5
catch {package require tclgd}

namespace eval ::escpos {
	namespace export *

	proc date {{format {%A %d. %b %Y %T}}} { return [clock format [clock seconds] -format $format] }
	proc _nLnH {n} { return [list [expr {$n % 256}] [expr {$n / 256}]] }

	proc ff {} { ## FF: Print and recover to page mode
		return "\x0c"
	}
	proc cr {} { ## CR: Print and carriage return
		return "\x0d"
	}
	proc can {} { ## CAN: Cancel print data in page mode
		return "\x18"
	}
	proc rt_status {n} { ## DLE EOT n: Real-time status transmission
		return [format "\x10\x04%c" $n]
	}
	proc rt_request {n} { ## DLE ENQ n: Real-time request to printer
		return [format "\x10\x05%c" $n]
	}
	proc rt_pulse {n m t} { ## DLE DC4 n m t: Real-time output of specified pulse
		return [format "\x10\x14%c%c%c" $n $m $t]
	}
	proc pm_print {} { ## ESC FF: Print data in page mode
		return "\x1b\x0c"
	}
	proc set_right_space {n} { ## ESC SP n: Set character right space amount
		return [format "\x1b\x20%c" $n]
	}
	proc set_batch_print_mode {n} { ## ESC ! n: Batch specify print mode
		return [format "\x1b\x21%c" $n]
	}
	proc set_abs_pos {n} { ## ESC $ nL nH: Name Specify absolute position
		return [format "\x1b\x24%c%c" {*}[_nLnH $n]]
	}
	proc set_dl_charset {n} { ## ESC % n: Specify/cancel download character set
		return [format "\x1b\x25%c" $n]
	}
	## NOT IMPLEMENTED: ESC& yc1c2 [x1d1...d (y x x1) ] ... [axd1...d (yxax)]: Define download characters
	## NOT IMPLEMENTED: ESC * m nL nH d1...dk: Specify bit image mode
	proc ul {n} { ## ESC - n: Specify/cancels underline mode
		return [format "\x1b\x2d%c" $n]
	}
	proc set_default_linespacing {} { ## ESC 2: Set default line spacing
		return "\x1b\x32"
	}
	proc set_linefeed {n} { ## ESC 3 n: Set line feed amount
		return [format "\x1b\x33%c" $n]
	}
	proc set_device {n} { ## ESC = n: Select peripheral device
		return [format "\x1b\x3d%c" $n]
	}
	proc del_dl_characters {n} { ## ESC ? n: Delete download characters
		return [format "\x1b\x3f%c" $n]
	}
	proc init {} { ## ESC @: Initialize printer
		return "\x1b\x40"
	}
	## NOT IMPLEMENTED: ESC D n1...nk NUL: Set horizontal tab position
	proc set_emph_chars {n} { ## ESC E n: Specify/cancel emphasized characters
		return [format "\x1b\x45%c" $n]
	}
	proc set_double_printing {n} { ## ESC G n: Specify/cancel emphasized characters
		return [format "\x1b\x47%c" $n]
	}
	proc print_feed {n} { ## ESC J n: Print and Paper Feed
		return [format "\x1b\x4a%c" $n]
	}
	proc set_pm {} { ## ESC L: Select page mode
		return "\x1b\x4c"
	}
	proc set_font {n} { ## ESC M n: Select character font
		return [format "\x1b\x4d%c" $n]
	}
	proc small {} { return [set_font 1] }
	proc normal {} { return [set_font 0] }
	proc set_int_chars {n} { ## ESC R n: Select international characters
		return [format "\x1b\x52%c" $n]
	}
	proc set_sm {} { ## ESC S: Select standard mode
		return "\x1b\x53"
	}
	proc set_direction {n} { ## ESC T n: Select character print direction in page mode
		return [format "\x1b\x54%c" $n]
	}
	proc set_charrot90 {n} { ## ESC V n: Specify/cancel character 90 degree clockwise rotation
		return [format "\x1b\x56%c" $n]
	}
	## NOT IMPLEMENTED: ESC W xL xH yL yH dxL dxH dyL dyH: Set print region in page mode
	proc rel_pos {n} { ## ESC \ nL nH: Specify relative position
		return [format "\x1b\x5c%c%c" {*}[_nLnH $n]]
	}
	proc align {n} { ## ESC a n: Position Alignment
		return [format "\x1b\x61%c" $n]
	}
	proc left {} { return [align 0] }
	proc center {} { return [align 1] }
	proc right {} { return [align 2] }
	proc set_paperout_signal {n} { ## ESC c 3 n: Select paper out sensor to enable at paper out signal output
		return [format "\x1b\x63\x33%c" $n]
	}
	proc set_paperout_stop {n} { ## ESC c 4 n: Select paper out sensor to enable at printing stop
		return [format "\x1b\x63\x34%c" $n]
	}
	proc set_panel_switches {n} { ## ESC c 5 n: Enable/disable panel switches
		return [format "\x1b\x63\x35%c" $n]
	}
	proc feed {{n 2}} { ## ESC d n: Print and feed paper n lines
		return [format "\x1b\x64%c" $n]
	}
	proc pulse {m t1 t2} { ## ESC p m t1 t2: Specify pulse
		return [format "\x1b\x70%c%c%c" m t1 t2]
	}
	proc set_codepage {n} { ## ESC t n: Select character code table
		return [format "\x1b\x74%c" $n]
	}
	proc set_usd {n} { ## ESC 0x7b n: Specify/cancel upside-down printing
		return [format "\x1b\x7b%c" $n]
	}
	## NOT IMPLEMENTED: FS g 1 m a1 a2 a3 a4 nL nH d1 ... dk: Write data to user NV memory
	## NOT IMPLEMENTED: FS g 2 m a1 a2 a3 a4 nL nH: Read user NV memory data
	proc bitimg_nv {n {m 0}} { ## FS p n m: Print NV bit image
		return [format "\x1c\x70%c%c" $n $m]
	}
	## NOT IMPLEMENTED: FS q n [xL xH yL yH d1...dk] 1... [xL xH yL yH d1...dk] n: Define NV bit image
	proc set_size {n} { ## GS ! n: Select character size
		return [format "\x1d\x21%c" $n]
	}
	## NOT IMPLEMENTED: GS $ nL nH: Specify absolute position for character vertical direction in page mode
	## NOT IMPLEMENTED: GS * xy d1...d (xX yX 8): Define download bit images
	## NOT IMPLEMENTED: GS ( A pL pH n m: Test print
	## NOT IMPLEMENTED: GS ( K pL pH n m: Set print density
	## NOT IMPLEMENTED: GS ( N pL pH n m: Specify print color
	proc bitimg_dl {m} { ## GS / m: Print download bit images
		return [format "\x1d\x2f%c" $m]
	}
	proc macro {} { ## GS :: Start/execute macro definition
		return "\x1d\x3a"
	}
	proc set_bwinv {{n 1}} { ## GS B n: Specify/cancel white/black inverted printing
		return [format "\x1d\x42%c" $n]
	}
	proc unset_wbinv {} { return [SET_BWINV 0] }
	proc set_cpm {n m} { ## GS C 0 n m: Set counter print mode
		return [format "\x1d\x43\x30%c%c" $n $m]
	}
	proc set_cm {a b n r} { ## GS C 1 aL aH bL bH n r: Set Counter Mode (A)
		return [format "\x1d\x43\x31%c%c%c%c%c%c" {*}[_nLnH $a] {*}[_nLnH $b] $n $r]
	}
	proc set_cmv {n} { ## GS C 2 nL nH: Set counter mode value
		return [format "\x1d\x43\x32%c%c" {*}[_nLnH $n]]
	}
	## NOT IMPLEMENTED: GS C ; sa; sb; sn; sr; sc;: Set Counter Mode (B)
	proc set_speed {n} { ## GS E n: Set printing speed
		return [format "\x1d\x45%c" $n]
	}
	proc set_hri {n} { ## GS H n: Set HRI character print position
		return [format "\x1d\x48%c" $n]
	}
	proc send_printer_id {n} { ## GS I n: Transmission of Printer ID
		return [format "\x1d\x49%c" $n]
	}
	proc set_left_margin {n} { ## GS L nL nH: Set left margin
		return [format "\x1d\x4c%c%c" {*}[_nLnH $n]]
	}
	proc set_bcp {x y} { ## GS P x y: Set basic calculated pitch
		return [format "\x1d\x50%c%c" $x $y]
	}
	proc move_top {n} { ## GS T n: Move to top of line
		return [format "\x1d\x54%c" $n]
	}
	proc cut {{m 0} {n 0}} { ## GS V m: Cut paper
		if {$m == 65 || $m == 66} {
			return [format "\x1d\x56%c%c" $m $n]
		} else {
			return [format "\x1d\x56%c" $m]
		}
	}
	proc set_prw {n} { ## GS W nL nH: Set print region width
		return [format "\x1d\x57%c%c" {*}[_nLnH $n]]
	}
	## NOT IMPLEMENTED: GS \ nL nH: Specify relative position for character vertical direction in page mode
	## NOT IMPLEMENTED: GS ^ r t m: Execute macro
	## NOT IMPLEMENTED: GS a n: Enable/disable transmission of automatic status
	proc set_smoothing {n} { ## GS b n: Specify/cancel smoothing
		return [format "\x1d\x62%c" $n]
	}
	proc counter {} { ## GS c: Print counter
		return "\x1d\x63"
	}
	proc set_hri_font {n} { ## GS f n: Select HRI character font
		return [format "\x1d\x66%c" $n]
	}
	proc set_barcode_height {n} { ## GS h n: Set bar code height
		return [format "\x1d\x68%c" $n]
	}
	proc barcode {m foo} { ## 1. GS k m d1...dk NUL, 2. GS k m n d1...dk: Print bar code
		if {$m <= 6} {
			return [format "\x1d\x6b%c%s\x00" $m $foo]
		} else {
			return [format "\x1d\x6b%c%c%s" $m [string bytelength $foo] $foo]
		}
	}
	proc send_status {n} { ## GS r n: Transmission of status
		return [format "\x1d\x72%c" $n]
	}
	# proc bitimg {m x y d} { ## GS v 0 m xL Hy yL yH d1..dk: Print raster bit images
	# 	## for m=0 (180x180dpi): x*y is 512*831 max.
	# 	#return [format "\x1d\x76\x30%c%c%c%c%c%s" $m {*}[_nLnH [expr {($x+7) / 8}]] {*}[_nLnH $y] $d]
	# 	return [format "\x1d\x76\x30%c%c%c%c%c%s" $m {*}[_nLnH $x] {*}[_nLnH $y] $d]
	# }
	proc set_barcode_size {{n 3}} { ## GS w n: Set bar code horizontal size
		return [format "\x1d\x77%c" $n]
	}
	## not implemented: Chinese Character Control Commands
	
	proc img {fn} { ## load and print bit image
		if {![file readable $fn]} {
			puts stderr "ERROR: file not readable: $fn"
			return ":("
		}
		if {[info commands ::GD] eq ""} {
			puts stderr "ERROR: tclgd library not loaded"
			return ":("
		}
		
		set fp [open $fn r]
		fconfigure $fp -translation binary
		GD create_from_png gdimg $fp
		close $fp

		set lightgray [expr {[gdimg resolve_color 0x80 0x80 0x80] & 0xff}]
		# set white [expr {[gdimg closest_color 0xff 0xff 0xff] & 0xff}]
		# set black [expr {[gdimg closest_color 0x00 0x00 0x00] & 0xff}]
		set threshold_color $lightgray

		set da {}
		set w_real [gdimg width]
		set h_real [gdimg height]

		## align width by 32
		set w [expr {($w_real-1) + 32-(($w_real-1) % 32)}]
		set h $h_real

		for {set y 0} {$y < $h} {incr y} {
			for {set x 0} {$x < $w} {incr x 8} {
				set c 0
				for {set k 0} {$k < 8} {incr k} {
					if {$x + $k < $w_real} {
						set px [gdimg pixel [expr {$x + $k}] $y]
						set p [expr {(($px | ($px >> 8) | ($px >> 16)) & 0xff) < $threshold_color}]
					} else {
						## border for unaligned images
						set p 0
					}
					set c [expr {$c | ($p << (7-$k))}]
				}
				lappend da $c
			}
		}

		## print using GS v 0
		return [binary format "H*cssc[expr {$w/8*$h}]" {1d7630} 0 [expr {$w / 8}] $h $da]
	}
	
}

namespace eval ::eptmpl {
	proc readfile {filename} {
		set f [open $filename r]
		fconfigure $f -translation binary
		set text [read $f]
		close $f
		return $text
	}

	proc foreachline {varlist elemlist output} {
		set out ""
		foreach $varlist $elemlist {
			lappend out [subst $output]
		}
		return [join $out "\n"]
	}

	proc parse {text {init {}}} {
		namespace import ::escpos::*
		eval $init
		return [subst $text]
	}

	proc openprinter {url} {
		switch -regexp -matchvar foo -- $url {
			^serial:(.*?):(.*)$ {
				foreach {all dev fconf} $foo {}
				set ps [open $dev r+]
				fconfigure $ps {*}$fconf
			}
			^tcp://(.*?):(.*)$ {
				set ps [socket {*}[lrange $foo 1 end]]
			}
			default {error "unrecognised printer url $url"}
		}
		fconfigure $ps -translation binary -encoding binary -buffering none
		return $ps
	}

	proc send {url data {do_init 1}} {
		set ps [openprinter $url]
		if {$do_init} {
			# set data "[::escpos::init]$data[::escpos::feed 5][::escpos::cut]"
			set data "[::escpos::init]$data[::escpos::feed 6][::escpos::cut]"
		}
		puts -nonewline $ps $data
		close $ps
	}
}

