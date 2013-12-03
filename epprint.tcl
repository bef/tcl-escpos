#!/usr/bin/env tclsh8.5
## Author: Ben Fuhrmannek <bef@eventphone.de>
## Date: 2013-03-24
##
## Copyright (c) 2013 Ben Fuhrmannek
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

set auto_path [linsert $auto_path 0 [file dirname [info script]]]
package require escpos

if {![info exists ::env(ESCPOSURL)]} {
	puts stderr "ERROR: please set ESCPOSURL=..."
	puts stderr {Examples:}
	puts stderr {  export ESCPOSURL=tcp://192.168.40.113:9100}
	puts stderr {  export ESCPOSURL="serial:/dev/cu.usbserial-FTFJUHM7:-mode 19200,n,8,1 -handshake xonxoff -blocking 0 -buffering none"}
	puts stderr "OR:\n  ESCPOSURL=tcp://192.168.40.113:9100 $::argv0 ..."
	exit 1
}
set printurl $::env(ESCPOSURL)
set filename [lindex $::argv 0]

if {![file readable $filename] && $filename ne "-"} {
	puts stderr "Usage: $::argv0 <template file|->"
	exit 1
}

set text [eptmpl::readfile $filename]
#puts [eptmpl::parse $text]
eptmpl::send $printurl [eptmpl::parse $text]
