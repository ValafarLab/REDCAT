#!/usr/bin/tclsh

if {$argc != 5} {
	puts "Usage: <infile> <outfile> <alpha> <beta> <gamma>\n"
	exit 0
}

set infile [lindex $argv 0]
set outfile [lindex $argv 1]
set alpha [lindex $argv 2]
set beta [lindex $argv 3]
set gamma [lindex $argv 4]

set pi 3.141592
set a [expr {$alpha * $pi / 180}]
set b [expr {$beta * $pi / 180}]
set c [expr {$gamma * $pi / 180}]

# Create direction cosine matrix (this is directly out of Arfken)
# This does z,y,z rotation

set d(0) [expr {cos($c) * cos($b) * cos($a) - sin($c) * sin($a)}]
set d(1) [expr {cos($c) * cos($b) * sin($a) + sin($c) * cos($a)}]
set d(2) [expr {-cos($c) * sin($b)}]
set d(3) [expr {-sin($c) * cos($b) * cos($a) - cos($c) * sin($a)}]
set d(4) [expr {-sin($c) * cos($b) * sin($a) + cos($c) * cos($a)}]
set d(5) [expr {sin($c) * sin($b)}]
set d(6) [expr {cos($a) * sin($b)}]
set d(7) [expr {sin($a) * sin($b)}]
set d(8) [expr {cos($b)}]


set fin [open $infile r]
set fout [open $outfile w]

while {[gets $fin input] != -1} {
   if {![string compare "ATOM  " [string range $input 0 5]] ||
       ![string compare "HETATM" [string range $input 0 5]]} {
      set x [string range $input 30 37]
      set y [string range $input 38 45]
      set z [string range $input 46 53]

      set Rx [expr {$x * $d(0) + $y * $d(1) + $z * $d(2)}]
      set Ry [expr {$x * $d(3) + $y * $d(4) + $z * $d(5)}]
      set Rz [expr {$x * $d(6) + $y * $d(7) + $z * $d(8)}]

      set before [string range $input 0 29]
      set changed [format "%8.3f%8.3f%8.3f" $Rx $Ry $Rz]
      set after [string range $input 54 end]
      set output [set new_line "$before$changed$after"]
      puts $fout $output
   } else {
      puts $fout $input
   }
}

close $fin
close $fout
