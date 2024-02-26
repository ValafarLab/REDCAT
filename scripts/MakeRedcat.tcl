#!/bin/sh
# the next line restarts using tclsh\
exec tclsh "$0" "$@"

proc parseSchema {pdb rdc start end schema ofile} {
	set schemaFile [open $schema r]
	set i 0
	set redcat [list]
	set a1 [list]
	set a2 [list]
	set gap [list]
	set dmax [list]
	while {[gets $schemaFile line] >=0 } {
		set lineList [split $line " "]
		lappend a1 [lindex $lineList 0]
		lappend a2 [lindex $lineList 1]
		lappend gap [lindex $lineList 2]
		lappend dmax [lindex $lineList 3]
		set outfile "temp.$i.out"
		incr i
	
	}
	getRDCCoords $ofile $pdb $rdc $start $end $a1 $a2 $gap $dmax "1"

}

proc getRDCCoords {outfile pdb rdc start end a1 a2 gap dmax err} {
	#print "$start $end $a1 $a2 $gap $dmax $error"
	set pdbIn [open $pdb r]
	set rcatOut [open $outfile w]
	#parse pdb file
	while {[gets $pdbIn oline] >=0} {
		set firstWord [string range $oline 0 3]
		if {$firstWord == "ATOM"} { 
	 		#found candidate
			set atom  [string range $oline 12 15]
			regsub -all " " $atom "" atom
			
			if {[lsearch $a1 $atom] != -1 || [lsearch $a2 $atom] != -1} {
					set resNum [string range $oline 22 25]
					regsub -all " " $resNum "" resNum
					
					set x [string range $oline 30 37]
					regsub -all " " $x "" x
					set y [string range $oline 38 45]
					regsub -all " " $y "" y			
					set z [string range $oline 46 53]
					regsub -all " " $z "" z
					#create dictionary of atoms relavent to schema
					set Redcat($atom,$resNum) "$x $y $z"
				
			}
			
		}
	
	}
	
	#for each residue between start and end
	for {set i $start} {$i<=$end} {incr i} {
		#for each rdc vector type included
		for {set j 0} {$j<[llength $a1]} {incr j} {
			set next [expr $i + [lindex $gap $j]]
			set atom1 [lindex $a1 $j]
			#look up atoms,residue in dictionary
			if {[info exists Redcat($atom1,$i)]} {
				set atom1 $Redcat($atom1,$i)
				
			} else {
				set atom1 "999 999 999"
			
			}
			set atom2 [lindex $a2 $j]
			#look up atoms,residue in dictionary
			if {[info exists Redcat($atom2,$next)]} {
				set atom2 $Redcat($atom2,$next)
				
			} else {
				set atom2 "999 999 999"
			
			}
			if {[info exists RDC($i)]} {
				set $rdc $RDC($i)
				set $err $ERR($i)
				
			} else {
				set $rdc "999"
				set $err "999"
				
			}
			puts $rcatOut "$atom1 $atom2 999 [lindex $dmax $j] $err /*[lindex $a1 $j]-[lindex $a2 $j] from $i*/"
			
		}
		
	}

	close $rcatOut
#	close $rdcIn
	close $pdbIn
	
}

set pdb [lindex $argv 0]
#set rdc [lindex $argv 1]
set rdc ""
set start [lindex $argv 1]
set end [lindex $argv 2]
set schema [lindex $argv 3]
set outfile [lindex $argv 4]

parseSchema $pdb $rdc $start $end $schema $outfile
