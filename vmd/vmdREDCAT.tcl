package provide redcat 2.0
namespace eval redcat {
  namespace export *
  
}

proc redcat_tk_cb {} {
  ::redcat::redcat
  return $::redcat::mainWindow
}

proc ::redcat::redcat {} {
  variable ::redcat::Count
  variable ::redcat::X
  variable ::redcat::Sel
  variable ::redcat::destroyTensor
  variable ::redcat::selTensor
  variable ::redcat::Cal_RDC
  variable ::redcat::message_r
  variable ::redcat::message_q
  variable ::redcat::message_s
  variable ::redcat::ErrorPad
  variable ::redcat::solveArray
  variable ::redcat::plotArray
  variable ::redcat::errorArray
  variable ::redcat::rejectArray
  variable ::redcat::MCItr
  variable Error_Message
  variable ::redcat::Sxx
  variable ::redcat::Syy
  variable ::redcat::Szz
  variable ::redcat::a
  variable ::redcat::b
  variable ::redcat::c
  variable ::redcat::2DMapDatGlobal
  variable ::redcat::3DMapDatGlobal
  variable outfname
  variable iter1
  variable iter2
  variable range
  variable plot_type
  variable ::redcat::ErrorGlobal
  variable ::redcat::RemoveHzAbove
  variable fin
  variable fout
  variable ::redcat::plot2DFile
  variable ::redcat::plot3DFile

  # If already initialized, just turn on
  if { [winfo exists .top32] } {
    wm deiconify .top32
    return
  }

global auto_path
if {![info exists ::redcat::vTcl(sourcing)]} {

    # Provoke name search
    catch {package require bogus-package-name}
    set packageNames [package names]

    package require Tk
    switch $::tcl_platform(platform) {
	windows {
            option add *Button.padY 0
	}
	default {
            option add *Scrollbar.width 10
            option add *Scrollbar.highlightThickness 0
            option add *Scrollbar.elementBorderWidth 2
            option add *Scrollbar.borderWidth 2
	}
    }
    
    ##add our bwidget to path
    set rPathIn [open /usr/share/Redcat/redcat.conf r]
    gets $rPathIn rPath
    lappend auto_path "$rPath/vmd/bwidget-1.9.4"
    lappend auto_path "$rPath/vmd"
    
    package require BWidget
    switch $::tcl_platform(platform) {
	windows {
	}
	default {
	    option add *ScrolledWindow.size 14
	}
    }
    
    package require base64
    switch $::tcl_platform(platform) {
	windows {
	}
	default {
	    option add *ScrolledWindow.size 14
	}
    } 

}

#############################################################################
# Visual Tcl v1.60 Project
#


#############################################################################
# vTcl Code to Load Stock Fonts


if {![info exist ::redcat::vTcl(sourcing)]} {
set vTcl(fonts,counter) 0
#############################################################################
## Procedure:  vTcl:font:add_font

proc ::redcat::vTcl:font:add_font {font_descr font_type {newkey {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[info exists ::redcat::vTcl(fonts,$font_descr,object)]} {
        ## cool, it already exists
        return $::redcat::vTcl(fonts,$font_descr,object)
    }

     incr ::redcat::vTcl(fonts,counter)
     set newfont [eval font create $font_descr]
     lappend ::redcat::vTcl(fonts,objects) $newfont

     ## each font has its unique key so that when a project is
     ## reloaded, the key is used to find the font description
     if {$newkey == ""} {
          set newkey vTcl:font$::redcat::vTcl(fonts,counter)

          ## let's find an unused font key
          while {[vTcl:font:get_font $newkey] != ""} {
             incr ::redcat::vTcl(fonts,counter)
             set newkey vTcl:font$::redcat::vTcl(fonts,counter)
          }
     }

     set ::redcat::vTcl(fonts,$newfont,type)       $font_type
     set ::redcat::vTcl(fonts,$newfont,key)        $newkey
     set ::redcat::vTcl(fonts,$newfont,font_descr) $font_descr
     set ::redcat::vTcl(fonts,$font_descr,object)  $newfont
     set ::redcat::vTcl(fonts,$newkey,object)      $newfont

     lappend ::redcat::vTcl(fonts,$font_type) $newfont

     ## in case caller needs it
     return $newfont
}

#############################################################################
## Procedure:  vTcl:font:getFontFromDescr

proc ::redcat::vTcl:font:getFontFromDescr {font_descr} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[info exists ::redcat::vTcl(fonts,$font_descr,object)]} {
        return $::redcat::vTcl(fonts,$font_descr,object)
    } else {
        return ""
    }
}

vTcl:font:add_font \
    "-family courier -size 12" \
    stock \
    vTcl:font3
}
#################################
# vTcl LIBRARY PROCEDURES
#

if {![info exists ::redcat::vTcl(sourcing)]} {
#############################################################################
## Library Procedure:  Window

proc ::redcat::Window {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    variable ::redcat::vTcl
    foreach {cmd name newname} [lrange $args 0 2] {}
    set rest    [lrange $args 3 end]
    if {$name == "" || $cmd == ""} { return }
    if {$newname == ""} { set newname $name }
    if {$name == "."} { wm withdraw $name; return }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists} {
                wm deiconify $newname
            } elseif {[info procs ::redcat::vTclWindow$name] != ""} {
                eval "::redcat::vTclWindow$name $newname $rest"
            }
            if {[winfo exists $newname] && [wm state $newname] == "normal"} {
                ::redcat::vTcl:FireEvent $newname <<Show>>
            }
        }
        hide    {
            if {$exists} {
                wm withdraw $newname
                ::redcat::vTcl:FireEvent $newname <<Hide>>
                return}
        }
        iconify { if $exists {wm iconify $newname; return} }
        deiconify { if $exists {wm deiconify $newname; return} }
        destroy { if $exists {wm iconify $newname; return} }
    }
}
#############################################################################
## Library Procedure:  vTcl:DefineAlias

proc ::redcat::vTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    variable ::redcat::widget
    set widget($alias) $target
    set widget(rev,$target) $alias
    if {$cmdalias} {
        interp alias {} $alias {} $widgetProc $target
    }
    if {$top_or_alias != ""} {
        set widget($top_or_alias,$alias) $target
        if {$cmdalias} {
            interp alias {} $top_or_alias.$alias {} $widgetProc $target
        }
    }
}
#############################################################################
## Library Procedure:  vTcl:DoCmdOption

proc ::redcat::vTcl:DoCmdOption {target cmd} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## menus are considered toplevel windows
    set parent $target
    while {[winfo class $parent] == "Menu"} {
        set parent [winfo parent $parent]
    }

    regsub -all {\%widget} $cmd $target cmd
    regsub -all {\%top} $cmd [winfo toplevel $parent] cmd

    uplevel #0 [list eval $cmd]
}
#############################################################################
## Library Procedure:  vTcl:FireEvent

proc ::redcat::vTcl:FireEvent {target event {params {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## The window may have disappeared
    if {![winfo exists $target]} return
    ## Process each binding tag, looking for the event
    foreach bindtag [bindtags $target] {
        set tag_events [bind $bindtag]
        set stop_processing 0
        foreach tag_event $tag_events {
            if {$tag_event == $event} {
                set bind_code [bind $bindtag $tag_event]
                foreach rep "\{%W $target\} $params" {
                    regsub -all [lindex $rep 0] $bind_code [lindex $rep 1] bind_code
                }
                set result [catch {uplevel #0 $bind_code} errortext]
                if {$result == 3} {
                    ## break exception, stop processing
                    set stop_processing 1
                } elseif {$result != 0} {
                    bgerror $errortext
                }
                break
            }
        }
        if {$stop_processing} {break}
    }
}
#############################################################################
## Library Procedure:  vTcl:Toplevel:WidgetProc

proc ::redcat::vTcl:Toplevel:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[llength $args] == 0} {
        ## If no arguments, returns the path the alias points to
        return $w
    }
    set command [lindex $args 0]
    set args [lrange $args 1 end]
    switch -- [string tolower $command] {
        "setvar" {
            foreach {varname value} $args {}
            if {$value == ""} {
                return [set ::redcat::${w}::${varname}]
            } else {
                return [set ::redcat::${w}::${varname} $value]
            }
        }
        "hide" - "show" {
            ::redcat::Window [string tolower $command] $w
        }
        "showmodal" {
            ## modal dialog ends when window is destroyed
            ::redcat::Window show $w; raise $w
            grab $w; tkwait window $w; grab release $w
        }
        "startmodal" {
            ## ends when endmodal called
            ::redcat::Window show $w; raise $w
            set ::redcat::${w}::_modal 1
            grab $w; tkwait variable ::redcat::${w}::_modal; grab release $w
        }
        "endmodal" {
            ## ends modal dialog started with startmodal, argument is var name
            set ::redcat::${w}::_modal 0
            ::redcat::Window hide $w
        }
        default {
            uplevel $w $command $args
        }
    }
}
#############################################################################
## Library Procedure:  ::redcat::vTcl:WidgetProc

proc ::redcat::vTcl:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[llength $args] == 0} {
        ## If no arguments, returns the path the alias points to
        return $w
    }

    set command [lindex $args 0]
    set args [lrange $args 1 end]
    uplevel $w $command $args
}
#############################################################################
## Library Procedure:  ::redcat::vTcl:toplevel

proc ::redcat::vTcl:toplevel {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    uplevel #0 eval toplevel $args
    set target [lindex $args 0]
    namespace eval ::redcat::$target {set _modal 0}
}
}


#################################
# USER DEFINED PROCEDURES
#
#############################################################################
## Procedure:  main

proc ::redcat::main {argc argv} {

#############################
#if {$argc == 1} {
#        Get_Data $argv 
#}
#############################

}
#############################################################################
## Procedure:  Change_Selection

proc ::redcat::Change_Selection {mode} {
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::X

for {set i 1} {$i <= $::redcat::Count} {incr i} {
    if {$mode == 0} {
        set ::redcat::Sel($i) [expr 1 - $::redcat::Sel($i)]
    }
    if {$mode == 1} {
        set ::redcat::Sel($i) 1
    }
    if {$mode == 2} {
       set flag 0
       for {set j 1} {$j <= 7} {incr j} {
           if {$::redcat::X($j,$i) == 999} {
              set flag 1
           }
       }
       if {$flag == 1} { 
          set ::redcat::Sel($i) 0 
       }
     }  
}  
}
#############################################################################
## Procedure:  Get_Best_Sol

proc ::redcat::Get_Best_Sol {outfile} {
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel


set m 0

for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {($::redcat::Sel($i) == 1) && ($::redcat::X(7,$i) != "AVG")} {incr m}
}

set message [exec solve $m 5 svd.out b.txt 1 1 0 1 0]
::redcat::Window show .top37

set message [exec diag 3 solve.out $outfile.Best matrix.out.Best]

::redcat::Get_Solutions $outfile.Best "matrix.out.Best"
}
#############################################################################
## Procedure:  Get_Data

proc ::redcat::Get_Data {svd_input} {
variable ::redcat::X
variable ::redcat::Sel
variable ::redcat::Count
variable ::redcat::rCatFile
variable ::redcat::loaded

if {$::redcat::loaded >= 0} {
        ::redcat::errorDialog "Data Error" "Please close the current file before loading a new one."
	return

}

if {$svd_input == ""} {
   set svd_input [tk_getOpenFile]
   set ::redcat::rCatFile $svd_input
}

if { $svd_input == "" } {
    ::redcat::errorDialog "Data Error" "Input file name is blank.\nFile not loaded."
    return

}

set fin [open $svd_input r]
set ::redcat::Count 0

while {[gets $fin oline] >=0} {
 incr ::redcat::Count
 set num_subs [regsub -all "\t" $oline " " line]
 set data [split $line " "]
 set ::redcat::X(1,$::redcat::Count) [lindex $data 0]
 set ::redcat::X(2,$::redcat::Count) [lindex $data 1] 
 set ::redcat::X(3,$::redcat::Count) [lindex $data 2]
 set ::redcat::X(4,$::redcat::Count) [lindex $data 3]
 set ::redcat::X(5,$::redcat::Count) [lindex $data 4] 
 set ::redcat::X(6,$::redcat::Count) [lindex $data 5]  
 set ::redcat::X(7,$::redcat::Count) [lindex $data 6] 
 set ::redcat::X(8,$::redcat::Count) [lindex $data 7] 
 set ::redcat::X(9,$::redcat::Count) [lindex $data 8]
 set ::redcat::X(10,$::redcat::Count) [lrange $data 9 end]
}
close $fin

for {set i 1} {$i<=$::redcat::Count} {incr i} {
    set ::redcat::Sel($i) 1

}

set ::redcat::loaded 0
#::redcat::Make_Win
}
#############################################################################
## Procedure:  Get_Dynamic

proc ::redcat::Get_Dynamic {fin fout fstate numstate Sxx Syy Szz} {
variable ::redcat::widget
variable ::redcat::X
variable ::redcat::Sel
variable ::redcat::Count

if {[::redcat::Save_Data "$fin" 0]==0} {
    ::redcat::dataLoadedError
    return

}
exec Dynamic $fin $fout $fstate $numstate $Sxx $Syy $Szz

set resultfile [open $fout r]

for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {[gets $resultfile line] >=0} { 
     if {$::redcat::Sel($i) == 1} {
        set result [split $line " "]
        set ::redcat::X(7,$i) [lindex $result 6]
     }
  } else {
        ::redcat::errorDialog "Error !" "Inconsistency in number of entries\n"
  } 
}
}
#############################################################################
## Procedure:  Get_Solutions

proc ::redcat::Get_Solutions {outfname matrix} {
variable ::redcat::widget

::redcat::Window show .top37
set fin [open $outfname r]
set count 0


set results [exec euler $matrix]
set angles [split $results \n]
.top37.cpd32.03 insert end "      Sxx                Syy               Szz               a                b               c               Eta	           GDO  \n"
while {[gets $fin line] >=0 } {
# Each order parameter is repeated twice. For a solution and its inversion.
    gets $fin line
    scan $line {%f %f %f %f %f %f %f %f %f} ::redcat::X(1) ::redcat::X(2) ::redcat::X(3) ::redcat::X(4) ::redcat::X(5) ::redcat::X(6) Sxx Syy Szz


    set GDO [expr sqrt([expr {2*($Sxx*$Sxx + $Syy*$Syy + $Szz*$Szz)/3}])]
    set Eta [expr ($Sxx - $Syy) / $Szz]
        
    .top37.cpd32.03 insert end [format "%e %e %e " $Sxx $Syy $Szz]
    .top37.cpd32.03 insert end [lindex $angles $count]
    .top37.cpd32.03 insert end [format " %e %e" $Eta $GDO]
    .top37.cpd32.03 insert end "\n"

    incr count
}
close $fin
}
#############################################################################
## Procedure:  Get_Sub_RDC

proc ::redcat::Get_Sub_RDC {Sxx Syy Szz a b c error Sub_RDC Corr_Plot {printCorrPlot 0}} {
variable ::redcat::widget
variable ::redcat::X
variable ::redcat::Cal_RDC
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::rejectArray
variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s

if {[::redcat::Save_Data "Redcat.in" 2]==0} {
	::redcat::dataLoadedError
	return

}

if {$error == -1 } {
    if {[::redcat::checkRun]!=0} {
    	set backCalcString [exec Redcat -q -i Redcat.in -c $Sxx $Syy $Szz $a $b $c y]
    
    } else {
        #RunError
        ::redcat::errorDialog "Empty Matrix" "Not enough equations selected.\nPlease Select five or more equations from the Main window."
        return
        
      }
 
} else {
      if {[::redcat::checkRun]!=0} {
         set backCalcString [exec Redcat -q -i Redcat.in -c $Sxx $Syy $Szz $a $b $c n $error]
         
      } else {
         #RunError
         ::redcat::errorDialog "Empty Matrix" "Not enough equations selected.\nPlease Select five or more equations from the Main window."
         return
         
      }
    
}
set graph1 "corrPlot.png"
set backCalcList [split $backCalcString "\t"]

set backRDC "[lindex $backCalcList 1]"
set rmsd "[lindex $backCalcList 2]"
set qfac "[lindex $backCalcList 3]"
set sfac "[lindex $backCalcList 4]"

set rdcList [split $backRDC "\n"]

set ::redcat::Cal_RDC [lrange $rdcList 1 end-1]

#sub if necessary
if {$Sub_RDC == 1} {
    for {set i 1} {$i <= $::redcat::Count} {incr i} {
        if {$::redcat::Sel($i) == 1} {
            set ::redcat::X(7,$i) [lindex $::redcat::Cal_RDC [expr $i-1]]
        }
    }

}

regexp {[0-9]+.*[0-9]*$} $rmsd rmsd
regexp {[0-9]+.*[0-9]*$} $qfac qfac
regexp {[0-9]+.*[0-9]*$} $sfac sfac

set ::redcat::message_r "RMSD = $rmsd"
set ::redcat::message_q "Q-Factor = $qfac"
set ::redcat::message_s "S-Factor = $sfac"

if { $Corr_Plot == 1 } {
    set PlotCorr [open "|gnuplot -persist" w+]
    flush $PlotCorr
    fconfigure $PlotCorr -buffering none
    puts $PlotCorr "set mouse"
    if {$printCorrPlot!=0} {
        puts $PlotCorr "set term pbm color size 300,300; set output '$printCorrPlot'"
        
    }
    puts $PlotCorr "set key off"
    puts $PlotCorr "set xlabel \"Computed\""
    puts $PlotCorr "set ylabel \"Measured\""
    puts $PlotCorr "f(x)=a*x+b"
    puts $PlotCorr "fit f(x) '-' via a,b"

    for { set i 1 } { $i <= $::redcat::Count } { incr i } {
        if { ($::redcat::Sel($i) == 1) && ($::redcat::X(7,$i) != "AVG" && [lindex $::redcat::rejectArray $i] != "...") } {
            set C($i) [lindex $::redcat::Cal_RDC [expr $i -1]]
            if {$C($i) != 999} {
                puts $PlotCorr "$C($i) $::redcat::X(7,$i)"
                
            }
            
        }
        
    }

    puts $PlotCorr "e"
    puts $PlotCorr "plot '-', f(x)"
    for { set i 1 } { $i <= $::redcat::Count } { incr i } {
        if {($::redcat::Sel($i) == 1) && ($::redcat::X(7,$i) != "AVG" && [lindex $::redcat::rejectArray $i] != "...") } {
            if {$C($i) != 999} {
                puts $PlotCorr "$C($i) $::redcat::X(7,$i)"

            }
            
        }
        
    }
    
    puts $PlotCorr "e\n"
    
}
catch { close $PlotCorr } results
}
#############################################################################
## Procedure:  Make_Win

proc ::redcat::Make_Win {} {
variable ::redcat::X;
variable ::redcat::Count;
variable ::redcat::Sel;
variable ::redcat::loaded;

if {$::redcat::loaded != 0} {
	return
	
}


set ba_t .top32.cpd38.03
set ba_f $ba_t.fra0
frame $ba_f

label $ba_f.lab1 -text  "Eq.#  "
label $ba_f.lab2 -text  "    "
label $ba_f.lab3 -text  "X1       "
label $ba_f.lab4 -text  "Y1       "
label $ba_f.lab5 -text  "Z1       "
label $ba_f.lab6 -text  "X2       "
label $ba_f.lab7 -text  "Y2       "
label $ba_f.lab8 -text  "Z2       "
label $ba_f.lab9 -text  "Dipol 1  "
# checkbutton $ba_f.cb1 -variable ::redcat::Dip(1) -selectcolor #ff0c04
#label $ba_f.lab12 -text "Diopl 2  "
# checkbutton $ba_f.cb2 -variable ::redcat::Dip(2) -selectcolor #ff0c04
#label $ba_f.lab13 -text "Diopl 3  "
# checkbutton $ba_f.cb3 -variable ::redcat::Dip(3) -selectcolor #ff0c04
label $ba_f.lab10 -text "Error    "
label $ba_f.lab11 -text "Comments "
grid $ba_f.lab1 $ba_f.lab2 $ba_f.lab3 $ba_f.lab4 $ba_f.lab5  $ba_f.lab6   $ba_f.lab7 $ba_f.lab8 $ba_f.lab9 $ba_f.lab10 $ba_f.lab11
$ba_t window create end -window $ba_f

for {set i 1} {$i <= $::redcat::Count} {incr i} {
 set ba_f $ba_t.fra$i
 frame $ba_f
 
 label $ba_f.ln$i -text "$i) "
 if { $::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999 || $::redcat::X(7,$i) == 999 } {
   set ::redcat::Sel($i) 0
 } else {
   set ::redcat::Sel($i) 1
 }
 checkbutton $ba_f.rb$i -variable ::redcat::Sel($i) -selectcolor #ff0c04

 entry $ba_f.x1$i -textvariable ::redcat::X(1,$i) -width 6
 entry $ba_f.y1$i -textvariable ::redcat::X(2,$i) -width 6
 entry $ba_f.z1$i -textvariable ::redcat::X(3,$i) -width 6
 entry $ba_f.x2$i -textvariable ::redcat::X(4,$i) -width 6
 entry $ba_f.y2$i -textvariable ::redcat::X(5,$i) -width 6
 entry $ba_f.z2$i -textvariable ::redcat::X(6,$i) -width 6
 entry $ba_f.d$i -textvariable ::redcat::X(7,$i) -width 6
 #entry $ba_f.d$i -textvariable ::redcat::X(11,$i) -width 6
 #entry $ba_f.d$i -textvariable ::redcat::X(12,$i) -width 6
 entry $ba_f.e$i -textvariable ::redcat::X(9,$i) -width 6
 entry $ba_f.c$i -textvariable ::redcat::X(10,$i) -width 15


 grid $ba_f.ln$i $ba_f.rb$i $ba_f.x1$i $ba_f.y1$i  $ba_f.z1$i $ba_f.x2$i  $ba_f.y2$i  $ba_f.z2$i $ba_f.d$i  $ba_f.e$i $ba_f.c$i
 $ba_t window create end -window $ba_f
}
set ::redcat::loaded 1
}
#############################################################################
## Procedure:  Rotate_Coord

proc ::redcat::Rotate_Coord {type a b c theta} {
variable ::redcat::Count
variable ::redcat::X
variable ::redcat::Sel

set a [::redcat::parseSci $a]
set b [::redcat::parseSci $b]
set c [::redcat::parseSci $c]

set pi 3.1415926535897932
if {$type == 1} {
    set a [expr {$a * $pi / 180}]
    set b [expr {$b * $pi / 180}]
    set c [expr {$c * $pi / 180}]
    set d(0) [expr {cos($c) * cos($b) * cos($a) - sin($c) * sin($a)}]
    set d(1) [expr {cos($c) * cos($b) * sin($a) + sin($c) * cos($a)}]
    set d(2) [expr {-cos($c) * sin($b)}]
    set d(3) [expr {-sin($c) * cos($b) * cos($a) - cos($c) * sin($a)}]
    set d(4) [expr {-sin($c) * cos($b) * sin($a) + cos($c) * cos($a)}]
    set d(5) [expr {sin($c) * sin($b)}]
    set d(6) [expr {cos($a) * sin($b)}]
    set d(7) [expr {sin($a) * sin($b)}]
    set d(8) [expr {cos($b)}]
} else {  
    set norm [expr sqrt([expr {$a * $a + $b * $b + $c * $c}])]
    set a [expr $a / $norm]
    set b [expr $b / $norm]
    set c [expr $c / $norm]
    set theta [expr $theta * $pi / 180]
    
    set ct [expr cos($theta)]
    set st [expr sin($theta)]
    set d(0) [expr {$a * $a + $ct * ($b * $b + $c * $c)}]
    set d(1) [expr {$a * $b * (1.0 - $ct) - $c * $st}]
    set d(2) [expr {$a * $c * (1.0 - $ct) + $b * $st}]
    set d(3) [expr {$a * $b * (1.0 - $ct) + $c * $st}]
    set d(4) [expr {$b * $b + $ct * ($a * $a + $c * $c)}]
    set d(5) [expr {$c * $b * (1.0 - $ct) - $a * $st}]
    set d(6) [expr {$a * $c * (1.0 - $ct) - $b * $st}]
    set d(7) [expr {$c * $b * (1.0 - $ct) + $a * $st}]
    set d(8) [expr {$c * $c + $ct * ($b * $b + $a * $a)}]
} 

for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {$::redcat::Sel($i) == 1} {
    set newx1 [expr {$::redcat::X(1,$i) * $d(0) + $::redcat::X(2,$i) * $d(1) + $::redcat::X(3,$i) * $d(2)}]
    set newy1 [expr {$::redcat::X(1,$i) * $d(3) + $::redcat::X(2,$i) * $d(4) + $::redcat::X(3,$i) * $d(5)}]
    set newz1 [expr {$::redcat::X(1,$i) * $d(6) + $::redcat::X(2,$i) * $d(7) + $::redcat::X(3,$i) * $d(8)}]
    
    set newx2 [expr {$::redcat::X(4,$i) * $d(0) + $::redcat::X(5,$i) * $d(1) + $::redcat::X(6,$i) * $d(2)}]
    set newy2 [expr {$::redcat::X(4,$i) * $d(3) + $::redcat::X(5,$i) * $d(4) + $::redcat::X(6,$i) * $d(5)}]
    set newz2 [expr {$::redcat::X(4,$i) * $d(6) + $::redcat::X(5,$i) * $d(7) + $::redcat::X(6,$i) * $d(8)}]
    
    set ::redcat::X(1,$i) $newx1
    set ::redcat::X(2,$i) $newy1
    set ::redcat::X(3,$i) $newz1
    set ::redcat::X(4,$i) $newx2
    set ::redcat::X(5,$i) $newy2
    set ::redcat::X(6,$i) $newz2

  }

}

}
#############################################################################
## Procedure:  Run_Error_Analysis

proc ::redcat::Run_Error_Analysis {get_error} {
# If get_error = 2 then only replace the error values for the violations.
# If get_error = 1 then replace the error values
# If get_error = 0 then just report e
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::ErrorPad
variable ::redcat::errorArray

if {$get_error != 0} {
    set e 0
    for {set i 1} {$i <= $::redcat::Count} {incr i} {
       set err [lindex $::redcat::errorArray $e]
       if {($::redcat::Sel($i) != 0) && ($::redcat::X(7,$i) != "AVG") && !($::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999 || $::redcat::X(7,$i) == 999)} {
           if {$get_error == 1} {
               set ::redcat::X(9,$i) [expr 0.1 + $err]
               
           }
           if {$get_error == 2} {
               if {$::redcat::X(9,$i) < $err} {
                  set ::redcat::X(9,$i) [expr $::redcat::ErrorPad + $err]
                  
	       }

           }           
	   incr e

       }
       
    }
    
} else {
    ::redcat::printRun
    
}
}
#############################################################################
## Procedure:  Run_SVD

proc ::redcat::Run_SVD {saveType} {
variable ::redcat::widget
variable ::redcat::MCItr
variable ::redcat::solveArray
variable ::redcat::plotArray
variable ::redcat::errorArray
variable ::redcat::rejectArray

set ::redcat::solveArray [list]
set ::redcat::plotArray [list]
set ::redcat::errorArray [list]
set ::redcat::rejectArray [list]

if {[::redcat::Save_Data "Redcat.in" $saveType]==0} {
    ::redcat::dataLoadedError
    return

}

if {[::redcat::checkRun]!=0} {
   set msg [exec Redcat -q -i "Redcat.in" -s $::redcat::MCItr]
   
} else {
   #RunError
   ::redcat::errorDialog "Empty Matrix" "Not enough equations selected.\nPlease Select five or more equations from the Main window."
   return 0
 
}
	
set msgList [split $msg {=} ]

set rejAndErr [lindex $msgList 2]
set solveString [lindex $msgList 4]

regsub -all {Rejections by Equation [0-9]+: } $rejAndErr "" rejAndErr
regsub -all {Equation [0-9]+ excluded ...} $rejAndErr "...\t" rejAndErr

set rejAndErrArray [split $rejAndErr "\n"]

lappend rejectArray [lindex $rejAndErrArray 1]
#populate reject and error array
for { set i 4 } { $i < [llength $rejAndErrArray] } { incr i } {
    if {[lindex $rejAndErrArray $i] == ""} {
        continue
    }
    if {[regexp -all {[.]{3}} [lindex $rejAndErrArray $i]] < 1} {
        set temp [split [lindex $rejAndErrArray $i] "\t"]
        lappend ::redcat::rejectArray [lindex $temp 0]
        lappend ::redcat::errorArray [lindex $temp 1]
        
    } else {
        lappend rejectArray "..."
        
    }

}

#split strings to lists on \n's
set solveAndPlotArray [split $solveString "\n"]
set solveString {}
set plotString {}

#split out solve and plot
for { set i 0 } { $i < 5 } { incr i } {
     append solveString [lindex $solveAndPlotArray [expr $i]]
	append solveString "\n"
	
}

for { set i 7 } { $i < [llength $solveAndPlotArray] } { set i [expr $i + 3] } {
	append solveString [lindex $solveAndPlotArray [expr $i - 2]]
	append solveString "\n"
	
	append plotString [lindex $solveAndPlotArray [expr $i - 1]]
	append plotString [lindex $solveAndPlotArray [expr $i]]
	append plotString "\n"
	
}

set ::redcat::solveArray [split $solveString "\n"]
regsub -all {[\t]+} $plotString "" plotString
set ::redcat::plotArray [split $plotString "\n"]

return 1
}
#############################################################################
## Procedure:  Save_Data

proc ::redcat::Save_Data {fname selected} {
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::X
variable ::redcat::RemoveHzAbove

if { [array exists ::redcat::X] } {
    if {$fname == ""} {
       set saveName [tk_getSaveFile]
       if { $saveName == "" } {
           return
           
       }
       set fout [open $saveName w]
    } else {
       set fout [open $fname w]
    }

    if {$selected == 0} {
       for {set i 1} {$i <= $::redcat::Count} {incr i} {
            puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) $::redcat::X(9,$i) $::redcat::X(10,$i)"
            
       }
       
    }
    if {$selected == 1} {
       for {set i 1} {$i <= $::redcat::Count} {incr i} {
         if {$::redcat::Sel($i) == 1} { 
             puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) $::redcat::X(9,$i) $::redcat::X(10,$i)"
             
         } 
         
       }
       
    }
    if {$selected == 2} {
        for {set i 1} {$i <= $::redcat::Count} {incr i} {
            if {$::redcat::Sel($i) == 1} {
                puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) $::redcat::X(9,$i) $::redcat::X(10,$i)"
            
            } else {
                puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) 999 $::redcat::X(8,$i) $::redcat::X(9,$i) $::redcat::X(10,$i)"
            
            }
        
        }
        
    }
    if {$selected == 3} {
        for {set i 1} {$i <= $::redcat::Count} {incr i} {
            if {$::redcat::Sel($i) == 1} {
                puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) $::redcat::RemoveHzAbove $::redcat::X(10,$i)"
            
            }
        
        }
        
    }
    
    close $fout
    return 1
} else {
    return 0
    
}
}
#############################################################################
## Procedure:  Write_Data

proc ::redcat::Write_Data {Aname bname} {
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::Error_message

set Afile [open $Aname w]
set bfile [open $bname w]

set xy 0
set xz 0
set yy 0
set yz 0
set zz 0
set avg_counter 0

for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {$::redcat::Sel($i) == 1} {
     set dx  [expr $::redcat::X(4,$i)-$::redcat::X(1,$i)]
     set dy  [expr $::redcat::X(5,$i)-$::redcat::X(2,$i)]
     set dz  [expr $::redcat::X(6,$i)-$::redcat::X(3,$i)]
     set r   [expr sqrt($dx*$dx + $dy*$dy +$dz*$dz)]
     set d   $::redcat::X(7,$i)
     set max $::redcat::X(8,$i)
     set e   $::redcat::X(9,$i)

     if {$r != 0} {
       set x [expr $dx/$r]
       set y [expr $dy/$r]
       set z [expr $dz/$r]
       set r [expr $r*$r*$r]
     }
     
     set xy [expr 2*$x*$y*$max/$r + $xy]
     set xz [expr 2*$x*$z*$max/$r + $xz]
     set yy [expr ($y*$y-$x*$x)*$max/$r + $yy]
     set yz [expr 2*$y*$z*$max/$r + $yz]
     set zz [expr ($z*$z-$x*$x)*$max/$r + $zz] 
     incr avg_counter
              
     if {$::redcat::X(7,$i) != "AVG"} {
        set xy [expr $xy / $avg_counter]
        set xz [expr $xz / $avg_counter]
        set yy [expr $yy / $avg_counter]
        set yz [expr $yz / $avg_counter]
        set zz [expr $zz / $avg_counter]    
        puts $Afile "$xy $xz $yy $yz $zz"
        puts $bfile "$d $e"
        set xy 0
        set xz 0
        set yy 0
        set yz 0
        set zz 0    
        set avg_counter 0
     } 
  }
}
close $Afile
close $bfile
}
#############################################################################
## Procedure:  Get_REDCAT_Line

proc ::redcat::Get_REDCAT_Line {atom1 atom2 startat delta fin} {
variable ::redcat::widget

set stopat [expr $startat + $delta]
set flag_atom1 0
set flag_atom2 0

set index1 1
set index2 1
while { [gets $fin input] != -1 } {
   incr index1
   set temp [string range $input 0 5]
   regsub -all { } $temp "" header
   if { ![string compare "ATOM" $header] } {
	  incr index2
       set temp [string range $input 22 25]
       regsub -all { } $temp "" resnum
       set temp [string range $input 12 15]
       regsub -all { } $temp "" atom

       if { ($resnum == $startat) && ($atom1 == $atom) } {
          set flag_atom1 1;
          set temp [string range $input 30 37]
          regsub -all { } $temp "" atom1_x
          set temp [string range $input 38 45]
          regsub -all { } $temp "" atom1_y
          set temp [string range $input 46 53]
          regsub -all { } $temp "" atom1_z
       }

       if { ($resnum == $stopat) && ($atom2 == $atom) } {
          set flag_atom2 1;
          set temp [string range $input 30 37]
          regsub -all { } $temp "" atom2_x
          set temp [string range $input 38 45]
          regsub -all { } $temp "" atom2_y
          set temp [string range $input 46 53]
          regsub -all { } $temp "" atom2_z
       }

       if { ($flag_atom1 == 1) && ($flag_atom2 == 1) } {
             return "$atom1_x $atom1_y $atom1_z $atom2_x $atom2_y $atom2_z"
       }
   }
}
set type ZZZ
return "999 999 999 999 999 999"
}
#############################################################################
## Procedure:  ApplySelection

proc ::redcat::ApplySelection {Start Stop} {
variable ::redcat::Count
variable ::redcat::Sel

if {$Start <=0 } {
   set Start 1
}

if {$Stop > $::redcat::Count} {
   set Stop $::redcat::Count
}

for {set i $Start} {$i <= $Stop} {incr i} {
     set ::redcat::Sel($i) [expr 1 - $::redcat::Sel($i)]
}
}
#############################################################################
## Procedure:  Parse_Selection

proc ::redcat::Parse_Selection {Selection} {
variable ::redcat::widget

regsub -all " " $Selection "" ProcessedSelection
set Split_Selection [split $ProcessedSelection ","]

for {set i 0} {$i < [llength $Split_Selection]} {incr i} {
  set Range [split [lindex $Split_Selection $i] "-"]
  if {[llength $Range] == 2} {
    ::redcat::ApplySelection [lindex $Range 0] [lindex $Range 1]
  }
  
  if {[llength $Range] == 1} {
    if { [lindex $Range 0] == "*" } {
       ::redcat::Change_Selection 1
    } elseif { [lindex $Range 0] == "!" } {
       ::redcat::Change_Selection 0
    } elseif { [lindex $Range 0] == "~" } {
       ::redcat::Change_Selection 2
    } else {
      ::redcat::ApplySelection [lindex $Range 0] [lindex $Range 0]
    }
  }
}
}
#############################################################################
## Procedure:  LaunchGnuInt

proc ::redcat::LaunchGnuInt {} {
variable ::redcat::widget
variable ::redcat::MapDatGlobal
if { $::redcat::MapDatGlobal == "" } {
	exec GnuInt.tcl &

} else {
	exec GnuInt.tcl 0 $::redcat::MapDatGlobal &

}
}
#############################################################################
## Procedure:  LaunchGnu3DInt

proc ::redcat::LaunchGnu3DInt {} {
exec Gnu3DInt.tcl 1 &
}
#############################################################################
## Procedure:  msg

proc ::redcat::msg {} {
variable ::redcat::widget

::redcat::Window show .top37
}
#############################################################################
## Procedure:  Print

proc ::redcat::Print {mesg} {
variable ::redcat::widget

.top37.cpd32.03 insert end "$mesg"
}
#############################################################################
## Procedure:  genReport

proc ::redcat::genReport {outfile hz} {
variable ::redcat::widget
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::MapDatGlobal

#set graph outputs for 2d and 3d
set graph1 "graph1.gif"
set graph2 "graph2.gif"
set graph3 "graph3.gif"

if {$::redcat::MapDatGlobal == ""} {
    set 2dmap map.dat
} else {
    set 2dmap $::redcat::MapDatGlobal
}

#open window
::redcat::Window show .fullReport

#solve.out, matrix.out.Best
::redcat::Write_Data "A.txt" "b.txt"
set m 0
for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {($::redcat::Sel($i) == 1) && ($::redcat::X(7,$i) != "AVG")} {incr m}
}

set rank [exec svd $m 5 A.txt]

set rejections [exec solve $m 5 svd.out b.txt 1 1 0 1 0]
set sysRank [exec diag 3 solve.out $outfile.Best matrix.out.Best]

set j 1
set k 1
set m 1
for {set i 1} {$i <= $::redcat::Count} {incr i} {
      set result [split $rejections \n]
      if {$::redcat::Sel($i) != 0 && $::redcat::X(7,$i) != "AVG"} {
        regexp {[0-9]+.[0-9]+$} [lindex $result $m] error
        #add equation number to array
        if {$error <= $hz} {
            set err($k) $error
            incr k
        } else {
            set ::redcat::Sel($i) 0;
            set oldSel($j) $i
            set rmRdc($j) $i
            set rmErr($j) $error
            incr j
        }
      }
      incr m
}

#call Write_Data to correct
::redcat::Write_Data "A.error.txt" "b.txt"


#actual analysis of data below error

#solve.out, matrix.out.Best
set m 0
for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {($::redcat::Sel($i) == 1) && ($::redcat::X(7,$i) != "AVG")} {incr m}
}

set rank [exec svd $m 5 A.txt]

if {$rank == 5} {
    FullReportText insert end "System is fully determined (rank = $rank)\n"
} else {
    FullReportText insert end "System is under determined (rank = $rank)\nSampling Null_Space\n"
}

set rejections [exec solve $m 5 svd.out b.txt 1 1 0 1 0]
set sysRank [exec diag 3 solve.out $outfile.Best matrix.out.Best]

set m 1
for {set i 1} {$i <= $::redcat::Count} {incr i} {
      set result [split $rejections \n]
      if {$::redcat::Sel($i) != 0 && $::redcat::X(7,$i) != "AVG"} {
        regexp {[0-9]+.[0-9]+$} [lindex $result $m] error
      }
      incr m
}

#print matrix.out.Best
set fileid [open "matrix.out.Best" r]
set matrix [read -nonewline $fileid]
close $fileid
FullReportText insert end "Best S:\n$matrix\n\n"

#print out data for best order tensor
set fin [open "Results.dat.Best" r]
set count 0

set results [exec euler matrix.out.Best]
set angles [split $results \n]
FullReportText insert end "   Sxx          Syy          Szz           a         b          c          Eta         GDO\n"
while {[gets $fin line] >=0 } {
    gets $fin line
    scan $line {%f %f %f %f %f %f %f %f %f} ::redcat::X(1) ::redcat::X(2) ::redcat::X(3) ::redcat::X(4) ::redcat::X(5) ::redcat::X(6) Sxx Syy Szz


    set GDO [expr sqrt([expr {2*($Sxx*$Sxx + $Syy*$Syy + $Szz*$Szz)/3}])]
    set Eta [expr ($Sxx - $Syy) / $Szz]
        
    FullReportText insert end [format "%e %e %e " $Sxx $Syy $Szz]
    FullReportText insert end [lindex $angles $count]
    FullReportText insert end [format " %e %e" $Eta $GDO]
    FullReportText insert end "\n"

    incr count
}
close $fin

#Print Da, Dr, and R
FullReportText insert end "\nC-N Da: [::redcat::calcDa $Szz 6125.0]"
FullReportText insert end "\nN-H Da: [::redcat::calcDa $Szz 24350.0]"
FullReportText insert end "\nC-H Da: [::redcat::calcDa $Szz -60400.0]"
FullReportText insert end "\nH-H Da: [::redcat::calcDa $Szz -240200.0]"

FullReportText insert end "\n\nDr:\n"
FullReportText insert end [expr ($Sxx - $Syy)]
#"[expr (2.0/3.0) * (($::redcat::X(1) - $::redcat::X(2))/$::redcat::X(3))]\n"
FullReportText insert end "\n\nR:\n"
FullReportText insert end [::redcat::calcR $Sxx $Syy $Szz]


#create gif of graph1
set fileid [open "|gnuplot" w+]
puts $fileid "set term gif size 325,325\n"
puts $fileid "set output '$graph1'\n"
puts $fileid "set size square\n"
puts $fileid "set nokey\n"
puts $fileid "plot 'map.dat' notitle w d lt -1, 'Results.dat' using 1:2 t 'Sxx' w point pointtype 1 pointsize 1, 'Results.dat.Best' using 1:2 t 'Best Sxx' w point pointtype 7 pointsize 0.5, 'Results.dat' using 3:4 t 'Syy' w point pointtype 2 pointsize 1, 'Results.dat.Best' using 3:4 t 'Best Syy' w point pointtype 7 pointsize 0.5, 'Results.dat' using 5:6 t 'Szz' w point pointtype 3 pointsize 1, 'Results.dat.Best' using 5:6 t 'Best Szz' w point pointtype 7 pointsize 0.5\n"
puts $fileid "exit\n"
close $fileid

#create gif of graph2
set fileid [open "|gnuplot" w+]
puts $fileid "set term gif size 400,400\n"
puts $fileid "set output '$graph2'\n"
puts $fileid "set size square\n"
puts $fileid "set view 90, 0\n"
puts $fileid "splot 'map3D.dat'notitle w d lt -1, 'Results.dat' using 1:2:7 t 'Sxx' w point pointtype 1 pointsize 1, 'Results.dat.Best' using 1:2:7 t 'Best' w point pointtype 7 pointsize 0.5, 'Results.dat' using 3:4:8 t 'Syy' w point pointtype 2 pointsize 1, 'Results.dat.Best' using 3:4:8 t 'Best' w point pointtype 7 pointsize 0.5, 'Results.dat' using 5:6:9 t 'Szz' w point pointtype 3 pointsize 1, 'Results.dat.Best' using 5:6:9 t 'Best' w point pointtype 7 pointsize 0.5\n"
puts $fileid "exit\n"
close $fileid

#perform error analysis
set fileid [open "|gnuplot" w+]
puts $fileid "set term gif size 640,325\n"
puts $fileid "set output '$graph3'\n"
puts $fileid "plot '-' with impulses\n"
foreach key [array names err] {
    puts $fileid "$err($key)\n"
}
puts $fileid "e\n"
puts $fileid "exit\n"
close $fileid

#show error analysis
#show graph1
FullReportText insert end "\n\n"
FullReportText image create {end linestart} -image [image create photo -file $graph1]

#show  graph2
FullReportText image create {end linestart} -image [image create photo -file $graph2]

#show graph3
FullReportText insert end "\n\n"
FullReportText insert end "Error Analysis:\n"
FullReportText image create {end linestart} -image [image create photo -file $graph3]

FullReportText insert end "\n\nOf the the $::redcat::Count RDC vectors given, [expr $j - 1] were in violation greater than $hz Hz error."
FullReportText insert end "\n\nThe following RDC's had errors above $hz Hz:\n"
if {[array size rmRdc] <= 0} {
    FullReportText insert end "No violations above $hz Hz error"
} else {
    for {set i 1} {$i < $j} {incr i} {
        #replace Sel array
        set n $oldSel($i)
        set ::redcat::Sel($n) 1
        FullReportText insert end "Removed RDC $rmRdc($i), Error: $rmErr($i)\n"
    }
}
FullReportText insert end "\n\n"
}
#############################################################################
## Procedure:  about

proc ::redcat::about {} {
variable ::redcat::widget

set redVers [exec Redcat -v]

set introMsg "REDCAT version 10/22/10.\nDeveloped by Dr. Homayoun Valafar\nwww.cse.sc.edu/~homayoun\nEmail To: homayoun@cec.sc.edu"

::redcat::errorDialog "About Redcat" "$introMsg\n\nRedcat Engine: $redVers"
}
#############################################################################
## Procedure:  shrinkImage

proc ::redcat::shrinkImage {image percent} {
set deno      [::redcat::gcd $percent 100]
set zoom      [expr {$percent/$deno}]
set subsample [expr {100/$deno}]
set im1 [image create photo]
$im1 copy $image -zoom $zoom
set im2 [image create photo]
$im2 copy $im1 -subsample $subsample
image delete $im1
set im2
}
#############################################################################
## Procedure:  gcd

proc ::redcat::gcd {u v} {
expr {$u? [::redcat::gcd [expr $v%$u] $u]: $v}
}
#############################################################################
## Procedure:  calcDa

proc ::redcat::calcDa {Szz num} {
set answer [expr ( ( $Szz ) * ( $num ) ) / 2.0 ]
return $answer
}
#############################################################################
## Procedure:  calcR

proc ::redcat::calcR {Sxx Syy Szz} {
set answer [expr (2.0 / 3.0) * ( ( $Sxx - $Syy ) / ( $Szz ) )]
return $answer
}
#############################################################################
## Procedure:  hyper_references

proc ::redcat::hyper_references {c} {
$c bind reference <Enter> [list $c configure -cursor hand2]
$c bind reference <Leave> [list $c configure -cursor {}]
$c bind reference <ButtonPress-1> [list $c itemconfigure current -fill red]
$c bind reference <B1-Leave> "[list $c configure -cursor {}] [list $c itemconfigure current -fill blue]"
$c bind reference <ButtonRelease-1> [list ::redcat::hyper_jump $c red]
}
#############################################################################
## Procedure:  hyper_jump

proc ::redcat::hyper_jump {c highlight} {
if {[$c itemcget current -fill] ne $highlight} then {return}
# Now do the actual jump
}
#############################################################################
## Procedure:  launchBrowser

proc ::redcat::launchBrowser {url} {
  global tcl_platform

    # It *is* generally a mistake to switch on $tcl_platform(os), particularly
    # in comparison to $tcl_platform(platform).  For now, let's just regard it
    # as a stylistic variation subject to debate.
      switch $::tcl_platform(os) {
	Darwin {
	  set command [list open $url]
	}
	HP-U::redcat::X -
	Linux  -
	SunOS {
	  foreach executable {firefox mozilla netscape iexplorer opera lynx
		              w3m links epiphany galeon konqueror mosaic amaya
		              browsex elinks} {
	    set executable [auto_execok $executable]
	    if [string length $executable] {
	      # Do you want to mess with -remote?  How about other browsers?
	      set command [list $executable $url &]
	      break
	    }
	  }
	}
	{Windows 95} -
	{Windows NT} {
	  set command "[auto_execok start] {} [list $url]"
	}
      }
      if [info exists command] {
	# Replace {*}$command by eval "$command" if you want < tcl 8.5 compatibility ([RA])
        # Added the '&' to launch the browser as background process. [Duoas]
	if [catch {eval exec $command} err] {
	  tk_messageBox -icon error -message "error '$err' with '$command'"
	}
      } else {
	tk_messageBox -icon error -message "Please tell CL that ($tcl_platform(os), $tcl_platform(platform)) is not yet ready for browsing."
      }
}
#############################################################################
## Procedure:  Make_Win_Proto

proc ::redcat::Make_Win_Proto {} {
variable ::redcat::X;
variable ::redcat::Count;
variable ::redcat::Sel;
variable ::redcat::Dip;


set ba_t .top32.cpd38.03
set ba_f $ba_t.fra0
frame $ba_f

label $ba_f.lab1 -text  "Eq.#  "
label $ba_f.lab2 -text  "    "
label $ba_f.lab3 -text  "X1       "
label $ba_f.lab4 -text  "Y1       "
label $ba_f.lab5 -text  "Z1       "
label $ba_f.lab6 -text  "X2       "
label $ba_f.lab7 -text  "Y2       "
label $ba_f.lab8 -text  "Z2       "
label $ba_f.lab9 -text  "Dipol 1  "
checkbutton $ba_f.cb1 -variable ::redcat::Dip(1) -selectcolor #ff0c04
label $ba_f.lab12 -text "Diopl 2  "
checkbutton $ba_f.cb2 -variable ::redcat::Dip(2) -selectcolor #ff0c04
label $ba_f.lab13 -text "Diopl 3  "
checkbutton $ba_f.cb3 -variable ::redcat::Dip(3) -selectcolor #ff0c04
label $ba_f.lab10 -text "Error    "
label $ba_f.lab11 -text "Comments "
grid $ba_f.lab1 $ba_f.lab2 $ba_f.lab3 $ba_f.lab4 $ba_f.lab5  $ba_f.lab6   $ba_f.lab7 $ba_f.lab8 $ba_f.lab9 $ba_f.lab10 $ba_f.lab11
$ba_t window create end -window $ba_f

for {set i 1} {$i <= $::redcat::Count} {incr i} {
 set ba_f $ba_t.fra$i
 frame $ba_f
 
 label $ba_f.ln$i -text "$i) "
 if { $::redcat::X(7,$i) == 999 } {
   set ::redcat::Sel($i) 0
 } else {
   set ::redcat::Sel($i) 1
 }
 checkbutton $ba_f.rb$i -variable ::redcat::Sel($i) -selectcolor #ff0c04

 entry $ba_f.x1$i -textvariable ::redcat::X(1,$i) -width 6
 entry $ba_f.y1$i -textvariable ::redcat::X(2,$i) -width 6
 entry $ba_f.z1$i -textvariable ::redcat::X(3,$i) -width 6
 entry $ba_f.x2$i -textvariable ::redcat::X(4,$i) -width 6
 entry $ba_f.y2$i -textvariable ::redcat::X(5,$i) -width 6
 entry $ba_f.z2$i -textvariable ::redcat::X(6,$i) -width 6
 entry $ba_f.d$i -textvariable ::redcat::X(7,$i) -width 6
 entry $ba_f.d$i -textvariable ::redcat::X(11,$i) -width 6
 entry $ba_f.d$i -textvariable ::redcat::X(12,$i) -width 6
 entry $ba_f.e$i -textvariable ::redcat::X(9,$i) -width 6
 entry $ba_f.c$i -textvariable ::redcat::X(10,$i) -width 15

 grid $ba_f.ln$i $ba_f.rb$i $ba_f.x1$i $ba_f.y1$i  $ba_f.z1$i $ba_f.x2$i  $ba_f.y2$i  $ba_f.z2$i $ba_f.d$i  $ba_f.e$i $ba_f.c$i
 $ba_t window create end -window $ba_f
}
}
#############################################################################
## Procedure:  Get_Data_Proto

proc ::redcat::Get_Data_Proto {svd_input} {
variable ::redcat::X
variable ::redcat::Count

if {$svd_input == ""} {
   set svd_input [tk_getOpenFile]
}
set fin [open $svd_input r]
set ::redcat::Count 0

while {[gets $fin oline] >=0} {
 incr ::redcat::Count
 set num_subs [regsub -all "\t" $oline " " line]
 set data [split $line " "]
 set ::redcat::X(1,$::redcat::Count) [lindex $data 0]
 set ::redcat::X(2,$::redcat::Count) [lindex $data 1] 
 set ::redcat::X(3,$::redcat::Count) [lindex $data 2]
 set ::redcat::X(4,$::redcat::Count) [lindex $data 3]
 set ::redcat::X(5,$::redcat::Count) [lindex $data 4] 
 set ::redcat::X(6,$::redcat::Count) [lindex $data 5]  
 set ::redcat::X(7,$::redcat::Count) [lindex $data 6] 
 set ::redcat::X(11,$::redcat::Count) [lindex $data 7]
 set ::redcat::X(12,$::redcat::Count) [lindex $data 8]
 set ::redcat::X(8,$::redcat::Count) [lindex $data 9] 
 set ::redcat::X(9,$::redcat::Count) [lindex $data 10]
 set ::redcat::X(10,$::redcat::Count) [lrange $data 11 end]
}
close $fin

::redcat::Make_Win_Proto
}
#############################################################################
## Procedure:  printSolve

proc ::redcat::printSolve {} {
variable ::redcat::widget
variable ::redcat::solveArray
variable ::redcat::tensorOffset
variable ::redcat::selTensor
variable ::redcat::destroyTensor

::redcat::Window show .top37

#create a frame, which will hold a radiobutton, on each line of the Text box 
#with the text box being interactive and holding 
#the solve data

set baseTxt .top37.cpd32.03
set tensorArray [lrange $::redcat::solveArray 5 end-1]

Text2 insert end "[lindex $::redcat::solveArray 0]\n"
Text2 insert end "[lindex $::redcat::solveArray 1]\n"
Text2 insert end "[lindex $::redcat::solveArray 2]\n"
Text2 insert end "[lindex $::redcat::solveArray 3]\n"
Text2 insert end "[lindex $::redcat::solveArray 4]\n"

#add body
foreach s $tensorArray {
    set baseFrm $baseTxt.frm$destroyTensor
    frame $baseFrm
    radiobutton $baseFrm.rb$destroyTensor -variable ::redcat::selTensor -value $destroyTensor -command {
        set index [expr $::redcat::tensorOffset + $::redcat::selTensor + 1]
        set Select_Text [lindex $::redcat::solveArray $index]
        set Split_Selection [split $Select_Text "\t"]
        set ::redcat::Sxx [lindex $Split_Selection 1] 
        set ::redcat::Syy [lindex $Split_Selection 2]
        set ::redcat::Szz [lindex $Split_Selection 3]
        set ::redcat::a [lindex $Split_Selection 4]
        set ::redcat::b [lindex $Split_Selection 5]
        set ::redcat::c [lindex $Split_Selection 6]
        ::redcat::cleanSAndR
        
    }
    grid $baseFrm.rb$destroyTensor
    $baseTxt window create end -window $baseFrm
    Text2 insert end "$s\n"
    incr destroyTensor
    
}
}
#############################################################################
## Procedure:  Destroy_Tensor

proc ::redcat::Destroy_Tensor {} {
variable ::redcat::widget
variable ::redcat::destroyTensor
variable ::redcat::selTensor

if { $destroyTensor > 0 } {
    for { set i 0 } { $i < $destroyTensor } { incr i } {
        destroy .top37.cpd32.03.frm$i.rb$i
        destroy .top37.cpd32.03.frm$i.lbl$i
        destroy .top37.cpd32.03.frm$i
    
    }

}

set destroyTensor 0
set ::redcat::selTensor -1

::redcat::Destroy_Report Text2
}
#############################################################################
## Procedure:  printRun

proc ::redcat::printRun {} {
variable ::redcat::widget
variable ::redcat::Count
variable ::redcat::X
variable ::redcat::Sel
variable ::redcat::MCItr
variable ::redcat::rejectArray
variable ::redcat::errorArray

#check if populated
if {[llength $::redcat::rejectArray]<1||[llength $::redcat::errorArray]<1} {
    return 0
    
}

::redcat::Window show .top37

# Defining different color schemes
Text2 tag configure Green -background green
Text2 tag configure Red -background red
Text2 tag configure Orange -background orange
Text2 tag configure Yellow -background yellow
Text2 tag configure Gray -background gray

#print header
Text2 insert end "[lindex $::redcat::rejectArray 0]\n"
Text2 insert end "Equation:\tRejections:\tError Analysis:\n"

#print body
set r 1
set e 0
for { set i 1 } { $i <= $::redcat::Count } { incr i } {

	if {$::redcat::Sel($i) != 1 || [lindex $::redcat::rejectArray $r] == "..."} {
		Text2 insert end "Equation $i excluded ....\n" Gray
        incr r

	} elseif {$::redcat::X(7,$i) == "AVG"} {
		Text2 insert end "Equation $i is an average ....\n" Gray 
		 
	} else {
		regexp {^[0-9]+} [lindex $::redcat::rejectArray $r] rej
		set errString [lindex $::redcat::errorArray $e]
		regexp {^[-]*[0-9]*.*[0-9]*[e]*[-]*[0-9]*} $errString err
		if { $rej == $::redcat::MCItr && $err > $::redcat::X(9,$i) } {
			Text2 insert end "$i\t\t$rej\t\t$err\n" Red
			
		} elseif { $rej == $::redcat::MCItr } {
			Text2 insert end "$i\t\t$rej\t\t$err\n" Yellow
                        
		} elseif { $err > $::redcat::X(9,$i) } {
			Text2 insert end "$i\t\t$rej\t\t$err\n" Orange
                
		} else {
			Text2 insert end "$i\t\t$rej\t\t$err\n" Green
			
		}
		incr r
		incr e
		
	}
	
}
}
#############################################################################
## Procedure:  plotError

proc ::redcat::plotError {command} {
variable ::redcat::widget
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::X
variable ::redcat::errorArray

if {[::redcat::Run_SVD 2]==0} {
    return 

}

set PlotErr [open "|gnuplot -persist" w+]
flush $PlotErr
fconfigure $PlotErr -buffering none
if { $command != "" } {
    puts $PlotErr $command
    
}
puts $PlotErr "set mouse"
puts $PlotErr "set key off"
#puts $PlotErr "set xlabel \"Equation\""
#puts $PlotErr "set ylabel \"Error\""
puts $PlotErr "set xrange \[0:[expr $::redcat::Count + 1]\]"
puts $PlotErr "plot '-' with impulses"

set e 0
puts $PlotErr 0
for { set i 1 } { $i <= $::redcat::Count } { incr i } {
    if {$::redcat::Sel($i) == 1} {
        set errString [lindex $::redcat::errorArray $e]
        regsub -all {[" "]} $errString "" err
        if {$::redcat::X(7,$i)=="AVG"} {
            puts $PlotErr "0"
        } elseif {$::redcat::X(7,$i)==999} {
            puts $PlotErr "0"
        
        } else {
            puts $PlotErr "$err"
            incr e
            
        }
        
    } else {
        puts $PlotErr "0"

    }
    
}
puts $PlotErr "e"
#set output [read $PlotErr]
catch {close $PlotErr} resutls
return resutls
}
#############################################################################
## Procedure:  plot2D

proc ::redcat::plot2D {plotAll plotFile} {
variable ::redcat::widget
variable ::redcat::plotArray

set plotList [split [lindex $::redcat::plotArray 0] " " ]

set index 0
set Sxx($index) "[lindex $plotList 1] [lindex $plotList 2]"
set Syy($index)  "[lindex $plotList 3] [lindex $plotList 4]"
set Szz($index) "[lindex $plotList 5] [lindex $plotList 6]"
incr index

set Sxx($index) "[lindex $plotList 8] [lindex $plotList 9]"
set Syy($index)  "[lindex $plotList 10] [lindex $plotList 11]"              
set Szz($index) "[lindex $plotList 12] [lindex $plotList 13]"
incr index
        
if { $plotAll == 1 } {
    for { set i 1 } { $i < [llength $::redcat::plotArray] } { incr i } {
        set plotList [split [lindex $::redcat::plotArray $i] " " ]
        
        set Sxx($index) "[lindex $plotList 1] [lindex $plotList 2]"
        set Syy($index) "[lindex $plotList 3] [lindex $plotList 4]"
        set Szz($index) "[lindex $plotList 5] [lindex $plotList 6]"
        incr index
        
        set Sxx($index) "[lindex $plotList 8] [lindex $plotList 9]"
        set Syy($index) "[lindex $plotList 10] [lindex $plotList 11]"
        set Szz($index) "[lindex $plotList 12] [lindex $plotList 13]"
        incr index
        
    }

}
for { set i 0 } { $i < $index } { incr i } {
    puts $plotFile "$Sxx($i)"
        
}
puts $plotFile "e"
for { set i 0 } { $i < $index } { incr i } {
    puts $plotFile "$Syy($i)"
        
}
puts $plotFile "e"
for { set i 0 } { $i < $index } { incr i } {
    puts $plotFile "$Szz($i)"
        
}
puts $plotFile "e"
}
#############################################################################
## Procedure:  plot3D

proc ::redcat::plot3D {plotAll plotFile} {
variable ::redcat::widget
variable ::redcat::plotArray
variable ::redcat::solveArray

set tensorArray [lrange $::redcat::solveArray 5 end-1]

set plotList [split [lindex $::redcat::plotArray 0] " " ]
set tensorList [split [lindex $tensorArray 0] "\t"]

set plotIndex 0
set solveIndex 0

set Sxx($plotIndex) "[lindex $plotList 1] [lindex $plotList 2] [lindex $tensorList 1]"
set Syy($plotIndex)  "[lindex $plotList 3] [lindex $plotList 4] [lindex $tensorList 2]"
set Szz($plotIndex) "[lindex $plotList 5] [lindex $plotList 6] [lindex $tensorList 3]"
incr plotIndex

set Sxx($plotIndex) "[lindex $plotList 8] [lindex $plotList 9] [lindex $tensorList 1]"
set Syy($plotIndex)  "[lindex $plotList 10] [lindex $plotList 11] [lindex $tensorList 2]"              
set Szz($plotIndex) "[lindex $plotList 12] [lindex $plotList 13] [lindex $tensorList 3]"
incr plotIndex
incr solveIndex

if { $plotAll == 1 } {
    for { set i 1 } { $i < [llength $::redcat::plotArray] } { incr i } {
        set plotList [split [lindex $::redcat::plotArray $i] " " ]
        set tensorList [split [lindex $tensorArray $solveIndex] "\t"]
        
        set Sxx($plotIndex) "[lindex $plotList 1] [lindex $plotList 2] [lindex $tensorList 1]"
        set Syy($plotIndex) "[lindex $plotList 3] [lindex $plotList 4] [lindex $tensorList 2]"
        set Szz($plotIndex) "[lindex $plotList 5] [lindex $plotList 6] [lindex $tensorList 3]"
        incr plotIndex
        
        set Sxx($plotIndex) "[lindex $plotList 8] [lindex $plotList 9] [lindex $tensorList 1]"
        set Syy($plotIndex) "[lindex $plotList 10] [lindex $plotList 11] [lindex $tensorList 2]"
        set Szz($plotIndex) "[lindex $plotList 12] [lindex $plotList 13] [lindex $tensorList 3]"
        incr plotIndex
        incr solveIndex

    }

}
for { set i 0 } { $i < $plotIndex } { incr i } {
    puts $plotFile "$Sxx($i)"
        
}
puts $plotFile "e"
for { set i 0 } { $i < $plotIndex } { incr i } {
    puts $plotFile "$Syy($i)"
        
}
puts $plotFile "e"
for { set i 0 } { $i < $plotIndex } { incr i } {
    puts $plotFile "$Szz($i)"
        
}
puts $plotFile "e"
}
#############################################################################
## Procedure:  genReport1

proc ::redcat::genReport1 {keepSel command} {
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::2DMapDatGlobal
variable ::redcat::3DMapDatGlobal
variable ::redcat::rejectArray
variable ::redcat::errorArray
variable ::redcat::solveArray
variable ::redcat::destroyTensor
variable ::redcat::2DMapDatGlobal
variable ::redcat::3DMapDatGlobal
variable ::redcat::reportCount
variable ::redcat::Sxx
variable ::redcat::Syy
variable ::redcat::Szz
variable ::redcat::a
variable ::redcat::b
variable ::redcat::c
variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s

set engineError ""

if { $command == "" } {
    set command FullReportText
    ::redcat::Window show .fullReport

}

if { $::redcat::2DMapDatGlobal == "" } {
    set 2dmap "map.dat"
    
} else {
    set 2dmap $::redcat::2DMapDatGlobal
    
}

if { $::redcat::3DMapDatGlobal == "" } {
    set 3dmap "map3D.dat"
    
} else {
    set 3dmap $::redcat::3DMapDatGlobal
    
}

#set graph out for 2d and 3d
set graph1 "graph1.pbm"
set graph2 "graph2.pbm"
set graph3 "graph3.pbm"
set graph4 "graph4.pbm"

set errFlag 1
set reSelIndex 0
set errMssg ""
while { $errFlag == 1 } {
    #saveDataHz $hz 0
    if {[::redcat::Save_Data "Redcat.in" 3]==0} {
         ::redcat::dataLoadedError
         return
    
    }
    
    #before we run, make sure that we dont have an empty Selection
    set doRun [::redcat::checkRun]
    if {$doRun==0} {
        ::redcat::errorDialog "Empty Matrix" "No equations fall below $RemoveHzAbove Hz error."
        set errFlag 0
        continue
        
    }
    set mssg [exec Redcat -q -i Redcat.in -b]

    #create dictionary for translating index of Redcat.in to loaded redcat file
    set fakeIndex 1
    for { set i 1 } { $i <= $::redcat::Count } { incr i } {
        if { $::redcat::Sel($i) == 1 } {
            set dic($fakeIndex) $i
            incr fakeIndex
        
        }
    
    }
    
    set mssgList [split $mssg ":"]

    #unselect equations beyond error
    if { [llength $mssgList] > 5 } {
        for { set i 5 } { $i < [expr [llength $mssgList] - 1] } { set i [expr $i + 2] } {
            regexp {[0-9]+} [lindex $mssgList $i] cIndex
            set tIndex $dic($cIndex)
            set ::redcat::Sel($tIndex) 0
            set reSel($reSelIndex) $tIndex
            set tmpErr [lindex $mssgList [expr $i + 1]]
            regsub {\nError} $tmpErr "" tmpErr
            append errMssg "Equation $tIndex omitted: Error $tmpErr\n"
            incr reSelIndex

        }

    } else {
            set errFlag 0
            
    }

}

if {$doRun!=0} {
#clear out arrays to be repopulated
set ::redcat::solveArray ""
set ::redcat::errorArray ""
set ::redcat::plotArray ""
set ::redcat::rejectArray ""


#Run_SVD with Sel($bigError) off
::redcat::Run_SVD 3

#create images for SFPlot, 3DPlot, and ErrorPlot
set plotGraph1 [open "|gnuplot" w+]
flush $plotGraph1
fconfigure $plotGraph1 -buffering none
puts $plotGraph1 "set mouse"
puts $plotGraph1 "set term pbm color size 300,300;set output '$graph1'"
puts $plotGraph1 "plot '$2dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
::redcat::plot2D 1 $plotGraph1
#set gnu1 [read $plotGraph1]
catch { close $plotGraph1 } result1

set plotGraph2 [open "|gnuplot" w+]
flush $plotGraph2
fconfigure $plotGraph2 -buffering none
puts $plotGraph2 "set mouse"
puts $plotGraph2 "set term pbm color size 300,300; set view 90, 0;set output '$graph2'"
puts $plotGraph2 "splot '$3dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
::redcat::plot3D 1 $plotGraph2
#set gnu2 [read $plotGraph2]
catch { close $plotGraph2 } result2

::redcat::plotError "set term gif size 640,325; set output '$graph3'"

#save Sxx, Syy, and calc Szz
set bestline [split [lindex $::redcat::solveArray 5] "\t"]
regexp {[-]*[0-9]+.*[0-9]*[e]*[-]*[0-9]*} [lindex $bestline 1] Sxx
set Sxx [::redcat::parseSci $Sxx]
regexp {[-]*[0-9]+.*[0-9]*[e]*[-]*[0-9]*} [lindex $bestline 2] Syy
set Syy [::redcat::parseSci $Syy]
regexp {[-]*[0-9]+.*[0-9]*[e]*[-]*[0-9]*} [lindex $bestline 3] Szz
set Szz [::redcat::parseSci $Szz]

#clean up end of [lindex $mssgList 1]
regsub {\ndecompose} [lindex $mssgList 1] "" bestS

set Select_Text [lindex $solveArray 5]
set Split_Selection [split $Select_Text "\t"]
set ::redcat::Sxx [lindex $Split_Selection 1]
set ::redcat::Syy [lindex $Split_Selection 2]
set ::redcat::Szz [lindex $Split_Selection 3]
set ::redcat::a [lindex $Split_Selection 4]
set ::redcat::b [lindex $Split_Selection 5]
set ::redcat::c [lindex $Split_Selection 6]
::redcat::cleanSAndR

::redcat::Get_Sub_RDC $Sxx $Syy $Szz $a $b $c 0 0 1 $graph4

#Print info
$command insert end "Best S: $bestS"
$command insert end "[lindex $solveArray 0]\n"
$command insert end "[lindex $solveArray 1]\n"
$command insert end "[lindex $solveArray 2]\n"
$command insert end "[lindex $solveArray 3]\n"
$command insert end "[lindex $solveArray 4]\n"
$command insert end "[lindex $solveArray 5]\n"
$command insert end "\nC-N Da: [calcDa $Szz 6125.0]"
$command insert end "\nN-H Da: [calcDa $Szz 24350.0]"
$command insert end "\nC-H Da: [calcDa $Szz -60400.0]"
$command insert end "\nH-H Da: [calcDa $Szz -240200.0]"
$command insert end "\n\nR: [calcR $Sxx $Syy $Szz]"
$command insert end "\n\n"
$command insert end "\n$::redcat::message_r"
$command insert end "\n$::redcat::message_q"
$command insert end "\n$::redcat::message_s"
$command insert end "\n\n"

set img1 [image create photo -file $graph1]
$command image create {end linestart} -image $img1
set img2 [image create photo -file $graph2]
$command image create {end linestart} -image $img2
$command insert end "\n\n"
set img3 [image create photo -file $graph3]
$command image create {end linestart} -image $img3
$command insert end "\n\n"
set img4 [image create photo -file $graph4]
$command image create {end linestart} -image $img4

exec rm $graph1
exec rm $graph2
exec rm $graph3
exec rm $graph4

if { $errMssg == "" } {
    $command insert end "\n\nNo equations omitted\n"

} else {
    $command insert end "\n\n$errMssg"
    
}

}
if { $keepSel != 1 } {
    #clear out arrays to be repopulated
    set ::redcat::solveArray ""
    set ::redcat::errorArray ""
    set ::redcat::plotArray ""
    set ::redcat::rejectArray ""
    set ::redcat::Sxx ""
    set ::redcat::Syy ""
    set ::redcat::Szz ""
    set ::redcat::a ""
    set ::redcat::b ""
    set ::redcat::c ""
    set ::redcat::message_r "RMSD = "
    set ::redcat::message_q "Q-Factor = "
    set ::redcat::message_s "S-Factor = "
    #reset Sel($bigError) b/c no longer needed
    for { set i 0 } { $i < [array size reSel] } { incr i } {
        set index $reSel($i)
        set ::redcat::Sel($index) 1
    
    }
    #rerun with Sel($bigError) back on
    ::redcat::Run_SVD 2

}
}
#############################################################################
## Procedure:  saveDataHz

proc ::redcat::saveDataHz {hz add} {
variable ::redcat::Count
variable ::redcat::Sel
variable ::redcat::X
set fout [open "Redcat.in" w]
if { [array exists ::redcat::X] } {
    if { $add == 1} {
        for {set i 1} {$i <= $::redcat::Count} {incr i} {
           if {$::redcat::Sel($i) == 1} { 
              if {$::redcat::X(7,$i) == "AVG"} {
                 puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) AVG $::redcat::X(8,$i) [expr $hz + $::redcat::X(9,$i)] $::redcat::X(10,$i)"
                 
              } else {
                 puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) [expr $hz + $::redcat::X(9,$i)] $::redcat::X(10,$i)"
                 
              }
              
           }
           
        }
        
      } else {
            for {set i 1} {$i <= $::redcat::Count} {incr i} {
               if {$::redcat::Sel($i) == 1} { 
                   if {$::redcat::X(7,$i) == "AVG"} {
                     puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) AVG $::redcat::X(8,$i) $hz $::redcat::X(10,$i)"
                 
                   } else {
                     puts $fout "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i) $::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i) $::redcat::X(7,$i) $::redcat::X(8,$i) $hz $::redcat::X(10,$i)"
                 
                   }
               
               }
             
            }
            
      }
      close $fout
      
} else {
    ::redcat::dataLoadedError
    
}
}
#############################################################################
## Procedure:  parseSci

proc ::redcat::parseSci {sciNote} {
set sciNoteList [split $sciNote "e"]
if { [llength $sciNoteList] > 1 } {
    set power [expr pow(10,[lindex $sciNoteList 1])]
    return [expr [lindex $sciNoteList 0] * $power]

} else { 
    return $sciNote 

}
}
#############################################################################
## Procedure:  ::redcat::dataLoadedError

proc ::redcat::dataLoadedError {} {
::redcat::errorDialog "Data Error" "Please load a redcat file to procede."
}
#############################################################################
## Procedure:  cleanSAndR

proc ::redcat::cleanSAndR {} {
variable ::redcat::Sxx
variable ::redcat::Syy
variable ::redcat::Szz
variable ::redcat::a
variable ::redcat::b
variable ::redcat::c

regexp {[\-]*[0-9]+.[0-9]+([e][-]?[0-9]*)?} $::redcat::Sxx Sxx
regexp {[\-]*[0-9]+.[0-9]+([e][-]?[0-9]*)?} $::redcat::Syy Syy
regexp {[\-]*[0-9]+.[0-9]+([e][-]?[0-9]*)?} $::redcat::Szz Szz
regexp {[\-]*[0-9]+.[0-9]+} $::redcat::a a
regexp {[\-]*[0-9]+.[0-9]+} $::redcat::b b
regexp {[\-]*[0-9]+.[0-9]+} $::redcat::c c

set ::redcat::Sxx [parseSci $::redcat::Sxx]
set ::redcat::Syy [parseSci $::redcat::Syy]
set ::redcat::Szz [parseSci $::redcat::Szz]
set ::redcat::a [parseSci $::redcat::a]
set ::redcat::b [parseSci $::redcat::b]
set ::redcat::c [parseSci $::redcat::c]
}

#############################################################################
## Procedure:  hideAll

proc ::redcat::hideAll {} {
	
init {} {}
}
#############################################################################
## Procedure:  visualizeRDC

proc visualizeRDC {mol} {
	variable ::redcat::Count
	variable ::redcat::X
	variable ::redcat::Sel
	variable ::redcat::errorArray
	    
    set r 1
	set e 0
	for { set i 1 } { $i <= $::redcat::Count } { incr i } {
		if {$::redcat::Sel($i) != 1 || [lindex $::redcat::rejectArray $i] == "..."} {
#			Text2 insert end "Equation $i excluded ....\n" Gray
               incr r
	
		} elseif {$::redcat::X(7,$i) == "AVG"} {
#			Text2 insert end "Equation $i is an average ....\n" Gray 
			     
		} else {
			regexp {[0-9]+} [lindex $::redcat::rejectArray $r] rej
			set errString [lindex $::redcat::errorArray $e]
			regexp {^[-]*[0-9]*.*[0-9]*[e]*[-]*[0-9]*} $errString err
			if { $rej == $::redcat::MCItr && $err > $::redcat::X(9,$i) } {
				connect $mol "red3" "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i)" "$::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i)"
				
			} elseif { $rej == $::redcat::MCItr } {
				connect $mol "yellow" "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i)" "$::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i)"
     	                   
			} elseif { $err > $::redcat::X(9,$i) } {
				connect $mol "orange" "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i)" "$::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i)"
     	           
			} else {
				connect $mol "green" "$::redcat::X(1,$i) $::redcat::X(2,$i) $::redcat::X(3,$i)" "$::redcat::X(4,$i) $::redcat::X(5,$i) $::redcat::X(6,$i)"
				
			}
			incr r
			incr e
			
		}
		
	}

}

#############################################################################
## Procedure:  selectAtom

proc ::redcat::selectAtom {mol name residue} {
	set sel [atomselect $mol "name $name and resid $residue"]
	set pos [measure avpos $sel]
	return $pos
	
}
#############################################################################
## Procedure:  getNumResidues

proc ::redcat::getNumResidues {} {
	set sel [atomselect top "name CA"]
	set numRes [$sel num]
	return $numRes
	
}
#############################################################################
## Procedure:  vmd_draw_cylinder

proc ::redcat::draw_cylinder {mol color start end} {
	addClearMat
	graphics $mol color $color
	graphics $mol material Clear
	graphics $mol cylinder $start $end radius 0.27
	
}
#############################################################################
## Procedure:  draw_sphere

proc ::redcat::draw_sphere {mol color start} {
	addClearMat
	graphics $mol color $color
	graphics $mol material Clear
	if {$color == "red3"} {
		set size 0.27
		
	} else {
		set size 0.269
		
	}
	graphics $mol sphere $start radius $size
	
}
#############################################################################
## Procedure:  connect

proc ::redcat::connect {mol color coord1 coord2} {
	::redcat::draw_cylinder $mol $color $coord1 $coord2
	::redcat::draw_sphere $mol $color $coord1
	::redcat::draw_sphere $mol $color $coord2

}
#############################################################################
## Procedure:  clearGraphics

proc ::redcat::clearGraphics {mol} {
	graphics $mol delete all
	
}
#############################################################################
## Procedure:  addClearMat

proc ::redcat::addClearMat {} {
	changeRed3
	set matString [material list]
	set matList [split $matString " "]
	foreach i $matList {
		if {$i=="Clear"} {
			return
			
		}
	
	}
	material add Clear
	material change ambient Clear 0.00
	material change diffuse Clear 0.60
	material change specular Clear 0.10
	material change shininess Clear 0.10
	material change opacity Clear 0.26
	material change outline Clear 1.00
	material change outlinewidth Clear 1.00
	
}
#############################################################################
## Procedure:  changeRed3

proc ::redcat::changeRed3 {} {
	color change rgb red3 0.75 0.1 0.1
	
}
#############################################################################
## Procedure:  resetRed3

proc ::redcat::resetRed3 {} {
	color change rgb red3 0.81 0 0
	
}
#############################################################################
## Procedure:  resetRedcat

proc ::redcat::resetRedcat {mol} {
	clearGraphics $mol
	resetRed3
	material delete Clear
	
}
#############################################################################
## Procedure:  clearRedcat

proc ::redcat::clearRedcat {mol} {
	clearGraphics $mol
	
}
#############################################################################
## Procedure:  labelRdc

proc ::redcat::labelRdc {atom1 atom2 text} {
	set C1 [split $atom1 " "]
	set C2 [split $atom2 " "]
	set midpoint "[expr [expr [lindex $C1 0] + [lindex $C2 0]] / 2]\
	[expr [expr [expr [lindex $C1 1] + [lindex $C2 1]] / 2] + 0.25]\
	[expr [expr [expr [lindex $C1 2] + [lindex $C2 2]] / 2] + 0.25]"
	
	draw text $midpoint "$text" size 0.5
	
}
#############################################################################
## Procedure:  launchEnsemble

proc ::redcat::launchEnsemble {} {
        set baseTop [toplevel .topEnsemble]
        wm title $baseTop "Ensemble Solutions"
	wm geometry $baseTop 400x200
	
	set headFrame [frame $baseTop.headFrame]
	set headFrameElem [list]
	lappend headFrameElem [label $headFrame.lbl0 -text "Ensemble Structure Analysis"]
	foreach i $headFrameElem {
		pack $i -side left
	
	}
	
	set bodyFrame [frame $baseTop.bodyFrame]
	set bodyFrameRow1 [frame $bodyFrame.bodyFrameRow1]
      	set bodyFrameRow2 [frame $bodyFrame.bodyFrameRow2]
        set bodyFrameElem [list]
	lappend bodyFrameElem [label $bodyFrameRow1.lbl0 -text "Please Load the Ensemble file"]
	lappend bodyFrameElem [entry $bodyFrameRow1.entr0 -textvariable ::redcat::ensembleFile]
	lappend bodyFrameElem [button $bodyFrameRow1.but0 -text "Load" -command {set ::redcat::ensembleFile [tk_getOpenFile]}]
        lappend bodyFrameElem [label $bodyFrameRow2.lbl1 -text "Remove Error Above:"]
        lappend bodyFrameElem [entry $bodyFrameRow2.entr1 -textvariable ::redcat::ensembleError]
        set space [label $bodyFrame.lblSpace -text ""]
        
        foreach i $bodyFrameElem {
            pack $i -side left
        
        }
        pack $bodyFrameRow1 $bodyFrameRow2 -side top
	
	set footFrame [frame $baseTop.footFrame]
	set footFrameElem [list]
	lappend footFrameElem [button $footFrame.but0 -text "Run" -command {::redcat::Window show .top37
                                                                        ::redcat::runEnsemble $::redcat::ensembleFile $::redcat::ensembleError}]
	lappend footFrameElem [button $footFrame.but1 -text "Reset" -command {set ::redcat::ensembleFile ""
                                                                          set ::redcat::ensembleError ""}]
	lappend footFrameElem [button $footFrame.but2 -text "Done" -command {destroy .topEnsemble}]
	foreach i $footFrameElem {
		pack $i -side left
	
	}
		
	pack $headFrame $bodyFrame $footFrame -expand 1 -fill y -side top
}
#############################################################################
## Procedure:  runEnsemble

proc ::redcat::runEnsemble {ensembleFile ensembleError} {
    variable ::redcat::X
	variable ::redcat::Sel
	variable ::redcat::Count
	variable ::redcat::rCatFile
    variable ::redcat::RemoveHzAbove
    variable ::redcat::loaded
	
	if {$ensembleError == ""} {
            tk_messageBox -type ok -title "Data Error" -message "Please enter a value for \"Remove Error Above:\"."
            return
        
     }
	set prevRCat $::redcat::rCatFile
	set fin [open $ensembleFile r]
	if {$prevRCat != ""} {
		for {set i 1} {$i<=$::redcat::Count} {incr i} {
			set prevSel($i) $::redcat::Sel($i)
		
		}
		
	}
	while {[gets $fin line] >=0} {
        set ::redcat::loaded -1
        ::redcat::Get_Data $line
		::redcat::Run_SVD 3
        set tmpHz $::redcat::RemoveHzAbove
        set ::redcat::RemoveHzAbove $ensembleError
        Text2 insert end "$line\n"
		::redcat::genReport1 0 Text2
        set ::redcat::RemoveHzAbove $tmpHz
        array unset ::redcat::X
	
	}
    set loaded -1
    if {$prevRCat != ""} {
		Get_Data $prevRCat
        set ::redcat::loaded 1
		for {set i 1} {$i<=$::redcat::Count} {incr i} {
			set ::redcat::Sel($i) $prevSel($i)
				
        }
        Run_SVD 2

	} else {
		init {} {}

	}
}
#############################################################################
## Procedure:  redcatRotate

proc ::redcat::redcatRotate {mol type a b c theta} {
	variable ::redcat::X
	variable ::redcat::Count
	variable ::redcat::Sel
	
	set pi 3.1415926535897932
	if {$type == 1} {
	    set a [expr {$a * $pi / 180}]
	    set b [expr {$b * $pi / 180}]
	    set c [expr {$c * $pi / 180}]
	    set d(0) [expr {cos($c) * cos($b) * cos($a) - sin($c) * sin($a)}]
	    set d(1) [expr {cos($c) * cos($b) * sin($a) + sin($c) * cos($a)}]
	    set d(2) [expr {-cos($c) * sin($b)}]
	    set d(3) [expr {-sin($c) * cos($b) * cos($a) - cos($c) * sin($a)}]
	    set d(4) [expr {-sin($c) * cos($b) * sin($a) + cos($c) * cos($a)}]
	    set d(5) [expr {sin($c) * sin($b)}]
	    set d(6) [expr {cos($a) * sin($b)}]
	    set d(7) [expr {sin($a) * sin($b)}]
	    set d(8) [expr {cos($b)}]
	    
	} else {  
	    set norm [expr sqrt([expr {$a * $a + $b * $b + $c * $c}])]
	    set a [expr $a / $norm]
	    set b [expr $b / $norm]
	    set c [expr $c / $norm]
	    set theta [expr $theta * $pi / 180]
	    
	    set ct [expr cos($theta)]
	    set st [expr sin($theta)]
	    set d(0) [expr {$a * $a + $ct * ($b * $b + $c * $c)}]
	    set d(1) [expr {$a * $b * (1.0 - $ct) - $c * $st}]
	    set d(2) [expr {$a * $c * (1.0 - $ct) + $b * $st}]
	    set d(3) [expr {$a * $b * (1.0 - $ct) + $c * $st}]
	    set d(4) [expr {$b * $b + $ct * ($a * $a + $c * $c)}]
	    set d(5) [expr {$c * $b * (1.0 - $ct) - $a * $st}]
	    set d(6) [expr {$a * $c * (1.0 - $ct) - $b * $st}]
	    set d(7) [expr {$c * $b * (1.0 - $ct) + $a * $st}]
	    set d(8) [expr {$c * $c + $ct * ($b * $b + $a * $a)}]
	    
	} 
	
	for {set i 1} {$i <= $Count} {incr i} {
	  if {$Sel($i) == 1} {
	    set newx1 [expr {$X(1,$i) * $d(0) + $X(2,$i) * $d(1) + $X(3,$i) * $d(2)}]
	    set newy1 [expr {$X(1,$i) * $d(3) + $X(2,$i) * $d(4) + $X(3,$i) * $d(5)}]
	    set newz1 [expr {$X(1,$i) * $d(6) + $X(2,$i) * $d(7) + $X(3,$i) * $d(8)}]
	    
	    set newx2 [expr {$X(4,$i) * $d(0) + $X(5,$i) * $d(1) + $X(6,$i) * $d(2)}]
	    set newy2 [expr {$X(4,$i) * $d(3) + $X(5,$i) * $d(4) + $X(6,$i) * $d(5)}]
	    set newz2 [expr {$X(4,$i) * $d(6) + $X(5,$i) * $d(7) + $X(6,$i) * $d(8)}]
	    
	    set X(1,$i) $newx1
	    set X(2,$i) $newy1
	    set X(3,$i) $newz1
	    set X(4,$i) $newx2
	    set X(5,$i) $newy2
	    set X(6,$i) $newz2
	    
	  }
	  
	}
	
	for {set i 0} {$i<9} {set i [expr $i + 3]} {
		lappend M [list $d($i) $d([expr $i+1]) $d([expr $i+2])]
		
	}
	set M [trans_from_rotate $M]
	set allAtoms [atomselect $mol "all"]
	$allAtoms move $M
	
	.top32.fra39.but45 invoke
	
}

#############################################################################
## Procedure:  rdcFromRedcat

proc ::redcat::rdcFromRedcat {} {
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel

set fin [open [tk_getOpenFile] r]
set i 0

while {[gets $fin line] >= 0} {
    incr i
    if {$i <= $::redcat::Count} {
        regsub -all "\t" $line " " line
        set data [split $line " "]

        if {[llength $data] >= 9} {
            set ::redcat::X(7,$i) [lindex $data 6]
            set ::redcat::X(9,$i) [lindex $data 8]
        
        } else {
            set ::redcat::X(7,$i) "999"
        
        }
        if {$::redcat::X(7,$i) == 999} {
                set ::redcat::Sel($i) 0
                
        } else {
                if { !($::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999)} {
                    set ::redcat::Sel($i) 1
                    
                } else {
                    set ::redcat::Sel($i) 0
                
                }
                
        }
          
    }
    
}
if {$i < $::redcat::Count} {
    for {set k $i} {$k < $::redcat::Count} {incr k} {
        set ::redcat::X(7,$k) "999"
        set ::redcat::Sel($k) 0
    
    }

}

close $fin
}
#############################################################################
## Procedure:  rdcFromRedcraft

proc ::redcat::rdcFromRedcraft {} {
variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel

set fname [tk_getOpenFile]
if {$fname == ""} {
    ::redcat::errorDialog "Data Error" "No file selected"
    return

}

set fin [open $fname r]
set i 0
set j 0

while {[gets $fin line] >= 0} {
    if {$j > 0} {
        incr i
        if {$i < $::redcat::Count} {
            set data [split $line " "]
            set ::redcat::X(7,$i) [lindex $data 0]
            if {$::redcat::X(7,$i) == 999} {
                set ::redcat::Sel($i) 0
                
            } else {
                if { $::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999} {
                    set ::redcat::Sel($i) 0
                    
                } else {
                    set ::redcat::Sel($i) 1
                
                }
                
            }

        } else {
            break
        
        }
    
    }
    set j [expr ($j + 1) % 7]
    
}

if {$i < $::redcat::Count} {
    for {set k $i} {$k < $::redcat::Count} {incr k} {
        set ::redcat::X(7,$k) "999"
        set ::redcat::Sel($k) 0
    
    }

}
close $fin
}
#############################################################################
## Procedure:  save2DPlot

proc ::redcat::save2DPlot {} {
variable ::redcat::plotArray
variable ::redcat::solveArray

set file [tk_getSaveFile]
if { $file == "" } {
    ::redcat::errorDialog "Save Name Error" "Please enter a valid name to save the plot."
    
} else {
    set fout [open $file w]
    for {set i 0} {$i < [llength $::redcat::plotArray] - 1} {incr i} {
        set plotList [split [lindex $::redcat::plotArray $i] " " ]
        puts $fout "[lindex $plotList 1] [lindex $plotList 2] [lindex $plotList 3] [lindex $plotList 4] [lindex $plotList 5] [lindex $plotList 6]"
        
        puts $fout "[lindex $plotList 8] [lindex $plotList 9] [lindex $plotList 10] [lindex $plotList 11] [lindex $plotList 12] [lindex $plotList 13]"
                
    }
    close $fout

}
}
#############################################################################
## Procedure:  save3DPlot

proc ::redcat::save3DPlot {} {
variable ::redcat::plotArray
variable ::redcat::solveArray

set file [tk_getSaveFile]
if { $file == "" } {
    ::redcat::errorDialog "Save Name Error" "Please enter a valid name to save the plot."
    
} else {
    set tensorArray [lrange $::redcat::solveArray 5 end-1]
    set fout [open $file w]
    for {set i 0} {$i < [llength $::redcat::plotArray] - 1} {incr i} {
        set plotList [split [lindex $::redcat::plotArray $i] " " ]
        set tensorList [split [lindex $tensorArray $i] "\t"]
        puts $fout "[lindex $plotList 1] [lindex $plotList 2] [lindex $tensorList 1] [lindex $plotList 3] [lindex $plotList 4] [lindex $tensorList 2] [lindex $plotList 5] [lindex $plotList 6] [lindex $tensorList 3]"
        
        puts $fout "[lindex $plotList 8] [lindex $plotList 9] [lindex $tensorList 1] [lindex $plotList 10] [lindex $plotList 11] [lindex $tensorList 2] [lindex $plotList 12] [lindex $plotList 13] [lindex $tensorList 3]"
                
    }
    close $fout

}
}
#############################################################################
## Procedure:  launchXplorGUI

proc ::redcat::launchXplorGUI {} {
variable ::redcat::pathRcat
exec java -jar "$::redcat::pathRcat/XplorGUI/XplorGUI.jar" &
}
#############################################################################
## Procedure:  closeRedcat

proc ::redcat::closeRedcat {} {
variable ::redcat::Count;


Destroy_Tensor
.top37.cpd32.03 delete 1.0 end

for {set i 1} {$i<=11} {incr i} {
    destroy .top32.cpd38.03.fra0.lab$i

}

destroy .top32.cpd38.03.fra0
for {set i 1} {$i <= $::redcat::Count} {incr i} {
 destroy .top32.cpd38.03.fra$i.ln$i
 destroy .top32.cpd38.03.fra$i.rb$i
 destroy .top32.cpd38.03.fra$i.x1$i
 destroy .top32.cpd38.03.fra$i.y1$i
 destroy .top32.cpd38.03.fra$i.z1$i
 destroy .top32.cpd38.03.fra$i.x2$i
 destroy .top32.cpd38.03.fra$i.y2$i
 destroy .top32.cpd38.03.fra$i.z2$i
 destroy .top32.cpd38.03.fra$i.d$i
 destroy .top32.cpd38.03.fra$i.e$i
 destroy .top32.cpd38.03.fra$i.c$i
 destroy .top32.cpd38.03.fra$i
}

.top32.cpd38.03 delete 0.0 end

init {} {}
}
#############################################################################
## Procedure:  saveState

proc ::redcat::saveState {fname} {
variable ::redcat::X
variable ::redcat::Sel
variable ::redcat::Count
variable ::redcat::outfname
variable ::redcat::MCItr
variable ::redcat::range
variable ::redcat::rejectArray
variable ::redcat::errorArray
variable ::redcat::solveArray
variable ::redcat::destroyTensor
variable ::redcat::tensorOffset
variable ::redcat::selTensor
variable ::redcat::plotArray
variable ::redcat::plot2DFile
variable ::redcat::plot3DFile
variable ::redcat::plot_type
variable ::redcat::ErrorPad
variable ::redcat::ErrorGlobal
variable ::redcat::2DMapDatGlobal
variable ::redcat::3DMapDatGlobal
variable ::redcat::Sxx
variable ::redcat::Syy
variable ::redcat::Szz
variable ::redcat::a
variable ::redcat::b
variable ::redcat::c
variable ::redcat::error_source
variable ::redcat::error
variable ::redcat::Sub_RDC
variable ::redcat::corr_plot
variable ::redcat::Cal_RDC
variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s
variable ::redcat::RemoveHzAbove
variable ::redcat::keepExclusion
variable ::redcat::rCatFile
variable ::redcat::loaded
variable ::redcat::pathRcat
variable ::redcat::tcl_precision
variable ::redcat::iter1
variable ::redcat::iter2
variable ::redcat::fin
variable ::redcat::fout
variable ::redcat::fstate
variable ::redcat::numstate
variable ::redcat::rotationType
variable ::redcat::x
variable ::redcat::y
variable ::redcat::z
variable ::redcat::theta
variable ::redcat::pdbfile
variable ::redcat::redcatfile
variable ::redcat::startres
variable ::redcat::stopres
variable ::redcat::atom
variable ::redcat::reportCount
variable ::redcat::molNum

if {[array exists ::redcat::X]} {

if {$fname == ""} {
    set fname [tk_getSaveFile]
    if {$fname == ""} {
        return
    
    }

}

set pout [open $fname w]

puts $pout "SAVE"

#main window
foreach j [array get ::redcat::X] {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

foreach j [array get ::redcat::Sel] {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

puts $pout "$::redcat::Count"

puts $pout "$::redcat::outfname"

puts $pout "$::redcat::MCItr"

puts $pout "$::redcat::range"

#reject
foreach j $::redcat::rejectArray {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

#error
foreach j $::redcat::errorArray {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

#solve
foreach j $::redcat::solveArray {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

puts $pout "$::redcat::destroyTensor"

puts $pout "$::redcat::tensorOffset"

puts $pout "$::redcat::selTensor"

puts $pout "$::redcat::Selection"

puts $pout "$::redcat::keepExclusion"

#plot
foreach j $::redcat::plotArray {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

puts $pout "$::redcat::plot2DFile"

puts $pout "$::redcat::2DCommand"

puts $pout "$::redcat::plotBool"

puts $pout "$::redcat::plot3DFile"

puts $pout "$::redcat::3DCommand"

puts $pout "$::redcat::plotBool3D"

#options
puts $pout "$::redcat::plot_type"

puts $pout "$::redcat::ErrorPad"

puts $pout "$::redcat::ErrorGlobal"

puts $pout "$::redcat::2DMapDatGlobal"

puts $pout "$::redcat::3DMapDatGlobal"

#RDC
puts $pout "$::redcat::Sxx"

puts $pout "$::redcat::Syy"

puts $pout "$::redcat::Szz"

puts $pout "$::redcat::a"

puts $pout "$::redcat::b"

puts $pout "$::redcat::c"

puts $pout "$::redcat::error_source"

puts $pout "$::redcat::error"

puts $pout "$::redcat::Sub_RDC"

puts $pout "$::redcat::corr_plot"

foreach j $::redcat::Cal_RDC {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

puts $pout "$::redcat::message_r"

puts $pout "$::redcat::message_q"

puts $pout "$::redcat::message_s"

#full report
puts $pout "$::redcat::RemoveHzAbove"

puts $pout "$::redcat::keepExclusion"

#ensemble
puts $pout "$::redcat::rCatFile"

puts $pout "$::redcat::loaded"

#system
puts $pout "$::redcat::pathRcat"

puts $pout "$::redcat::tcl_precision"

#old
puts $pout "$::redcat::iter1"

puts $pout "$::redcat::iter2"

#dynamic averaging
puts $pout "$::redcat::fin"

puts $pout "$::redcat::fout"

puts $pout "$::redcat::fstate"

puts $pout "$::redcat::numstate"

#rotate coordinates

puts $pout "$::redcat::rotationType"

puts $pout "$::redcat::x"

puts $pout "$::redcat::y"

puts $pout "$::redcat::z"

puts $pout "$::redcat::theta"

puts $pout "$::redcat::pdbfile"

puts $pout "$::redcat::redcatfile"

puts $pout "$::redcat::startres"

puts $pout "$::redcat::stopres"

foreach j [array get ::redcat::atom] {
    puts -nonewline $pout "$j&"
    
}

puts $pout ""

puts $pout "$::redcat::reportCount"

puts $pout "$::redcat::molNum"

close $pout

} else {
    ::redcat::dataLoadedError

}
}
#############################################################################
## Procedure:  loadState

proc ::redcat::loadState {fname} {
variable ::redcat::X
variable ::redcat::Sel
variable ::redcat::Count
variable ::redcat::outfname
variable ::redcat::MCItr
variable ::redcat::range
variable ::redcat::rejectArray
variable ::redcat::errorArray
variable ::redcat::solveArray
variable ::redcat::destroyTensor
variable ::redcat::tensorOffset
variable ::redcat::selTensor
variable ::redcat::plotArray
variable ::redcat::plot2DFile
variable ::redcat::plot3DFile
variable ::redcat::plot_type
variable ::redcat::ErrorPad
variable ::redcat::ErrorGlobal
variable ::redcat::2DMapDatGlobal
variable ::redcat::3DMapDatGlobal
variable ::redcat::Sxx
variable ::redcat::Syy
variable ::redcat::Szz
variable ::redcat::a
variable ::redcat::b
variable ::redcat::c
variable ::redcat::error_source
variable ::redcat::error
variable ::redcat::Sub_RDC
variable ::redcat::corr_plot
variable ::redcat::Cal_RDC
variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s
variable ::redcat::RemoveHzAbove
variable ::redcat::keepExclusion
variable ::redcat::rCatFile
variable ::redcat::loaded
variable ::redcat::pathRcat
variable ::redcat::tcl_precision
variable ::redcat::iter1
variable ::redcat::iter2
variable ::redcat::fin
variable ::redcat::fout
variable ::redcat::fstate
variable ::redcat::numstate
variable ::redcat::rotationType
variable ::redcat::x
variable ::redcat::y
variable ::redcat::z
variable ::redcat::theta
variable ::redcat::pdbfile
variable ::redcat::redcatfile
variable ::redcat::startres
variable ::redcat::stopres
variable ::redcat::atom
variable ::redcat::reportCount
variable ::redcat::molNum

if {$loaded >= 0} {
    ::redcat::errorDialog "Data Error" "Please close the current file before loading a new one."
	return
	
}

if {$fname == ""} {
    set fname [tk_getOpenFile]
    if {$fname == ""} {
        return
    
    }

}

#get data
set fp [open $fname r]
set stateString [read $fp]
close $fp

#parse data
set stateList [split $stateString "\n"]

#load parsed data
    set index 1
    #X
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {set i [expr $i+2]} {
        set key [lindex $parseList $i]
        set value [lindex $parseList [expr $i+1]]
        set ::redcat::X($key) $value
        
    }

    #Sel
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set selList [split $parse "&"]

    #Count
    set parse  [lindex $stateList $index]
    incr index
    set ::redcat::Count $parse

    #outfname
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::outfname $parse

    #MCItr
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::MCItr $parse

    #range
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::range $parse

    #rejectArray
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {incr i} {
        lappend ::redcat::rejectArray [lindex $parseList $i]
        
    }

    #errorArray
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {incr i} {
        lappend ::redcat::errorArray [lindex $parseList $i]
        
    }

    #solveArray
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {incr i} {
        lappend ::redcat::solveArray [lindex $parseList $i]
        
    }

    #destroyTensor
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::destroyTensor $parse
    
    #tensorOffset
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::tensorOffset $parse
    
    #selTensor
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::selTensor $parse
    
    #Selection
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::Selection $parse
    
    #keepExclusion
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::keepExclusion $parse
    
    #plotArray
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {incr i} {
        lappend ::redcat::plotArray [lindex $parseList $i]
        
    }

    #plot2DFile
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::plot2DFile $parse
    
    #2DCommand
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::2DCommand $parse
    
    #plotBool
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::plotBool $parse
    
    #plot3DFile
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::plot3DFile $parse
    
    #3DCommand
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::3DCommand $parse
    
    #plotBool3D
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::plotBool3D $parse
    
    #plot_type
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::plot_type $parse
    
    #ErrorPad
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::ErrorPad $parse
    
    #ErrorGlobal
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::ErrorGlobal $parse
    
    #2DMapDatGlobal
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::2DMapDatGlobal $parse
    
    #3DMapDatGlobal
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::3DMapDatGlobal $parse
    
    #Sxx
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::Sxx $parse
    
    #Syy
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::Syy $parse
    
    #Szz
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::Szz $parse
    
    #a
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::a $parse
    
    #b
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::b $parse
    
    #c
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::c $parse
    
    #error_source
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::error_source $parse
    
    #error
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::error $parse
    
    #Sub_RDC
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::Sub_RDC $parse
    
    #corr_plot
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::corr_plot $parse
    
    #Cal_RDC
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {incr i} {
        lappend ::redcat::Cal_RDC [lindex $parseList $i]
        
    }

    #message_r
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::message_r $parse
    
    #message_q
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::message_q $parse
    
    #message_s
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::message_s $parse
    
    #RemoveHzAbove
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::RemoveHzAbove $parse
    
    #keepExclusions
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::keepExclusions $parse
    
    #rCatFile
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::rCatFile $parse
    
    #loaded
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::loaded $parse
    
    #pathRcat
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::pathRcat $parse
    
    #tcl_percision
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::tcl_percision $parse
    
    #iter1
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::iter1 $parse
    
    #iter2
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::iter2 $parse
    
    #fin
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::fin $parse
    
    #fout
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::fout $parse
    
    #fstate
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::fstate $parse
    
    #numstate
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::numstate $parse
    
    #rotationType
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::rotationType $parse
    
    #x
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::x $parse
    
    #y
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::y $parse
    
    #z
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::z $parse
    
    #theta
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::theta $parse
    

    #pdbfile
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::pdbfile $parse
    

    #redcatfile
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::redcatfile $parse

    #startres
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::startres $parse

    #stopres
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::stopres $parse

    #atom
    set parse  [string replace [lindex $stateList $index] end end]
    incr index
    set parseList [split $parse "&"]
    for {set i 0} {$i<[llength $parseList]} {set i [expr $i+2]} {
        set key [lindex $parseList $i]
        set value [lindex $parseList [expr $i+1]]
        set ::redcat::atom($key) $value
        
    }
    
    #reportCount
    set reportCount [lindex $stateList $index]
    incr index
    set ::redcat::reportCount $parse
    
    #molNum
    set parse [lindex $stateList $index]
    incr index
    set ::redcat::molNum $parse
    
    set ::redcat::loaded 0
    
    ::redcat::Make_Win
    
    for {set i 0} {$i<[llength $selList]} {set i [expr $i+2]} {
        set key [lindex $selList $i]
        set value [lindex $selList [expr $i+1]]
        set ::redcat::Sel($key) $value
        
    }
}
#############################################################################
## Procedure:  Destroy_Report

proc ::redcat::Destroy_Report {command} {
variable reportCount

if {$reportCount > 0} {
    for {set i 0} {$i< $::redcat::reportCount} {incr i} {
        destroy [$command].imgFram$i.canGraph1
        destroy [$command].imgFram$i.canGraph2
        destroy [$command].imgFram$i.canGraph3
        destroy [$command].imgFram$i
        
    }
    
}

set ::redcat::reportCount 0
}
#############################################################################
## Procedure:  htmlReport

proc ::redcat::htmlReport {} {
	set baseTop [toplevel .topHtmlReport]
	wm title $baseTop "HTML Report"
	wm geometry $baseTop 450x150
    
	set headFrame [frame $baseTop.headFrame]
	set headFrameElem [list]
	lappend headFrameElem [label $headFrame.lbl0 -text "HTMLReport"]
	foreach i $headFrameElem {
	pack $i -side left

	}
	
	set bodyFrame [frame $baseTop.bodyFrame]
	set bodyFrameRow1 [frame $bodyFrame.bodyFrameRow1]
      	set bodyFrameRow2 [frame $bodyFrame.bodyFrameRow2]
#      	set bodyFrameRow3 [frame $bodyFrame.bodyFrameRow3]
        set bodyFrameElem [list]
	lappend bodyFrameElem [label $bodyFrameRow1.lbl0 -text "Please specify a name for the run"]
	lappend bodyFrameElem [entry $bodyFrameRow1.entr0 -textvariable htmlRunName]
	lappend bodyFrameElem [button $bodyFrameRow1.but0 -text "Save" -command {set htmlRunName [tk_getSaveFile]}]
        lappend bodyFrameElem [label $bodyFrameRow2.lbl1 -text "Remove Error Above:"]
        lappend bodyFrameElem [entry $bodyFrameRow2.entr1 -textvariable htmlError]
        set space [label $bodyFrame.lblSpace -text ""]
        
        foreach i $bodyFrameElem {
            pack $i -side left
        
        }
#        pack $bodyFrameRow1 $bodyFrameRow2 $bodyFrameRow3 -side top
        pack $bodyFrameRow1 $bodyFrameRow2 -side top
	
	set footFrame [frame $baseTop.footFrame]
	set footFrameElem [list]
	lappend footFrameElem [button $footFrame.but0 -text "Run" -command {Window show .top37
                                                                            genReport2 $htmlRunName $htmlError}]
	lappend footFrameElem [button $footFrame.but1 -text "Reset" -command {set htmlRunName ""
                                                                              set htmlError ""}]
	lappend footFrameElem [button $footFrame.but2 -text "Done" -command {destroy .topHtmlReport}]
	foreach i $footFrameElem {
		pack $i -side left
	
	}
		
	pack $headFrame $bodyFrame $footFrame -expand 1 -fill y -side top
}
#############################################################################
## Procedure:  binaryGif

proc ::redcat::binaryGif {img} {
set fopen [open "|convert $img gif:-" w+]
fconfigure $fopen -translation binary
set rawData [read $fopen]
close $fopen
return $rawData
}
#############################################################################
## Procedure:  gifToBase

proc ::redcat::gifToBase {gif} {
return [base64::encode $gif]
}
#############################################################################
## Procedure:  errorDialog

proc ::redcat::errorDialog {title errorMsg} {
tk_messageBox -type ok -title "$title" -message "$errorMsg"
}
#############################################################################
## Procedure:  checkRun

proc ::redcat::checkRun {} {
global ::redcat::Sel
global ::redcat::Count

set i 1
set c 0
while {$i<=$::redcat::Count && $c<5} {
    if {$::redcat::Sel($i)==1} {
        incr c
        
    }
    incr i

}

if {$c<5} {
    return 0

} else {
    return 1

}
}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {
variable ::redcat::outfname
variable ::redcat::iter1
variable ::redcat::iter2
variable ::redcat::range
variable ::redcat::plot_type
variable ::redcat::ErrorPad
variable ::redcat::ErrorGlobal
variable ::redcat::tcl_precision
variable ::redcat::error_source
variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s
variable ::redcat::MCItr
variable ::redcat::solveArray
variable ::redcat::rejectArray
variable ::redcat::errorArray
variable ::redcat::destroyTensor
variable ::redcat::tensorOffset
variable ::redcat::selTensor
variable ::redcat::X
variable ::redcat::RemoveHzAbove
variable ::redcat::fin
variable ::redcat::fout
variable ::redcat::plot2DFile
variable ::redcat::plotBool
variable ::redcat::plot3DFile
variable ::redcat::plotBool3D
variable ::redcat::rCatFile
variable ::redcat::loaded
variable ::redcat::pathRcat
variable ::redcat::2DMapDatGlobal
variable ::redcat::3DMapDatGlobal
variable ::redcat::a
variable ::redcat::b
variable ::redcat::c
variable ::redcat::Cal_RDC
variable ::redcat::corr_plot
variable ::redcat::Count
variable ::redcat::error
variable ::redcat::fstate
variable ::redcat::keepExclusion
variable ::redcat::numstate
variable ::redcat::plotArray
variable ::redcat::rotationType
variable ::redcat::Sel
variable ::redcat::Sub_RDC
variable ::redcat::Sxx
variable ::redcat::Syy
variable ::redcat::Szz
variable ::redcat::theta
variable ::redcat::x
variable ::redcat::y
variable ::redcat::z
variable ::redcat::reportCount

variable ::redcat::3DCommand
variable ::redcat::atom
variable ::redcat::Dip
variable ::redcat::Error_message
variable ::redcat::MapDatGlobal
variable ::redcat::pdbfile
variable ::redcat::molNum

array unset ::redcat::X
set ::redcat::outfname "Results.dat"
set ::redcat::iter1 10000
set ::redcat::iter2 10
set ::redcat::range 1
set ::redcat::plot_type png

set ::redcat::ErrorPad 0.1
set ::redcat::ErrorGlobal 2.0
set ::redcat::MCItr 10000

set ::redcat::solveArray ""
set ::redcat::rejectArray ""
set ::redcat::tensorArray ""
set ::redcat::errorArray ""

set ::redcat::plot2DFile ""
set ::redcat::plotBool 0
set ::redcat::plot3DFile ""
set ::redcat::plotBool3D 0

set ::redcat::fin ""
set ::redcat::fout ""

set ::redcat::destroyTensor 0
set ::redcat::tensorOffset 4
set ::redcat::selTensor -1

set ::redcat::RemoveHzAbove 1

set tcl_precision 17
set ::redcat::error_source 0
set ::redcat::message_r "RMSD = "
set ::redcat::message_q "Q-Factor = "
set ::redcat::message_s "S-Factor = "

set ::redcat::rCatFile ""


set ::redcat::loaded -1

#set pathRcat [exec sh -c "echo \$REDCATPATH"]
set ::redcat::pathIn [open /usr/share/Redcat/redcat.conf r]
gets $::redcat::pathIn ::redcat::pathRcat

set ::redcat::2DMapDatGlobal "$pathRcat/data/map.dat"
set ::redcat::3DMapDatGlobal "$pathRcat/data/map3D.dat"

set ::redcat::a ""
set ::redcat::b ""
set ::redcat::c ""
set ::redcat::Cal_RDC [list]
set ::redcat::corr_plot ""
set ::redcat::Count 0
set ::redcat::error ""
set ::redcat::fstate ""
set ::redcat::keepExclusion ""
set ::redcat::numstate ""
set ::redcat::plotArray [list]
set ::redcat::rotationType ""
array unset ::redcat::Sel 
set ::redcat::Sub_RDC ""
set ::redcat::Sxx ""
set ::redcat::Syy ""
set ::redcat::Szz ""
set ::redcat::theta ""
set ::redcat::x ""
set ::redcat::y ""
set ::redcat::z ""
set ::redcat::reportCount 0
set ::redcat::molNum 0

set ::redcat::windowList [list]

}

init {} {}

#################################
# vTcl GENERATED GUI PROCEDURES
#

proc ::redcat::vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $top passive
    wm geometry $top 1x1+0+0; update
    wm maxsize $top 2545 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm withdraw $top
    wm title $top "vTcl.tcl"
    bindtags $top "$top vTcl.tcl all"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    ###################
    # SETTING GEOMETRY
    ###################

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.fullReport {base} {
    if {$base == ""} {
        set base .fullReport
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m67" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 800x577+312+313; update
    wm maxsize $top 1265 770
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 0 0
    wm title $top "Full Report"
    vTcl:DefineAlias "$top" "fullReport" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    button $top.reportOk \
        \
        -command {#::redcat::genReport $outfname $::redcat::RemoveHzAbove
::redcat::genReport1 $::redcat::keepExclusion {}} \
        -text OK 
    vTcl:DefineAlias "$top.reportOk" "Button1" ::redcat::vTcl:WidgetProc "fullReport" 1
    menu $top.m67 \
        -tearoff 1 
    $top.m67 add command \
        \
        -command {set saveFile [tk_getSaveFile]
if {$saveFile == ""} {
    ::redcat::errorDialog "Data Error" "Input file name is blank.\nFile not saved."
    return
} 

set fout [open $saveFile w]
puts -nonewline $fout "<!DOCTYPE html><html><head><title>REDCAT Full Report - $saveFile</title></head><body><h1>Full Report</h1>"
foreach {key value index} [FullReportText dump 1.0 end] {
  if {$key == "text"} {
    puts -nonewline $fout "$value<br />"
    
  } elseif {$key == "image"} {
    puts -nonewline $fout "<img src=\"data:image/gif;base64,"
    set img [$value data -format ppm]
    set imageOut [open "tempImg.gif" w+]
    fconfigure $imageOut -translation binary
    puts $imageOut $img
    close $imageOut
    puts -nonewline $fout [::redcat::gifToBase [::redcat::binaryGif "tempImg.gif"]]
    exec rm tempImg.gif
    puts -nonewline $fout " \" />"
  
  }
  
}
puts -nonewline $fout "</body>\n</html>"
close $fout} \
        -label Save 
    button $top.reportCancel \
        \
        -command {::redcat::Destroy_Report FullReportText
FullReportText delete 0.0 end} \
        -text Clear 
    vTcl:DefineAlias "$top.reportCancel" "Button2" ::redcat::vTcl:WidgetProc "fullReport" 1
    frame $top.cpd67 \
        -height 539 -width 793 
    vTcl:DefineAlias "$top.cpd67" "Frame2" ::redcat::vTcl:WidgetProc "fullReport" 1
    set site_3_0 $top.cpd67
    scrollbar $site_3_0.01 \
        -command "$site_3_0.03 xview" -orient horizontal 
    vTcl:DefineAlias "$site_3_0.01" "Scrollbar3" ::redcat::vTcl:WidgetProc "fullReport" 1
    scrollbar $site_3_0.02 \
        -command "$site_3_0.03 yview" 
    vTcl:DefineAlias "$site_3_0.02" "Scrollbar4" ::redcat::vTcl:WidgetProc "fullReport" 1
    text $site_3_0.03 \
        -background #ffffff -height 39 -width 110 \
        -xscrollcommand "$site_3_0.01 set" -yscrollcommand "$site_3_0.02 set" 
    vTcl:DefineAlias "$site_3_0.03" "FullReportText" ::redcat::vTcl:WidgetProc "fullReport" 1
    grid $site_3_0.01 \
        -in $site_3_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_3_0.02 \
        -in $site_3_0 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $site_3_0.03 \
        -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    label $top.excludeLbl \
        -text {Remove Error Above:} 
    vTcl:DefineAlias "$top.excludeLbl" "Label1" ::redcat::vTcl:WidgetProc "fullReport" 1
    button $top.removeNhz \
        -command {::redcat::genReport $outfile [tk_messageBox -message ""]} -text Nhz 
    vTcl:DefineAlias "$top.removeNhz" "Button8" ::redcat::vTcl:WidgetProc "fullReport" 1
    ComboBox $top.removeHz \
        -dropenabled 1 -entrybg white -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -takefocus 1 -textvariable ::redcat::RemoveHzAbove \
        -values {1 2 3 4 5} 
    vTcl:DefineAlias "$top.removeHz" "ComboBox2" ::redcat::vTcl:WidgetProc "fullReport" 1
    bindtags $top.removeHz "$top.removeHz BwComboBox $top all"
    button $top.exitFullReport \
        -command {::redcat::Window hide .fullReport} -text Done 
    vTcl:DefineAlias "$top.exitFullReport" "Button3" ::redcat::vTcl:WidgetProc "fullReport" 1
    text $top.featureFullReport \
        -font [vTcl:font:getFontFromDescr "-family courier -size 12"] \
        -foreground #0000ff -highlightcolor #d9d9d9 -highlightthickness 0 \
        -insertbackground black -relief flat -selectbackground #d9d9d9 \
        -selectforeground #d9d9d9 
    vTcl:DefineAlias "$top.featureFullReport" "FeatureFullReport" ::redcat::vTcl:WidgetProc "fullReport" 1
    checkbutton $top.che60 \
        -text {Keep Error Exclusions} -variable ::redcat::keepExclusion 
    vTcl:DefineAlias "$top.che60" "Checkbutton1" ::redcat::vTcl:WidgetProc "fullReport" 1
    bindtags $top.che60 "$top.che60 Checkbutton $top all _vTclBalloon"
    bind $top.che60 <<SetBalloon>> {
        set ::redcat::vTcl::balloon::%W {Check this to propagate exclusions to Main Window\nand to show Best S, Da, Dr, and R for exclusions.}
    }
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.reportOk \
        -in $top -x 630 -y 545 -anchor nw -bordermode ignore 
    place $top.reportCancel \
        -in $top -x 680 -y 545 -anchor nw -bordermode ignore 
    place $top.cpd67 \
        -in $top -x 5 -y 5 -width 793 -height 539 -anchor nw \
        -bordermode inside 
    place $top.excludeLbl \
        -in $top -x 5 -y 545 -anchor nw -bordermode ignore 
    place $top.removeNhz \
        -in $top -x -70 -y 460 -width 48 -height 23 -anchor nw \
        -bordermode ignore 
    place $top.removeHz \
        -in $top -x 140 -y 545 -anchor nw -bordermode ignore 
    place $top.exitFullReport \
        -in $top -x 740 -y 545 -anchor nw -bordermode ignore 
    place $top.featureFullReport \
        -in $top -x 10 -y 580 -width 568 -height 20 -anchor nw \
        -bordermode ignore 
    place $top.che60 \
        -in $top -x 360 -y 545 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.plot_vectors {base} {
    if {$base == ""} {
        set base .plot_vectors
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m48" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 186x166+615+498; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Plot Vectors"
    vTcl:DefineAlias "$top" "PlotVectors" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    label $top.lab47 \
        -disabledforeground #a2a2a2 -text Plot: 
    vTcl:DefineAlias "$top.lab47" "Label1" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    radiobutton $top.rad55 \
        -disabledforeground #a2a2a2 -text All -value 1 -variable ::redcat::PlotWhat 
    vTcl:DefineAlias "$top.rad55" "Radiobutton1" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    radiobutton $top.rad56 \
        -disabledforeground #a2a2a2 -text Selected -value 0 \
        -variable ::redcat::PlotWhat 
    vTcl:DefineAlias "$top.rad56" "Radiobutton2" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    button $top.but47 \
        -command {close $PlotVector
::redcat::Window hide .plot_vectors} \
        -disabledforeground #a2a2a2 -text Done 
    vTcl:DefineAlias "$top.but47" "Button1" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    menu $top.m48 \
        -disabledforeground #a2a2a2 -tearoff 1 
    button $top.but48 \
        \
        -command {puts $PlotVector "set xrange \[$xmin:$xmax\]"
puts $PlotVector "set yrange \[$ymin:$ymax\]"
puts $PlotVector "set zrange \[$zmin:$zmax\]"
puts $PlotVector "replot"} \
        -disabledforeground #a2a2a2 -text Plot 
    vTcl:DefineAlias "$top.but48" "Button2" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    label $top.lab49 \
        -disabledforeground #a2a2a2 -text {::redcat::X range:} 
    vTcl:DefineAlias "$top.lab49" "Label2" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    label $top.cpd50 \
        -disabledforeground #a2a2a2 -text {Y range:} 
    vTcl:DefineAlias "$top.cpd50" "Label3" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    label $top.cpd51 \
        -disabledforeground #a2a2a2 -text {Z range:} 
    vTcl:DefineAlias "$top.cpd51" "Label4" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.ent52 \
        -background white -insertbackground black -textvariable ::redcat::xmin 
    vTcl:DefineAlias "$top.ent52" "Entry1" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.cpd56 \
        -background white -insertbackground black -textvariable ::redcat::xmax 
    vTcl:DefineAlias "$top.cpd56" "Entry2" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    label $top.lab55 \
        -disabledforeground #a2a2a2 -text Min 
    vTcl:DefineAlias "$top.lab55" "Label5" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    label $top.cpd57 \
        -disabledforeground #a2a2a2 -text Max 
    vTcl:DefineAlias "$top.cpd57" "Label6" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.cpd58 \
        -background white -insertbackground black -textvariable ::redcat::ymin 
    vTcl:DefineAlias "$top.cpd58" "Entry3" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.cpd59 \
        -background white -insertbackground black -textvariable ::redcat::ymax 
    vTcl:DefineAlias "$top.cpd59" "Entry4" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.cpd60 \
        -background white -insertbackground black -textvariable ::redcat::zmin 
    vTcl:DefineAlias "$top.cpd60" "Entry5" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    entry $top.cpd61 \
        -background white -insertbackground black -textvariable ::redcat::zmax 
    vTcl:DefineAlias "$top.cpd61" "Entry6" ::redcat::vTcl:WidgetProc "PlotVectors" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.lab47 \
        -in $top -x 15 -y 12 -width 33 -height 20 -anchor nw \
        -bordermode ignore 
    place $top.rad55 \
        -in $top -x 50 -y 10 -width 43 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.rad56 \
        -in $top -x 100 -y 10 -width 81 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.but47 \
        -in $top -x 110 -y 130 -anchor nw -bordermode ignore 
    place $top.but48 \
        -in $top -x 35 -y 130 -anchor nw -bordermode ignore 
    place $top.lab49 \
        -in $top -x 15 -y 50 -anchor nw -bordermode ignore 
    place $top.cpd50 \
        -in $top -x 15 -y 75 -width 55 -height 20 -anchor nw \
        -bordermode ignore 
    place $top.cpd51 \
        -in $top -x 15 -y 101 -width 55 -height 20 -anchor nw \
        -bordermode ignore 
    place $top.ent52 \
        -in $top -x 70 -y 50 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.cpd56 \
        -in $top -x 125 -y 50 -width 33 -height 22 -anchor nw \
        -bordermode inside 
    place $top.lab55 \
        -in $top -x 75 -y 30 -anchor nw -bordermode ignore 
    place $top.cpd57 \
        -in $top -x 125 -y 30 -anchor nw -bordermode inside 
    place $top.cpd58 \
        -in $top -x 70 -y 75 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.cpd59 \
        -in $top -x 125 -y 75 -width 33 -height 22 -anchor nw \
        -bordermode inside 
    place $top.cpd60 \
        -in $top -x 70 -y 100 -width 33 -height 22 -anchor nw \
        -bordermode inside 
    place $top.cpd61 \
        -in $top -x 125 -y 100 -width 33 -height 22 -anchor nw \
        -bordermode inside 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top24 {base} {
    if {$base == ""} {
        set base .top24
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m25" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 313x496+488+342; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "RDC"
    vTcl:DefineAlias "$top" "RDC" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    menu $top.m25 \
        -selectcolor #fffff0 -tearoff 1 
    frame $top.fra26 \
        -borderwidth 2 -relief groove -height 70 -width 372 
    vTcl:DefineAlias "$top.fra26" "Frame9" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_3_0 $top.fra26
    frame $site_3_0.cpd27 \
        -borderwidth 1 -relief raised -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.cpd27" "Frame10" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.cpd27
    label $site_4_0.01 \
        -anchor w -relief groove -text Sxx 
    vTcl:DefineAlias "$site_4_0.01" "Label6" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::Sxx -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry5" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.fra28 \
        -borderwidth 1 -relief raised -cursor fleur -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.fra28" "Frame11" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.fra28
    label $site_4_0.01 \
        -anchor w -relief groove -text Syy 
    vTcl:DefineAlias "$site_4_0.01" "Label7" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::Syy -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry6" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.fra29 \
        -borderwidth 1 -relief raised -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.fra29" "Frame12" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.fra29
    label $site_4_0.01 \
        -anchor w -relief groove -text Szz 
    vTcl:DefineAlias "$site_4_0.01" "Label8" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::Szz -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry7" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.fra30 \
        -borderwidth 1 -relief raised -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.fra30" "Frame13" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.fra30
    label $site_4_0.01 \
        -anchor w -relief groove -text a 
    vTcl:DefineAlias "$site_4_0.01" "Label9" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::a -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry8" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.fra32 \
        -borderwidth 1 -relief raised -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.fra32" "Frame15" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.fra32
    label $site_4_0.01 \
        -anchor w -relief groove -text c 
    vTcl:DefineAlias "$site_4_0.01" "Label11" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::c -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry10" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.fra31 \
        -borderwidth 1 -relief raised -cursor fleur -height 26 -width 86 
    vTcl:DefineAlias "$site_3_0.fra31" "Frame14" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_4_0 $site_3_0.fra31
    label $site_4_0.01 \
        -anchor w -relief groove -text b 
    vTcl:DefineAlias "$site_4_0.01" "Label10" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_4_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::b -width 8 
    vTcl:DefineAlias "$site_4_0.02" "Entry9" ::redcat::vTcl:WidgetProc "RDC" 1
    frame $site_4_0.fra37 \
        -borderwidth 1 -relief raised -cursor fleur -height 26 -width 86 
    vTcl:DefineAlias "$site_4_0.fra37" "Frame17" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_5_0 $site_4_0.fra37
    label $site_5_0.01 \
        -anchor w -relief groove -text b 
    vTcl:DefineAlias "$site_5_0.01" "Label12" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_5_0.02 \
        -background #dcdcdc -cursor {} -highlightthickness 0 \
        -insertbackground black -textvariable ::redcat::b -width 8 
    vTcl:DefineAlias "$site_5_0.02" "Entry11" ::redcat::vTcl:WidgetProc "RDC" 1
    pack $site_5_0.01 \
        -in $site_5_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_5_0.02 \
        -in $site_5_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    place $site_4_0.fra37 \
        -in $site_4_0 -x 105 -y 30 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    radiobutton $site_3_0.rad62 \
        -command {.top24.fra26.ent64 configure -state disabled} \
        -disabledforeground #a1a1a1 -overrelief flat -text {From file} \
        -value -1 -variable ::redcat::error_source 
    vTcl:DefineAlias "$site_3_0.rad62" "Radiobutton1" ::redcat::vTcl:WidgetProc "RDC" 1
    radiobutton $site_3_0.rad63 \
        -command {.top24.fra26.ent64 configure -state normal
set error 0} \
        -disabledforeground #a1a1a1 -text {Constant value} -textvariable ::redcat::e \
        -value 1 -variable ::redcat::error_source 
    vTcl:DefineAlias "$site_3_0.rad63" "Radiobutton2" ::redcat::vTcl:WidgetProc "RDC" 1
    entry $site_3_0.ent64 \
        -background white -disabledforeground #a1a1a1 -insertbackground black \
        -textvariable ::redcat::error 
    vTcl:DefineAlias "$site_3_0.ent64" "Entry1" ::redcat::vTcl:WidgetProc "RDC" 1
    label $site_3_0.lab65 \
        -disabledforeground #a1a1a1 -text Error: 
    vTcl:DefineAlias "$site_3_0.lab65" "Label1" ::redcat::vTcl:WidgetProc "RDC" 1
    place $site_3_0.cpd27 \
        -in $site_3_0 -x 0 -y 0 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.fra28 \
        -in $site_3_0 -x 105 -y 0 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.fra29 \
        -in $site_3_0 -x 210 -y 0 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.fra30 \
        -in $site_3_0 -x 0 -y 30 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.fra32 \
        -in $site_3_0 -x 210 -y 30 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.fra31 \
        -in $site_3_0 -x 105 -y 30 -width 86 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.rad62 \
        -in $site_3_0 -x 45 -y 63 -width 81 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_3_0.rad63 \
        -in $site_3_0 -x 127 -y 63 -width 118 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_3_0.ent64 \
        -in $site_3_0 -x 248 -y 62 -width 53 -height 22 -anchor nw \
        -bordermode ignore 
    place $site_3_0.lab65 \
        -in $site_3_0 -x 3 -y 64 -width 40 -height 20 -anchor nw \
        -bordermode ignore 
    button $top.but33 \
        \
        -command {if {$::redcat::error_source == -1} {
   ::redcat::Get_Sub_RDC $::redcat::Sxx $::redcat::Syy $::redcat::Szz $::redcat::a $::redcat::b $::redcat::c $::redcat::error_source $::redcat::Sub_RDC $::redcat::corr_plot
}

if {$::redcat::error_source == 1} {
   ::redcat::Get_Sub_RDC $::redcat::Sxx $::redcat::Syy $::redcat::Szz $::redcat::a $::redcat::b $::redcat::c $::redcat::error $::redcat::Sub_RDC $::redcat::corr_plot
}} \
        -text {Calculate RDC} 
    vTcl:DefineAlias "$top.but33" "you Button6" ::redcat::vTcl:WidgetProc "RDC" 1
    button $top.but34 \
        -command {::redcat::Window hide .top24} -height 0 -text Done 
    vTcl:DefineAlias "$top.but34" "Button7" ::redcat::vTcl:WidgetProc "RDC" 1
    frame $top.cpd36 \
        -borderwidth 1 -relief raised -cursor fleur -height 270 -width 313 
    vTcl:DefineAlias "$top.cpd36" "Frame16" ::redcat::vTcl:WidgetProc "RDC" 1
    set site_3_0 $top.cpd36
    listbox $site_3_0.01 \
        -background #dcdcdc \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -height 0 \
        -xscrollcommand "$site_3_0.02 set" -yscrollcommand "$site_3_0.03 set" \
        -listvariable ::redcat::Cal_RDC 
    vTcl:DefineAlias "$site_3_0.01" "Listbox1" ::redcat::vTcl:WidgetProc "RDC" 1
    scrollbar $site_3_0.02 \
        -command "$site_3_0.01 xview" -orient horizontal 
    vTcl:DefineAlias "$site_3_0.02" "Scrollbar6" ::redcat::vTcl:WidgetProc "RDC" 1
    scrollbar $site_3_0.03 \
        -command "$site_3_0.01 yview" 
    vTcl:DefineAlias "$site_3_0.03" "Scrollbar7" ::redcat::vTcl:WidgetProc "RDC" 1
    grid $site_3_0.01 \
        -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $site_3_0.02 \
        -in $site_3_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_3_0.03 \
        -in $site_3_0 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    checkbutton $top.che39 \
        -relief ridge -selectcolor #fffff0 -text {Substitute RDC} \
        -variable ::redcat::Sub_RDC 
    vTcl:DefineAlias "$top.che39" "Checkbutton2" ::redcat::vTcl:WidgetProc "RDC" 1
    message $top.mes47 \
        -highlightcolor black -justify left -relief ridge -text {RMSD =} \
        -textvariable ::redcat::message_r -width 100 
    vTcl:DefineAlias "$top.mes47" "Message1" ::redcat::vTcl:WidgetProc "RDC" 1
    message $top.mes48 \
        -highlightcolor black -justify right -relief ridge -text {Q-Factor =} \
        -textvariable ::redcat::message_q -width 100 
    vTcl:DefineAlias "$top.mes48" "Message2" ::redcat::vTcl:WidgetProc "RDC" 1
    checkbutton $top.che47 \
        -disabledforeground #a2a2a2 -relief ridge -text {Correlation Plot} \
        -variable ::redcat::corr_plot 
    vTcl:DefineAlias "$top.che47" "Checkbutton1" ::redcat::vTcl:WidgetProc "RDC" 1
    button $top.but53 \
        \
        -command {variable ::redcat::message_r
variable ::redcat::message_q
variable ::redcat::message_s
variable ::redcat::Cal_RDC
variable ::redcat:: Sub_RDC
variable ::redcat:: corr_plot
variable ::redcat:: error_source

set ::redcat::Sxx ""
set ::redcat::Syy ""
set ::redcat::Szz ""
set ::redcat::a ""
set ::redcat::b ""
set ::redcat::c ""
set ::redcat::Sub_RDC 0
set ::redcat::corr_plot 0
set ::redcat::error_source 0
set ::redcat::Cal_RDC ""
set ::redcat::message_r "RMSD ="
set ::redcat::message_q "Q-Factor ="
set ::redcat::message_s "S-Factor ="} \
        -text Reset 
    vTcl:DefineAlias "$top.but53" "Button1" ::redcat::vTcl:WidgetProc "RDC" 1
    message $top.mes61 \
        -highlightcolor black -relief ridge -text {S-Factor =} \
        -textvariable ::redcat::message_s -width 100 
    vTcl:DefineAlias "$top.mes61" "Message3" ::redcat::vTcl:WidgetProc "RDC" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.fra26 \
        -in $top -x 0 -y 0 -width 313 -relwidth 0 -height 90 -relheight 0 \
        -anchor nw -bordermode ignore 
    place $top.but33 \
        -in $top -x 5 -y 94 -width 114 -height 28 -anchor nw \
        -bordermode ignore 
    place $top.but34 \
        -in $top -x 120 -y 95 -width 54 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.cpd36 \
        -in $top -x 0 -y 157 -width 313 -relwidth 0 -height 270 -relheight 0 \
        -anchor nw -bordermode ignore 
    grid columnconf $top.cpd36 0 -weight 1
    grid rowconf $top.cpd36 0 -weight 1
    place $top.che39 \
        -in $top -x 180 -y 95 -width 128 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.mes47 \
        -in $top -x 3 -y 436 -width 100 -height 57 -anchor nw \
        -bordermode ignore 
    place $top.mes48 \
        -in $top -x 106 -y 436 -width 100 -height 57 -anchor nw \
        -bordermode ignore 
    place $top.che47 \
        -in $top -x 180 -y 115 -width 118 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.but53 \
        -in $top -x 5 -y 125 -width 65 -height 28 -anchor nw \
        -bordermode ignore 
    place $top.mes61 \
        -in $top -x 208 -y 436 -width 100 -height 57 -anchor nw \
        -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top32 {base} {
    if {$base == ""} {
        set base .top32
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m33" -highlightcolor black -takefocus 1 
    wm focusmodel $top passive
    wm geometry $top 560x380+396+372; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Main window"
    set ::redcat::mainWindow $top
    vTcl:DefineAlias "$top" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    menu $top.m33 \
        -tearoff 1 
    $top.m33 add cascade \
        -menu "$top.m33.men34" -label File 
    set site_3_0 $top.m33
    menu $site_3_0.men34 \
        -tearoff 0 
    $site_3_0.men34 add command \
        -command {::redcat::Window show .top56} -label {Prepare Input} 
    $site_3_0.men34 add cascade \
        -menu "$site_3_0.men34.men48" -command {} -label {Import RDC Tool} 
    set site_4_0 $site_3_0.men34
    menu $site_4_0.men48 \
        -tearoff 0 
    $site_4_0.men48 add command \
        \
        -command {variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel

set fin [open [tk_getOpenFile] r]
set i 0

while {[gets $fin oline] >= 0} {
 if {![regexp {^#} $oline]} {
    incr i
    if {$i <= $::redcat::Count} {
       set num_subs [regsub -all "\t" $oline " " line]
       set data [split $line " "]
       if { [llength $data] > 1 } {
          set ::redcat::X(7,$i) [lindex $data 0]
          set ::redcat::X(9,$i) [lindex $data 1]
	       if {$::redcat::X(7,$i) == 999} {
                set ::redcat::Sel($i) 0
                
            } else {
                if { $::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999} {
                    set ::redcat::Sel($i) 0
                    
                } else {
                    set ::redcat::Sel($i) 1
                
                }
                
           }
       } else {
          set ::redcat::X(7,$i) [lindex $data 0]    
	  if {$::redcat::X(7,$i) == 999} {
                set Sel($i) 0
                
       } else {
                if { $::redcat::X(1,$i) == 999 || $::redcat::X(2,$i) == 999 || $::redcat::X(3,$i) == 999 || $::redcat::X(4,$i) == 999 || $::redcat::X(5,$i) == 999 || $::redcat::X(6,$i) == 999} {
                    set ::redcat::Sel($i) 0
                    
                } else {
                    set ::redcat::Sel($i) 1
                
                }
                
       }
       
       }
    }
 }  
}
close $fin} \
        -label {Two Column ASCII} 
    $site_4_0.men48 add command \
        \
        -command {variable ::redcat::X
variable ::redcat::Count
variable ::redcat::Sel

set fin [open [tk_getOpenFile] r]
set i 0

while {[gets $fin oline] >= 0} {
 if { [regexp (^.*N-H) $oline] } {
    set data [split $oline " "]
    if { [llength $data] >= 2 } {
       set First [lindex $data 0]
       set Data  [lindex $data 1]
       if { [regexp (^.*N-H) $First] } {
          set Length [string length $First]
          set location [string range $First 1 [expr $Length - 1 - 3]]
                 set ::redcat::X(7,$location) $Data 
          if {$::redcat::X(7,$location) == 999} {
             set ::redcat::Sel($location) 0
          } else {
             set ::redcat::Sel($location) 1
          }
       }

    }
 }
}
close $fin} \
        -label {Two Column SPARKY} 
    $site_4_0.men48 add command \
        -command ::redcat::rdcFromRedcat -label {RDC from REDCAT} 
    $site_4_0.men48 add command \
        -command ::redcat::rdcFromRedcraft -label {RDC from REDCRAFT} 
    $site_3_0.men34 add cascade \
        -menu "$site_3_0.men34.menu60" -command {} -label Save 
    set site_4_0 $site_3_0.men34
    menu $site_4_0.menu60 \
        -tearoff 0 
    $site_4_0.menu60 add command \
        \
        -command {if {[::redcat::Save_Data "" 0]==0} {
     ::redcat::dataLoadedError
     return

}} \
        -label {Save Redcat File} 
    $site_4_0.menu60 add command \
        -command {::redcat::saveState {}} -label {Save State} 
    $site_3_0.men34 add cascade \
        -menu "$site_3_0.men34.menu61" -command {} -label Load 
    set site_4_0 $site_3_0.men34
    menu $site_4_0.menu61 \
        -tearoff 0 
    $site_4_0.menu61 add command \
        -command {::redcat::Get_Data ""
::redcat::Make_Win} -label {Load Redcat File} 
    $site_4_0.menu61 add command \
        -command {::redcat::loadState {}} -label {Load State} 
    $site_3_0.men34 add command \
        -command ::redcat::closeRedcat -label Close 
#    $site_3_0.men34 add command \
#        -command exit -label Exit 
    $top.m33 add cascade \
        -menu "$top.m33.men35" -label Edit 
    set site_3_0 $top.m33
    menu $site_3_0.men35 \
        -tearoff 0 
    $site_3_0.men35 add command \
        -command {::redcat::Window show .top53} -label Select 
    $site_3_0.men35 add command \
        -command {::redcat::Window show .top34} -label Options 
    $top.m33 add cascade \
        -menu "$top.m33.men36" -label Tools 
    set site_3_0 $top.m33
    menu $site_3_0.men36 \
        -tearoff 0 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.men56" -command {} -label Solution 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.men56 \
        -tearoff 0 
    $site_4_0.men56 add command \
        \
        -command {variable ::redcat::X
if { [array exists ::redcat::X] } {
    if { [llength $::redcat::solveArray] < 1 } {
        ::redcat::Run_SVD 2
    
    }
    ::redcat::printSolve
    
} else {
    ::redcat::dataLoadedError

}} \
        -label {Get Solutions} 
    $site_4_0.men56 add command \
        \
        -command {::redcat::Window show .fullReport
.fullReport.cpd67.03 delete 1.0 end} \
        -label {Generate Report} 
    $site_4_0.men56 add command \
        -command ::redcat::launchEnsemble -label {Ensemble Solutions} 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.men59" -command {} -label {Error Analysis} 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.men59 \
        -tearoff 0 
    $site_4_0.men59 add command \
        \
        -command {variable ::redcat::X
if { [array exists ::redcat::X] } {
    if { [llength $::redcat::solveArray] < 1 } {
        ::redcat::Run_SVD 2
    
    }
    ::redcat::Run_Error_Analysis 0
    
} else {
    ::redcat::dataLoadedError

}} \
        -label {Perfrom Error Analysis} 
    $site_4_0.men59 add command \
        \
        -command {variable ::redcat::X
if { [array exists ::redcat::X] } {
    if { [llength $::redcat::solveArray] < 1 } {
        ::redcat::Run_SVD 2
    
    }
    ::redcat::Run_Error_Analysis 1
    
} else {
    ::redcat::dataLoadedError

}} \
        -label {Get Estimated Errors for All} 
    $site_4_0.men59 add command \
        \
        -command {variable ::redcat::X
if { [array exists ::redcat::X] } {
    if { [llength $::redcat::solveArray] < 1 } {
        ::redcat::Run_SVD 2
    
    }
    ::redcat::Run_Error_Analysis 2
    
} else {
    ::redcat::dataLoadedError

}} \
        -label {Get Estimated Errors for Violations} 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.men57" -command {} -label Plot 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.men57 \
        -tearoff 0 
    $site_4_0.men57 add command \
        -command {::redcat::Window show .top61} -label {2D SF Plot} 
    $site_4_0.men57 add command \
        -command {::redcat::Window show .top62} -label {3D SF Plot} 
    $site_4_0.men57 add command \
        \
        -command {::redcat::Window show .plot_vectors

set PlotVector [open "|gnuplot" w+]
flush $PlotVector
fconfigure $PlotVector -buffering none

puts $PlotVector "set mouse"

set xmin -2
set xmax 2
set ymin -2
set ymax 2
set zmin -2
set zmax 2

puts $PlotVector "set xrange \[$xmin:$xmax\]"
puts $PlotVector "set yrange \[$ymin:$ymax\]"
puts $PlotVector "set zrange \[$zmin:$zmax\]"
puts $PlotVector "splot \"< echo 0 0 0\""
puts $PlotVector "set arrow to 0,0,2 lt 2 lw 1"
puts $PlotVector "set arrow to 0,2,0 lt 2 lw 1"
puts $PlotVector "set arrow to 2,0,0 lt 2 lw 1"

for {set i 1} {$i <= $::redcat::Count} {incr i} {
  if {$::redcat::Sel($i) == 1} {
     set x [expr $::redcat::X(4,$i) - $::redcat::X(1,$i)]
     set y [expr $::redcat::X(5,$i) - $::redcat::X(2,$i)]
     set z [expr $::redcat::X(6,$i) - $::redcat::X(3,$i)]
     puts $PlotVector "set arrow to $x, $y, $z lt 3 lw 2"
  }
}} \
        -label {Plot Vectors} 
    $site_4_0.men57 add command \
        -command {::redcat::plotError {}} -label {Plot Violations} 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.men58" -command {} -label Rotate 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.men58 \
        -tearoff 0 
    $site_4_0.men58 add command \
        -command {::redcat::Window show .top35} -label {Rotate PDB} 
    $site_4_0.men58 add command \
        -command {::redcat::Window show .top36} -label {Rotate Coordinates} 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.men61" -command {} -label Calculations 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.men61 \
        -tearoff 0 
    $site_4_0.men61 add command \
        -command {::redcat::Window show .top24} -label {Calculate/Substitute RDC} 
    $site_4_0.men61 add command \
        -command {::redcat::Window show .top38} -label {Dynamic Averaging} 
    $site_3_0.men36 add cascade \
        -menu "$site_3_0.men36.menu60" -command {} \
        -label {Structure Refinement} 
    set site_4_0 $site_3_0.men36
    menu $site_4_0.menu60 \
        -tearoff 0 
    $site_4_0.menu60 add command \
        -command ::redcat::launchXplorGUI -label XplorGUI
    $top.m33 add cascade \
        -menu "$top.m33.men37" -label Help 
    set site_3_0 $top.m33
    menu $site_3_0.men37 \
        -tearoff 0 
    $site_3_0.men37 add command \
        \
        -command {::redcat::launchBrowser http://ifestos.cse.sc.edu/REDCAT/documentation} \
        -label Help 
    $site_3_0.men37 add command \
        -command ::redcat::about -label {About REDCAT} 
    $site_3_0.men37 add command \
        -command {::redcat::launchBrowser mailto:valafarlab@gmail.com} \
        -label {Contact Us} 
    $site_3_0.men37 add command \
        -command {launchBrowser http://ifestos.cse.sc.edu/contactus} \
        -label {Bug Report} 
    frame $top.cpd38 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd38" "Frame1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.cpd38
    scrollbar $site_3_0.01 \
        -command "$site_3_0.03 xview" -orient horizontal 
    vTcl:DefineAlias "$site_3_0.01" "Scrollbar2" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    scrollbar $site_3_0.02 \
        -command "$site_3_0.03 yview" 
    vTcl:DefineAlias "$site_3_0.02" "Scrollbar3" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    text $site_3_0.03 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -height 10 \
        -width 20 -xscrollcommand "$site_3_0.01 set" \
        -yscrollcommand "$site_3_0.02 set" 
    vTcl:DefineAlias "$site_3_0.03" "Text1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    grid $site_3_0.01 \
        -in $site_3_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_3_0.02 \
        -in $site_3_0 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $site_3_0.03 \
        -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    frame $top.fra39 \
        -borderwidth 2 -relief groove 
    vTcl:DefineAlias "$top.fra39" "MainWindow1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.fra39
    frame $site_3_0.cpd40 \
        -borderwidth 1 -relief raised -cursor fleur -height 26 -width 380 
    vTcl:DefineAlias "$site_3_0.cpd40" "Frame3" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_4_0 $site_3_0.cpd40
    label $site_4_0.01 \
        -anchor w -relief groove -text {Output file name:} -width 29 
    vTcl:DefineAlias "$site_4_0.01" "Label1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    entry $site_4_0.02 \
        -cursor {} -highlightthickness 0 -textvariable ::redcat::outfname 
    vTcl:DefineAlias "$site_4_0.02" "Entry1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.cpd41 \
        -borderwidth 1 -relief raised -cursor fleur -height 26 -width 380 
    vTcl:DefineAlias "$site_3_0.cpd41" "Frame4" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_4_0 $site_3_0.cpd41
    label $site_4_0.01 \
        -anchor w -relief groove -text {Number of error space samplings:} \
        -width 29 
    vTcl:DefineAlias "$site_4_0.01" "Label2" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    entry $site_4_0.02 \
        -cursor {} -highlightthickness 0 -textvariable ::redcat::MCItr 
    vTcl:DefineAlias "$site_4_0.02" "Entry2" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $site_3_0.cpd42 \
        -borderwidth 1 -relief raised -height 26 -width 380 
    vTcl:DefineAlias "$site_3_0.cpd42" "Frame5" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_4_0 $site_3_0.cpd42
    entry $site_4_0.02 \
        -cursor {} -disabledbackground #666666 -highlightthickness 0 \
        -state disabled -textvariable ::redcat::nullItr 
    vTcl:DefineAlias "$site_4_0.02" "Entry3" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    label $site_4_0.01 \
        -anchor w -relief groove -text {Number of NULL space sampling:} \
        -width 29 
    vTcl:DefineAlias "$site_4_0.01" "Label3" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    frame $site_3_0.cpd43 \
        -borderwidth 1 -relief raised -height 26 -width 381 
    vTcl:DefineAlias "$site_3_0.cpd43" "Frame6" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    set site_4_0 $site_3_0.cpd43
    label $site_4_0.01 \
        -anchor w -relief groove \
        -text {Search range (in +/- units of error):} 
    vTcl:DefineAlias "$site_4_0.01" "Label4" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    entry $site_4_0.02 \
        -cursor {} -highlightthickness 0 -textvariable ::redcat::range 
    vTcl:DefineAlias "$site_4_0.02" "Entry4" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    pack $site_4_0.01 \
        -in $site_4_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_4_0.02 \
        -in $site_4_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $site_3_0.but45 \
        \
        -command {variable ::redcat::X
if { [array exists ::redcat::X] } {
    variable ::redcat::molNum
    
    ::redcat::clearRedcat $::redcat::molNum
    ::redcat::Run_SVD 2
    ::redcat::printRun
    ::redcat::visualizeRDC $::redcat::molNum
    
} else {
    ::redcat::dataLoadedError

}} \
        -text Run 
    vTcl:DefineAlias "$site_3_0.but45" "Button1" ::redcat::vTcl:WidgetProc "Toplevel1" 1
#    button $site_3_0.but35 \
        \
#        -command {set choice [tk_messageBox -type yesno -default no -message "Really quit the program?"]
#if {$choice == "yes"} {
#exit
#::redcat::hideAll
#}
#} \
#        -height 0 -text Quit -width 0 
#    vTcl:DefineAlias "$site_3_0.but35" "Button4" ::redcat::vTcl:WidgetProc "Toplevel1" 1
    place $site_3_0.cpd40 \
        -in $site_3_0 -x 2 -y 2 -width 380 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.cpd41 \
        -in $site_3_0 -x 2 -y 27 -width 380 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.cpd42 \
        -in $site_3_0 -x 2 -y 52 -width 380 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.cpd43 \
        -in $site_3_0 -x 2 -y 77 -width 381 -height 26 -anchor nw \
        -bordermode inside 
    place $site_3_0.but45 \
        -in $site_3_0 -x 485 -y 45 -anchor nw -bordermode ignore 
#    place $site_3_0.but35 \
#        -in $site_3_0 -x 485 -y 65 -anchor nw -bordermode ignore 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd38 \
        -in $top -x 0 -y 0 -relwidth 1 -relheight 0.7 -anchor nw \
        -bordermode inside 
    grid columnconf $top.cpd38 0 -weight 1
    grid rowconf $top.cpd38 0 -weight 1
    place $top.fra39 \
        -in $top -x 0 -y 0 -rely 1 -width 0 -relwidth 1 -height 0 \
        -relheight 0.3 -anchor sw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top33 {base} {
    if {$base == ""} {
        set base .top33
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 450x181+440+329; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Error!"
    vTcl:DefineAlias "$top" "Error" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    label $top.lab35 \
        -text label -textvariable ::redcat::Error_message 
    vTcl:DefineAlias "$top.lab35" "Label5" ::redcat::vTcl:WidgetProc "Error" 1
    button $top.but36 \
        -command {::redcat::Window hide .top33} -text OK 
    vTcl:DefineAlias "$top.but36" "Button2" ::redcat::vTcl:WidgetProc "Error" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.lab35 \
        -in $top -x 0 -y 0 -relwidth 1 -relheight 0.8 -anchor nw \
        -bordermode ignore 
    place $top.but36 \
        -in $top -x 205 -y 145 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top34 {base} {
    if {$base == ""} {
        set base .top34
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m54" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 283x183+522+367; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 0 0
    wm title $top "Options"
    vTcl:DefineAlias "$top" "Options" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    button $top.but35 \
        \
        -command {for {set i 1} { $i <= $::redcat::Count } { incr i} {
  set ::redcat::X(9,$i) $::redcat::ErrorGlobal
}
::redcat::Window hide .top34} \
        -text OK 
    vTcl:DefineAlias "$top.but35" "Button8" ::redcat::vTcl:WidgetProc "Options" 1
    frame $top.fra43 \
        -borderwidth 2 -relief groove -height 35 -width 290 
    vTcl:DefineAlias "$top.fra43" "Frame1" ::redcat::vTcl:WidgetProc "Options" 1
    set site_3_0 $top.fra43
    label $site_3_0.lab44 \
        -text {Plot type:} 
    vTcl:DefineAlias "$site_3_0.lab44" "Label1" ::redcat::vTcl:WidgetProc "Options" 1
    radiobutton $site_3_0.rad45 \
        -indicatoron 1 -takefocus {} -text PNG -value png -variable ::redcat::plot_type 
    vTcl:DefineAlias "$site_3_0.rad45" "Radiobutton1" ::redcat::vTcl:WidgetProc "Options" 1
    radiobutton $site_3_0.rad46 \
        -text Postscript -value ps -variable ::redcat::plot_type 
    vTcl:DefineAlias "$site_3_0.rad46" "Radiobutton2" ::redcat::vTcl:WidgetProc "Options" 1
    radiobutton $site_3_0.gifPlotRadBut \
        -text GIF -value gif -variable ::redcat::plot_type 
    vTcl:DefineAlias "$site_3_0.gifPlotRadBut" "Radiobutton4" vTcl:WidgetProc "Options" 1
    place $site_3_0.lab44 \
        -in $site_3_0 -x 5 -y 5 -anchor nw -bordermode ignore 
    place $site_3_0.rad45 \
        -in $site_3_0 -x 65 -y 5 -anchor nw -bordermode ignore 
    place $site_3_0.rad46 \
        -in $site_3_0 -x 120 -y 5 -anchor nw -bordermode ignore 
    place $site_3_0.gifPlotRadBut \
        -in $site_3_0 -x 210 -y 5 -anchor nw -bordermode ignore 
    frame $top.cpd48 \
        -borderwidth 1 -height 28 
    vTcl:DefineAlias "$top.cpd48" "Frame5" ::redcat::vTcl:WidgetProc "Options" 1
    set site_3_0 $top.cpd48
    label $site_3_0.01 \
        -anchor w -text {Error padding:} 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "Options" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::ErrorPad 
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "Options" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd47 \
        -borderwidth 1 -height 28 -highlightcolor black 
    vTcl:DefineAlias "$top.cpd47" "Frame6" ::redcat::vTcl:WidgetProc "Options" 1
    set site_3_0 $top.cpd47
    label $site_3_0.01 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -text {Set all errors to:} 
    vTcl:DefineAlias "$site_3_0.01" "Label3" ::redcat::vTcl:WidgetProc "Options" 1
    entry $site_3_0.02 \
        -cursor {} -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable ::redcat::ErrorGlobal -width 20 
    vTcl:DefineAlias "$site_3_0.02" "Entry2" ::redcat::vTcl:WidgetProc "Options" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd53 \
        -borderwidth 1 -height 28 -highlightcolor black 
    vTcl:DefineAlias "$top.cpd53" "Frame7" ::redcat::vTcl:WidgetProc "Options" 1
    set site_3_0 $top.cpd53
    entry $site_3_0.02 \
        -cursor fleur -foreground black -highlightcolor black \
        -insertbackground black -selectbackground #c4c4c4 \
        -selectforeground black -textvariable ::redcat::2DMapDatGlobal -width 8 
    vTcl:DefineAlias "$site_3_0.02" "Entry3" ::redcat::vTcl:WidgetProc "Options" 1
    bindtags $site_3_0.02 "$site_3_0.02 Entry $top all _TopLevel"
    label $site_3_0.01 \
        -activebackground #f9f9f9 -activeforeground black -anchor w \
        -foreground black -highlightcolor black -text {Set 2D map location:} 
    vTcl:DefineAlias "$site_3_0.01" "Label4" ::redcat::vTcl:WidgetProc "Options" 1
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 0 -fill x -padx 2 -pady 2 \
        -side right 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    menu $top.m54 \
        -tearoff 1 
    button $top.loadMapDat \
        -command {variable ::redcat::2DMapDatGlobal

set ::redcat::2DMapDatGlobal[tk_getOpenFile]} \
        -text Load 
    vTcl:DefineAlias "$top.loadMapDat" "Button1" ::redcat::vTcl:WidgetProc "Options" 1
    frame $top.cpd70 \
        -borderwidth 1 -height 28 
    vTcl:DefineAlias "$top.cpd70" "Frame11" ::redcat::vTcl:WidgetProc "Options" 1
    set site_3_0 $top.cpd70
    label $site_3_0.01 \
        -anchor w -text {Set 3D Map location:} 
    vTcl:DefineAlias "$site_3_0.01" "Label8" ::redcat::vTcl:WidgetProc "Options" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::3DMapDatGlobal
    vTcl:DefineAlias "$site_3_0.02" "Entry7" ::redcat::vTcl:WidgetProc "Options" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but72 \
        -command {variable ::redcat::3DMapDatGlobal

set ::redcat::3DMapDatGlobal[tk_getOpenFile]} \
        -text Load 
    vTcl:DefineAlias "$top.but72" "Button2" ::redcat::vTcl:WidgetProc "Options" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.but35 \
        -in $top -x 125 -y 145 -anchor nw -bordermode ignore 
    place $top.fra43 \
        -in $top -x -5 -y 0 -width 290 -height 35 -anchor nw \
        -bordermode ignore 
    place $top.cpd48 \
        -in $top -x 55 -y 40 -width 147 -height 28 -anchor nw \
        -bordermode ignore 
    place $top.cpd47 \
        -in $top -x 40 -y 65 -width 162 -height 28 -anchor nw \
        -bordermode inside 
    place $top.cpd53 \
        -in $top -x 15 -y 90 -width 204 -height 28 -anchor nw \
        -bordermode inside 
    place $top.loadMapDat \
        -in $top -x 220 -y 95 -width 56 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.cpd70 \
        -in $top -x 15 -y 115 -width 203 -height 28 -anchor nw \
        -bordermode inside 
    place $top.but72 \
        -in $top -x 220 -y 120 -width 56 -height 22 -anchor nw \
        -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top35 {base} {
    if {$base == ""} {
        set base .top35
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 261x126+531+538; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 0 0
    wm title $top "Rotate PDB"
    vTcl:DefineAlias "$top" "RotatePDB" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd36 \
        -borderwidth 1 -relief raised -height 26 -width 254 
    vTcl:DefineAlias "$top.cpd36" "Frame19" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    set site_3_0 $top.cpd36
    label $site_3_0.01 \
        -anchor w -relief groove -text {Input file name:} 
    vTcl:DefineAlias "$site_3_0.01" "Label14" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::fin 
    vTcl:DefineAlias "$site_3_0.02" "Entry13" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd37 \
        -borderwidth 1 -relief raised -height 26 -width 256 
    vTcl:DefineAlias "$top.cpd37" "Frame20" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    set site_3_0 $top.cpd37
    label $site_3_0.01 \
        -anchor w -relief groove -text {Output file name:} 
    vTcl:DefineAlias "$site_3_0.01" "Label15" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::fout -width 18 
    vTcl:DefineAlias "$site_3_0.02" "Entry14" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd38 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd38" "Frame21" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    set site_3_0 $top.cpd38
    label $site_3_0.01 \
        -anchor w -relief groove -text Alpha 
    vTcl:DefineAlias "$site_3_0.01" "Label16" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::a 
    vTcl:DefineAlias "$site_3_0.02" "Entry15" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd39 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd39" "Frame22" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    set site_3_0 $top.cpd39
    label $site_3_0.01 \
        -anchor w -relief groove -text Beta 
    vTcl:DefineAlias "$site_3_0.01" "Label17" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::b 
    vTcl:DefineAlias "$site_3_0.02" "Entry16" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd40 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd40" "Frame23" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    set site_3_0 $top.cpd40
    label $site_3_0.01 \
        -anchor w -relief groove -text Gamma 
    vTcl:DefineAlias "$site_3_0.01" "Label18" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::c 
    vTcl:DefineAlias "$site_3_0.02" "Entry17" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but41 \
        -command {exec Rotate_PDB.tcl $::redcat::fin $::redcat::fout $::redcat::a $::redcat::b $::redcat::c
::redcat::Window hide .top35} \
        -height 0 -text Rotate 
    vTcl:DefineAlias "$top.but41" "Button9" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    button $top.but42 \
        -command {::redcat::Window hide .top35} -height 0 -text Done -width 0 
    vTcl:DefineAlias "$top.but42" "Button10" ::redcat::vTcl:WidgetProc "RotatePDB" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd36 \
        -in $top -x 0 -y 0 -width 256 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.cpd37 \
        -in $top -x 0 -y 25 -width 256 -height 26 -anchor nw \
        -bordermode inside 
    place $top.cpd38 \
        -in $top -x 0 -y 50 -width 118 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.cpd39 \
        -in $top -x 0 -y 75 -width 118 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.cpd40 \
        -in $top -x 0 -y 100 -width 118 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.but41 \
        -in $top -x 120 -y 50 -anchor nw -bordermode ignore 
    place $top.but42 \
        -in $top -x 125 -y 80 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top36 {base} {
    if {$base == ""} {
        set base .top36
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m43" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 205x318+529+273; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Rotate Coordinates"
    vTcl:DefineAlias "$top" "RotateCoord" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    radiobutton $top.rad39 \
        -text {Euler rotate:} -value 1 -variable ::redcat::rotationType 
    vTcl:DefineAlias "$top.rad39" "Radiobutton1" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    radiobutton $top.rad40 \
        -command {} -text {Rotate about an axis:} -value 0 \
        -variable ::redcat::rotationType 
    vTcl:DefineAlias "$top.rad40" "Radiobutton2" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    frame $top.cpd41 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd41" "Frame24" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.cpd41
    label $site_3_0.01 \
        -anchor w -relief groove -text a 
    vTcl:DefineAlias "$site_3_0.01" "Label19" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::a \
        -width 5 
    vTcl:DefineAlias "$site_3_0.02" "Entry18" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra42 \
        -borderwidth 1 -relief raised -cursor fleur -height 30 -width 30 
    vTcl:DefineAlias "$top.fra42" "Frame25" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra42
    label $site_3_0.01 \
        -anchor w -relief groove -text b 
    vTcl:DefineAlias "$site_3_0.01" "Label20" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::b \
        -width 5 
    vTcl:DefineAlias "$site_3_0.02" "Entry19" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra43 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.fra43" "Frame26" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra43
    label $site_3_0.01 \
        -anchor w -relief groove -text c 
    vTcl:DefineAlias "$site_3_0.01" "Label21" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::c \
        -width 5 
    vTcl:DefineAlias "$site_3_0.02" "Entry20" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra46 \
        -borderwidth 1 -relief raised -height 26 -width 154 
    vTcl:DefineAlias "$top.fra46" "Frame27" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra46
    label $site_3_0.01 \
        -anchor w -relief groove -text {x coordinate:} 
    vTcl:DefineAlias "$site_3_0.01" "Label22" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::x 
    vTcl:DefineAlias "$site_3_0.02" "Entry21" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra47 \
        -borderwidth 1 -relief raised -height 26 -width 154 
    vTcl:DefineAlias "$top.fra47" "Frame29" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra47
    label $site_3_0.01 \
        -anchor w -relief groove -text {y coordinate:} 
    vTcl:DefineAlias "$site_3_0.01" "Label24" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::y 
    vTcl:DefineAlias "$site_3_0.02" "Entry23" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra48 \
        -borderwidth 1 -relief raised -height 26 -width 154 
    vTcl:DefineAlias "$top.fra48" "Frame30" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra48
    label $site_3_0.01 \
        -anchor w -relief groove -text {z coordinate:} 
    vTcl:DefineAlias "$site_3_0.01" "Label25" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 -textvariable ::redcat::z 
    vTcl:DefineAlias "$site_3_0.02" "Entry24" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra49 \
        -borderwidth 1 -relief raised -height 26 -width 154 
    vTcl:DefineAlias "$top.fra49" "Frame31" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    set site_3_0 $top.fra49
    label $site_3_0.01 \
        -anchor w -relief groove -text angle: 
    vTcl:DefineAlias "$site_3_0.01" "Label26" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::theta 
    vTcl:DefineAlias "$site_3_0.02" "Entry25" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but50 \
        \
        -command {variable ::redcat::molNum
                  variable ::redcat::a
                  variable ::redcat::b
                  variable ::redcat::c
                  if {$::redcat::rotationType == 1} {
                    ::redcat::redcatRotate $::redcat::molNum $::redcat::rotationType $::redcat::a $::redcat::b $::redcat::c 0
                  } else {
                    ::redcat::redcatRotate $::redcat::molNum $::redcat::rotationType $::redcat::x $::redcat::y $::redcat::z $::redcat::theta
        } 

::redcat::Window hide .top36} \
        -height 0 -text Rotate -width 0 
    vTcl:DefineAlias "$top.but50" "Button11" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    button $top.but51 \
        -command {::redcat::Window hide .top36} -text Done 
    vTcl:DefineAlias "$top.but51" "Button12" ::redcat::vTcl:WidgetProc "RotateCoord" 1
    menu $top.m43 \
        -tearoff 1 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.rad39 \
        -in $top -x 10 -y 15 -anchor nw -bordermode ignore 
    place $top.rad40 \
        -in $top -x 0 -y 120 -anchor nw -bordermode ignore 
    place $top.cpd41 \
        -in $top -x 40 -y 40 -anchor nw -bordermode inside 
    place $top.fra42 \
        -in $top -x 40 -y 65 -anchor nw -bordermode inside 
    place $top.fra43 \
        -in $top -x 40 -y 90 -anchor nw -bordermode inside 
    place $top.fra46 \
        -in $top -x 35 -y 145 -width 154 -height 26 -anchor nw \
        -bordermode inside 
    place $top.fra47 \
        -in $top -x 35 -y 170 -width 154 -height 26 -anchor nw \
        -bordermode inside 
    place $top.fra48 \
        -in $top -x 35 -y 195 -width 154 -height 26 -anchor nw \
        -bordermode inside 
    place $top.fra49 \
        -in $top -x 35 -y 220 -width 154 -height 26 -anchor nw \
        -bordermode inside 
    place $top.but50 \
        -in $top -x 35 -y 255 -anchor nw -bordermode ignore 
    place $top.but51 \
        -in $top -x 35 -y 285 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top37 {base} {
    if {$base == ""} {
        set base .top37
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m32" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 830x490+298+349; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Message!"
    vTcl:DefineAlias "$top" "Message" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd32 \
        -borderwidth 1 -relief raised -height 231 -width 311 
    vTcl:DefineAlias "$top.cpd32" "Frame7" ::redcat::vTcl:WidgetProc "Message" 1
    set site_3_0 $top.cpd32
    scrollbar $site_3_0.01 \
        -command "$site_3_0.03 xview" -orient horizontal 
    vTcl:DefineAlias "$site_3_0.01" "Scrollbar4" ::redcat::vTcl:WidgetProc "Message" 1
    scrollbar $site_3_0.02 \
        -command "$site_3_0.03 yview" 
    vTcl:DefineAlias "$site_3_0.02" "Scrollbar5" ::redcat::vTcl:WidgetProc "Message" 1
    text $site_3_0.03 \
        -font -Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-* -height 10 \
        -width 20 -xscrollcommand "$site_3_0.01 set" \
        -yscrollcommand "$site_3_0.02 set" 
    vTcl:DefineAlias "$site_3_0.03" "Text2" ::redcat::vTcl:WidgetProc "Message" 1
    grid $site_3_0.01 \
        -in $site_3_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_3_0.02 \
        -in $site_3_0 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    grid $site_3_0.03 \
        -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    frame $top.fra33 \
        -borderwidth 2 -relief groove -height 75 -width 125 
    vTcl:DefineAlias "$top.fra33" "Frame8" ::redcat::vTcl:WidgetProc "Message" 1
    set site_3_0 $top.fra33
    button $site_3_0.but34 \
        -command {::redcat::Window hide .top37} -text OK 
    vTcl:DefineAlias "$site_3_0.but34" "Button3" ::redcat::vTcl:WidgetProc "Message" 1
    button $site_3_0.but36 \
        -command {::redcat::Destroy_Tensor
.top37.cpd32.03 delete 1.0 end} \
        -text {Clear all} 
    vTcl:DefineAlias "$site_3_0.but36" "clearMessage" ::redcat::vTcl:WidgetProc "Message" 1
    place $site_3_0.but34 \
        -in $site_3_0 -x 0 -relx 0.3 -y 0 -rely 0.5 -anchor center \
        -bordermode ignore 
    place $site_3_0.but36 \
        -in $site_3_0 -x 0 -relx 0.6 -y 0 -rely 0.5 -anchor center \
        -bordermode ignore 
    menu $top.m32 \
        -tearoff 1 
    $top.m32 add command \
        \
        -command {# TODO: Your menu handler here
set fout [open [tk_getSaveFile] w]
foreach {key value index} [.top37.cpd32.03 dump 1.0 end] {
  if {$key == "text"} {
    puts -nonewline $fout $value
  }
}
close $fout} \
        -label Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd32 \
        -in $top -x 0 -y 0 -relwidth 1 -relheight 0.9 -anchor nw \
        -bordermode inside 
    grid columnconf $top.cpd32 0 -weight 1
    grid rowconf $top.cpd32 0 -weight 1
    place $top.fra33 \
        -in $top -x 0 -y 0 -rely 0.9 -relwidth 1 -relheight 0.1 -anchor nw \
        -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top38 {base} {
    if {$base == ""} {
        set base .top38
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 259x212+505+329; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 0 0
    wm title $top "Dynamic Averaging"
    vTcl:DefineAlias "$top" "DynamicAvg" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd39 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.cpd39" "Frame32" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.cpd39
    label $site_3_0.01 \
        -anchor w -relief groove -text {Input file name:} 
    vTcl:DefineAlias "$site_3_0.01" "Label27" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::fin 
    vTcl:DefineAlias "$site_3_0.02" "Entry26" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra40 \
        -borderwidth 1 -relief raised -height 26 -width 255 
    vTcl:DefineAlias "$top.fra40" "Frame33" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra40
    label $site_3_0.01 \
        -anchor w -relief groove -text {Output file name:} 
    vTcl:DefineAlias "$site_3_0.01" "Label28" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::fout 
    vTcl:DefineAlias "$site_3_0.02" "Entry27" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra41 \
        -borderwidth 1 -relief raised -height 26 -width 257 
    vTcl:DefineAlias "$top.fra41" "Frame34" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra41
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::fstate 
    vTcl:DefineAlias "$site_3_0.02" "Entry28" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    label $site_3_0.01 \
        -anchor w -relief groove -text {State file name: } -width 77 
    vTcl:DefineAlias "$site_3_0.01" "Label29" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    frame $top.fra42 \
        -borderwidth 1 -relief raised -height 26 -width 255 
    vTcl:DefineAlias "$top.fra42" "Frame35" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra42
    label $site_3_0.01 \
        -anchor w -relief groove -text {Number of states:} 
    vTcl:DefineAlias "$site_3_0.01" "Label30" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::numstate 
    vTcl:DefineAlias "$site_3_0.02" "Entry29" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra43 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.fra43" "Frame36" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra43
    label $site_3_0.01 \
        -anchor w -relief groove -text Sxx 
    vTcl:DefineAlias "$site_3_0.01" "Label31" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::Sxx 
    vTcl:DefineAlias "$site_3_0.02" "Entry30" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra44 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.fra44" "Frame37" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra44
    label $site_3_0.01 \
        -anchor w -relief groove -text Syy 
    vTcl:DefineAlias "$site_3_0.01" "Label32" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::Syy 
    vTcl:DefineAlias "$site_3_0.02" "Entry31" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.fra45 \
        -borderwidth 1 -relief raised -height 30 -width 30 
    vTcl:DefineAlias "$top.fra45" "Frame38" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    set site_3_0 $top.fra45
    label $site_3_0.01 \
        -anchor w -relief groove -text Szz 
    vTcl:DefineAlias "$site_3_0.01" "Label33" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    entry $site_3_0.02 \
        -background #ffffff -cursor {} -highlightthickness 0 \
        -textvariable ::redcat::Szz 
    vTcl:DefineAlias "$site_3_0.02" "Entry32" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but46 \
        -command {::redcat::Get_Dynamic $::redcat::fin $::redcat::fout $fstate $numstate $Sxx $Syy $Szz} \
        -height 0 -text Average -width 0 
    vTcl:DefineAlias "$top.but46" "Button13" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    button $top.but47 \
        -command {::redcat::Window hide .top38} -height 0 -text Done 
    vTcl:DefineAlias "$top.but47" "Button14" ::redcat::vTcl:WidgetProc "DynamicAvg" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd39 \
        -in $top -x 0 -y 0 -anchor nw -bordermode inside 
    place $top.fra40 \
        -in $top -x -1 -y 25 -width 255 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.fra41 \
        -in $top -x 0 -y 50 -width 254 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.fra42 \
        -in $top -x 0 -y 75 -width 254 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.fra43 \
        -in $top -x 0 -y 100 -width 254 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.fra44 \
        -in $top -x 0 -y 125 -width 254 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.fra45 \
        -in $top -x 0 -y 150 -width 253 -height 26 -anchor nw \
        -bordermode ignore 
    place $top.but46 \
        -in $top -x 35 -y 180 -anchor nw -bordermode ignore 
    place $top.but47 \
        -in $top -x 150 -y 180 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top53 {base} {
    if {$base == ""} {
        set base .top53
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m58" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 548x70+417+596; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 0 0
    wm title $top "Selection Window"
    vTcl:DefineAlias "$top" "Selection" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd54 \
        -borderwidth 1 -relief raised -height 28 
    vTcl:DefineAlias "$top.cpd54" "Frame5" ::redcat::vTcl:WidgetProc "Selection" 1
    set site_3_0 $top.cpd54
    label $site_3_0.01 \
        -anchor w -relief groove -text Selection: 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "Selection" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::Selection 
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "Selection" 1
    bind $site_3_0.02 <Key-Return> {
        ::redcat::Parse_Selection $::redcat::Selection
    }
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but55 \
        -command {::redcat::Parse_Selection $::redcat::Selection} -disabledforeground #a2a2a2 \
        -text Select 
    vTcl:DefineAlias "$top.but55" "Button1" ::redcat::vTcl:WidgetProc "Selection" 1
    button $top.but57 \
        -command {::redcat::Window hide .top53} -disabledforeground #a2a2a2 -text Done 
    vTcl:DefineAlias "$top.but57" "Button2" ::redcat::vTcl:WidgetProc "Selection" 1
    menu $top.m58 \
        -disabledforeground #a2a2a2 -tearoff 1 
    button $top.but48 \
        \
        -command {set msg "1,10,15-25 will select entries 1, 10 and 15 through 25.\n! will negate Selection\n* will select everything.\n~ will unselect entries with any 999" 
set choice [tk_messageBox -type ok -message $msg]} \
        -text Example 
    vTcl:DefineAlias "$top.but48" "Button3" ::redcat::vTcl:WidgetProc "Selection" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd54 \
        -in $top -x 0 -y 0 -width 543 -height 28 -anchor nw \
        -bordermode inside 
    place $top.but55 \
        -in $top -x 5 -y 35 -anchor nw -bordermode ignore 
    place $top.but57 \
        -in $top -x 95 -y 35 -anchor nw -bordermode ignore 
    place $top.but48 \
        -in $top -x 475 -y 35 -width 68 -height 28 -anchor nw \
        -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top56 {base} {
    if {$base == ""} {
        set base .top56
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m56" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 550x275+364+367; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Prepare Input"
    vTcl:DefineAlias "$top" "PrepInput" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd57 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd57" "Frame5" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd57
    label $site_3_0.01 \
        -anchor w -text {VMD molecule ID:} 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::molNum
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd58 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd58" "Frame6" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd58
    label $site_3_0.01 \
        -anchor w -text {RDC file (optional):} 
    vTcl:DefineAlias "$site_3_0.01" "Label3" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::rdcfile 
    vTcl:DefineAlias "$site_3_0.02" "Entry2" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd59 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd59" "Frame7" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd59
    label $site_3_0.01 \
        -anchor w -text {Start at residue:} 
    vTcl:DefineAlias "$site_3_0.01" "Label4" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::startres 
    vTcl:DefineAlias "$site_3_0.02" "Entry3" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd60 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd60" "Frame8" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd60
    label $site_3_0.01 \
        -anchor w -text {Stop at residue:} 
    vTcl:DefineAlias "$site_3_0.01" "Label5" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::stopres 
    vTcl:DefineAlias "$site_3_0.02" "Entry4" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd61 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd61" "Frame9" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd61
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label6" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(1,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry5" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd62 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd62" "Frame10" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd62
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label7" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(1,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry6" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd63 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd63" "Frame11" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd63
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label8" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(1,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry7" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd64 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd64" "Frame12" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd64
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label9" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(1,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry8" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd65 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd65" "Frame13" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd65
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label10" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(2,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry9" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd66 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd66" "Frame14" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd66
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label11" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(2,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry10" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd67 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd67" "Frame15" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd67
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label12" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(2,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry11" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd68 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd68" "Frame16" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd68
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label13" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(2,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry12" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd69 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd69" "Frame17" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd69
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label14" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(3,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry13" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd70 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd70" "Frame18" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd70
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label15" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(3,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry14" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd71 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd71" "Frame19" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd71
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label16" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(3,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry15" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd72 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd72" "Frame20" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd72
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label17" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(3,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry16" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd73 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd73" "Frame21" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd73
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label18" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(4,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry17" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd74 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd74" "Frame22" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd74
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label19" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(4,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry18" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd75 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd75" "Frame23" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd75
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label20" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(4,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry19" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd76 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd76" "Frame24" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd76
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label21" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(4,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry20" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd77 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd77" "Frame25" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd77
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label22" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(5,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry21" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd78 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd78" "Frame26" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd78
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label23" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(5,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry22" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd79 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd79" "Frame27" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd79
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label24" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(5,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry23" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd80 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd80" "Frame28" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd80
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label25" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(5,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry24" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd81 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd81" "Frame29" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd81
    label $site_3_0.01 \
        -anchor w -text {Atom 1:} 
    vTcl:DefineAlias "$site_3_0.01" "Label26" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(6,1) 
    vTcl:DefineAlias "$site_3_0.02" "Entry25" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd82 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd82" "Frame30" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd82
    label $site_3_0.01 \
        -anchor w -text {Atom 2:} 
    vTcl:DefineAlias "$site_3_0.01" "Label27" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(6,2) 
    vTcl:DefineAlias "$site_3_0.02" "Entry26" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd83 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd83" "Frame31" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd83
    label $site_3_0.01 \
        -anchor w -text Gap: 
    vTcl:DefineAlias "$site_3_0.01" "Label28" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(6,3) 
    vTcl:DefineAlias "$site_3_0.02" "Entry27" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd84 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd84" "Frame32" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd84
    label $site_3_0.01 \
        -anchor w -text {Max RDC:} 
    vTcl:DefineAlias "$site_3_0.01" "Label29" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(6,4) 
    vTcl:DefineAlias "$site_3_0.02" "Entry28" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but85 \
        \
        -command {
#set fin [open $::redcat::pdbfile r]
#set fout [open $::redcat::redcatfile w]
#for {set i $::redcat::startres} {$i <= $::redcat::stopres} {incr i} {
#  for {set j 1} {$j <= 6} {incr j} {
#     if {[string compare $::redcat::atom($j,1) ""]} {
#        set line [::redcat::Get_REDCAT_Line $::redcat::atom($j,1) $::redcat::atom($j,2) $i $::redcat::atom($j,3) $fin]
#        puts $fout "$line 999 $::redcat::atom($j,4) $::redcat::atom($j,5) /* $::redcat::atom($j,1)-$::redcat::atom($j,2) from $i */"
#        seek $fin 0 start
#     }
#  }
#}
#close $fin
#close $fout
#tk_messageBox -type ok -message "Done creating the file $::redcat::redcatfile"
	variable ::redcat::X
	variable ::redcat::Sel
	variable ::redcat::Count
	variable ::redcat::loaded
	
	if {$::redcat::loaded >= 0} {
		tk_messageBox -type ok -title "Data Error" -message "Please close the current file before loading a new one."
		return
		
	}
	
	set ba_t .top32.cpd38.03
	set ba_f $ba_t.fra0
	frame $ba_f
	
	label $ba_f.lab1 -text  "Eq.#  "
	label $ba_f.lab2 -text  "    "
	label $ba_f.lab3 -text  "X1       "
	label $ba_f.lab4 -text  "Y1       "
	label $ba_f.lab5 -text  "Z1       "
	label $ba_f.lab6 -text  "X2       "
	label $ba_f.lab7 -text  "Y2       "
	label $ba_f.lab8 -text  "Z2       "
	label $ba_f.lab9 -text  "Dipol 1  "
	label $ba_f.lab10 -text "Error    "
	label $ba_f.lab11 -text "Comments "
	grid $ba_f.lab1 $ba_f.lab2 $ba_f.lab3 $ba_f.lab4 $ba_f.lab5  $ba_f.lab6   $ba_f.lab7 $ba_f.lab8 $ba_f.lab9 $ba_f.lab10 $ba_f.lab11
	$ba_t window create end -window $ba_f
	set c 0
	for {set i $::redcat::startres} {$i <= $::redcat::stopres} {incr i} {
		for {set j 1} {$j < 7} {incr j} {
			if {$::redcat::atom($j,1) != "" && $::redcat::atom($j,2) != ""} {
				incr c
				set atomCoord(0) [::redcat::selectAtom $::redcat::molNum $::redcat::atom($j,1) $i]
				set atomCoord(1) [::redcat::selectAtom $::redcat::molNum $::redcat::atom($j,2) [expr $i+$::redcat::atom($j,3)]]
				#puts "\[$atomCoord(0)\]\n\[$atomCoord(1)\]"
				for {set p 0} {$p<2} {incr p} {
					if {![string compare $atomCoord($p) ""]} {
						set atomCoord($p) "999 999 999"
						
					} else {
						regsub {\{} $atomCoord($p) "" atomCoord($p)
						regsub {\}} $atomCoord($p) "" atomCoord($p)
						
					}
						
				}
				incr ::redcat::Count
				set atomArray1 [split $atomCoord(0) " "]
				set atomArray2 [split $atomCoord(1) " "]
				set ::redcat::X(1,$c) [lindex $atomArray1 0]
				set ::redcat::X(2,$c) [lindex $atomArray1 1]
				set ::redcat::X(3,$c) [lindex $atomArray1 2]
				set ::redcat::X(4,$c) [lindex $atomArray2 0]
				set ::redcat::X(5,$c) [lindex $atomArray2 1]
				set ::redcat::X(6,$c) [lindex $atomArray2 2]
				set ::redcat::X(7,$c) 999
				set ::redcat::X(8,$c) $::redcat::atom($j,4)
				set ::redcat::X(9,$c) $::redcat::atom($j,5)
				set ::redcat::X(10,$c) "/* $::redcat::atom($j,1)-$::redcat::atom($j,2) from $i */"
				
				set ba_f $ba_t.fra$c
				frame $ba_f
 
				label $ba_f.ln$c -text "$c) "
				if {$::redcat::X(1,$c) == 999 || $::redcat::X(2,$c) == 999 || $::redcat::X(3,$c) == 999 || $::redcat::X(4,$c) == 999 || $::redcat::X(5,$c) == 999 || $::redcat::X(6,$c) == 999} {
					set ::redcat::Sel($c) 0
					
				} else {
					set ::redcat::Sel($c) 1
					
				}
				checkbutton $ba_f.rb$c -variable ::redcat::Sel($c) -selectcolor #ff0c04
				
				entry $ba_f.x1$c -textvariable ::redcat::X(1,$c) -width 6
				entry $ba_f.y1$c -textvariable ::redcat::X(2,$c) -width 6
				entry $ba_f.z1$c -textvariable ::redcat::X(3,$c) -width 6
				entry $ba_f.x2$c -textvariable ::redcat::X(4,$c) -width 6
				entry $ba_f.y2$c -textvariable ::redcat::X(5,$c) -width 6
				entry $ba_f.z2$c -textvariable ::redcat::X(6,$c) -width 6
				entry $ba_f.d$c -textvariable ::redcat::X(7,$c) -width 6
				entry $ba_f.e$c -textvariable ::redcat::X(9,$c) -width 6
				entry $ba_f.c$c -textvariable ::redcat::X(10,$c) -width 15
				
				
				grid $ba_f.ln$c $ba_f.rb$c $ba_f.x1$c $ba_f.y1$c  $ba_f.z1$c $ba_f.x2$c  $ba_f.y2$c  $ba_f.z2$c $ba_f.d$c  $ba_f.e$c $ba_f.c$c
					$ba_t window create end -window $ba_f
				
			}
			
		}
		
	}
	set ::redcat::Count $c
	if {$::redcat::rdcfile != ""} {
		set fin [open $::redcat::rdcfile r]
		set i 0

		while {[gets $fin oline] >= 0} {
			if {![regexp {^#} $oline]} {
				incr i
				if {$i <= $::redcat::Count} {
					set num_subs [regsub -all "\t" $oline " " line]
					set data [split $line " "]
					if { [llength $data] > 1 } {
						set ::redcat::X(7,$i) [lindex $data 0]
			          	set ::redcat::X(9,$i) [lindex $data 1]
						if {$::redcat::X(7,$i) == 999} {
							set ::redcat::Sel($i) 0
							
			          	}
			          	
			       	} else {
			          	set ::redcat::X(7,$i) [lindex $data 0]    
						if {$::redcat::X(7,$i) == 999} {
							set ::redcat::Sel($i) 0
							
						}
						
			       	}
			       	
				}
				
			}
			
		}
		close $fin
	
	}
	set ::redcat::loaded 1
	tk_messageBox -type ok -message "Done creating the Loading Redcat file"

} \
        -text Run 
    vTcl:DefineAlias "$top.but85" "Button1" ::redcat::vTcl:WidgetProc "PrepInput" 1
    button $top.but86 \
        -command {::redcat::Window hide .top56} -text Done 
    vTcl:DefineAlias "$top.but86" "Button2" ::redcat::vTcl:WidgetProc "PrepInput" 1
    frame $top.cpd85 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd85" "Frame33" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd85
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label30" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(1,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry29" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd87 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd87" "Frame34" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd87
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label31" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(2,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry30" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd88 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd88" "Frame35" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd88
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label32" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(3,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry31" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd89 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd89" "Frame36" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd89
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label33" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(4,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry32" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd90 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd90" "Frame37" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd90
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label34" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(5,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry33" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    frame $top.cpd91 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd91" "Frame38" ::redcat::vTcl:WidgetProc "PrepInput" 1
    set site_3_0 $top.cpd91
    label $site_3_0.01 \
        -anchor w -text Error: 
    vTcl:DefineAlias "$site_3_0.01" "Label35" ::redcat::vTcl:WidgetProc "PrepInput" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::atom(6,5) 
    vTcl:DefineAlias "$site_3_0.02" "Entry34" ::redcat::vTcl:WidgetProc "PrepInput" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but43 \
        \
        -command {set msg "Gap: Number of residues apart\nMax RDCs:\nH-H -240200\nN-H 24350\nC-N 6125\nC-C -15200\nC-H -60400" 
set choice [tk_messageBox -type ok -message $msg]} \
        -text Help 
    vTcl:DefineAlias "$top.but43" "Button3" ::redcat::vTcl:WidgetProc "PrepInput" 1
    menu $top.m56 \
        -tearoff 1 
    $top.m56 add cascade \
        -menu "$top.m56.men60" -command {} -label File 
    set site_3_0 $top.m56
    menu $site_3_0.men60 \
        -tearoff 0 
    $site_3_0.men60 add command \
        \
        -command {variable ::redcat::atom

set fileName [tk_getOpenFile]
if { $fileName == "" } { return }
set fin [open $fileName r]
set i 1

while {[gets $fin oline] >= 0} {
 if {![regexp {^#} $oline]} {
    set num_subs [regsub -all "\t" $oline " " line]
    set data [split $line " "]
    set ::redcat::atom($i,1) [lindex $data 0]
    set ::redcat::atom($i,2) [lindex $data 1]
    set ::redcat::atom($i,3) [lindex $data 2]
    set ::redcat::atom($i,4) [lindex $data 3]
    set ::redcat::atom($i,5) [lindex $data 4]
    incr i
 }
}    

close $fin} \
        -label {Load Schema} 
    $site_3_0.men60 add command \
        \
        -command {variable ::redcat::atom

set fileName [tk_getSaveFile]
if { $fileName == "" } { return }
set fin [open $fileName w]
set i 1

while { ($i < 7) && ($::redcat::atom($i,1) != "")} {
 puts $fin "$::redcat::atom($i,1) $::redcat::atom($i,2) $::redcat::atom($i,3) $::redcat::atom($i,4) $::redcat::atom($i,5)"
 incr i
}    

close $fin} \
        -label {Save Schema} 
	button $top.cpd86 \
        -command {variable ::redcat::rdcfile
set ::redcat::rdcfile [tk_getOpenFile]} \
        -text Load 
    vTcl:DefineAlias "$top.cpd86" "Button4" vTcl:WidgetProc "PrepInput" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd57 \
        -in $top -x 0 -y 5 -width 220 -anchor nw -bordermode inside 
    place $top.cpd58 \
        -in $top -x 230 -y 5 -width 220 -anchor nw -bordermode inside 
    place $top.cpd59 \
        -in $top -x 0 -y 30 -width 150 -anchor nw -bordermode inside 
    place $top.cpd60 \
        -in $top -x 230 -y 30 -width 150 -anchor nw -bordermode inside 
    place $top.cpd61 \
        -in $top -x 15 -y 70 -width 95 -anchor nw -bordermode inside 
    place $top.cpd62 \
        -in $top -x 115 -y 70 -width 95 -anchor nw -bordermode inside 
    place $top.cpd63 \
        -in $top -x 210 -y 70 -width 110 -anchor nw -bordermode inside 
    place $top.cpd64 \
        -in $top -x 325 -y 70 -width 130 -anchor nw -bordermode inside 
    place $top.cpd65 \
        -in $top -x 15 -y 95 -width 95 -anchor nw -bordermode inside 
    place $top.cpd66 \
        -in $top -x 115 -y 95 -width 95 -anchor nw -bordermode inside 
    place $top.cpd67 \
        -in $top -x 210 -y 95 -width 110 -anchor nw -bordermode inside 
    place $top.cpd68 \
        -in $top -x 325 -y 95 -width 130 -anchor nw -bordermode inside 
    place $top.cpd69 \
        -in $top -x 15 -y 120 -width 95 -anchor nw -bordermode inside 
    place $top.cpd70 \
        -in $top -x 115 -y 120 -width 95 -anchor nw -bordermode inside 
    place $top.cpd71 \
        -in $top -x 210 -y 120 -width 110 -anchor nw -bordermode inside 
    place $top.cpd72 \
        -in $top -x 325 -y 120 -width 130 -anchor nw -bordermode inside 
    place $top.cpd73 \
        -in $top -x 15 -y 145 -width 95 -anchor nw -bordermode inside 
    place $top.cpd74 \
        -in $top -x 115 -y 145 -width 95 -anchor nw -bordermode inside 
    place $top.cpd75 \
        -in $top -x 210 -y 145 -width 110 -anchor nw -bordermode inside 
    place $top.cpd76 \
        -in $top -x 325 -y 145 -width 130 -anchor nw -bordermode inside 
    place $top.cpd77 \
        -in $top -x 15 -y 170 -width 95 -anchor nw -bordermode inside 
    place $top.cpd78 \
        -in $top -x 115 -y 170 -width 95 -anchor nw -bordermode inside 
    place $top.cpd79 \
        -in $top -x 210 -y 170 -width 110 -anchor nw -bordermode inside 
    place $top.cpd80 \
        -in $top -x 325 -y 170 -width 130 -anchor nw -bordermode inside 
    place $top.cpd81 \
        -in $top -x 15 -y 195 -width 95 -anchor nw -bordermode inside 
    place $top.cpd82 \
        -in $top -x 115 -y 195 -width 95 -anchor nw -bordermode inside 
    place $top.cpd83 \
        -in $top -x 210 -y 195 -width 110 -anchor nw -bordermode inside 
    place $top.cpd84 \
        -in $top -x 325 -y 195 -width 130 -anchor nw -bordermode inside 
    place $top.but85 \
        -in $top -x 145 -y 235 -anchor nw -bordermode ignore 
    place $top.but86 \
        -in $top -x 275 -y 235 -anchor nw -bordermode ignore 
    place $top.cpd85 \
        -in $top -x 455 -y 70 -width 80 -anchor nw -bordermode inside 
    place $top.cpd87 \
        -in $top -x 455 -y 95 -width 80 -anchor nw -bordermode inside 
    place $top.cpd88 \
        -in $top -x 455 -y 120 -width 80 -anchor nw -bordermode inside 
    place $top.cpd89 \
        -in $top -x 455 -y 145 -width 80 -anchor nw -bordermode inside 
    place $top.cpd90 \
        -in $top -x 455 -y 170 -width 80 -anchor nw -bordermode inside 
    place $top.cpd91 \
        -in $top -x 455 -y 195 -width 80 -anchor nw -bordermode inside 
    place $top.but43 \
        -in $top -x 390 -y 235 -anchor nw -bordermode ignore 
    place $top.cpd86 \
        -in $top -x 455 -y 10 -width 54 -height 21 -anchor nw \
        -bordermode inside 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top61 {base} {
    if {$base == ""} {
        set base .top61
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m66" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 350x115+487+513; update
    wm maxsize $top 2545 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "2D SF Plot"
    vTcl:DefineAlias "$top" "2dPlotWin" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    radiobutton $top.rad62 \
        -text {Plot Best} -value 0 -variable ::redcat::plotBool 
    vTcl:DefineAlias "$top.rad62" "Radiobutton1" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    radiobutton $top.rad63 \
        -text {Plot All} -value 1 -variable ::redcat::plotBool 
    vTcl:DefineAlias "$top.rad63" "Radiobutton2" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    button $top.but64 \
        \
        -command {variable ::redcat::2DMapDatGlobal
variable ::redcat::plot2DFile

if { $::redcat::2DMapDatGlobal== "" } {
    set 2dmap "map.dat"
    
} else {
    set 2dmap $::redcat::2DMapDatGlobal
    
}
if { $::redcat::plot2DFile == "" } { 
    set ::redcat::plot2DFile [open "|gnuplot" w+]
    flush $::redcat::plot2DFile
    fconfigure $::redcat::plot2DFile -buffering none
    
}
puts $::redcat::plot2DFile "set mouse"
#puts $::redcat::plot2DFile "unset key"
puts $::redcat::plot2DFile "plot '$2dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"

::redcat::plot2D $::redcat::plotBool $::redcat::plot2DFile} \
        -text Plot 
    vTcl:DefineAlias "$top.but64" "Plot2DButton" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    button $top.but65 \
        \
        -command {variable ::redcat::plot2DFile

catch { close $::redcat::plot2DFile } result
set ::redcat::plot2DFile ""
::redcat::Window hide .top61} \
        -text Done 
    vTcl:DefineAlias "$top.but65" "Button2" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    menu $top.m66 \
        -tearoff 1 
    $top.m66 add command \
        -command ::redcat::save2DPlot -label Save 
    frame $top.cpd73 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd73" "Frame5" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    set site_3_0 $top.cpd73
    label $site_3_0.01 \
        -anchor w -text Command: 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    entry $site_3_0.02 \
        -cursor fleur -textvariable ::redcat::2DCommand 
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    bind $site_3_0.02 <Key-Return> {
        variable ::redcat::plot2DFile

puts $::redcat::plot2DFile "$::redcat::2DCommand"
set ::redcat::2DCommand ""
Plot2DButton invoke
    }
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but60 \
        \
        -command {variable ::redcat::2DMapDatGlobal
variable ::redcat::plot_type
set fout [tk_getSaveFile]
if { $fout == "" } {
    ::redcat::errorDialog "Save Name Error" "Please enter a file name to save the plot."
    return
    
} else {
    if { $::redcat::2DMapDatGlobal == "" } {
        set 2dmap "map.dat"
    
    } else {
        set 2dmap $::redcat::2DMapDatGlobal
    
    }
    set plotImgFile [open "|gnuplot" w+]
    flush $plotImgFile
    fconfigure $plotImgFile -buffering none
    set convertCheck [exec which convert]
    if {$convertCheck == "" || $::redcat::plot_type=="ps"} {
        puts $plotImgFile "set term postscript color; set output '$fout';"
        puts $plotImgFile "set mouse"
        #puts $plotImgFile "unset key"
        puts $plotImgFile "plot '$2dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
        ::redcat::plot2D $::redcat::plotBool $plotImgFile
        catch { close $plotImgFile } result
        
    } else {
        puts $plotImgFile "set term pbm color; set output '$fout';"
        puts $plotImgFile "set mouse"
        #puts $plotImgFile "unset key"
        puts $plotImgFile "plot '$2dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
        ::redcat::plot2D $::redcat::plotBool $plotImgFile
        catch { close $plotImgFile } result
        if {$::redcat::plot_type=="gif"} {
	        append result [exec convert $fout temp.gif]
     	   append result [exec mv temp.gif $fout]
    	   } else {
             append result [exec convert $fout temp.png]
             append result [exec mv temp.png $fout]
    	   }
    }
    return result
    
}} \
        -text {Capture Image} 
    vTcl:DefineAlias "$top.but60" "Button3" ::redcat::vTcl:WidgetProc "2dPlotWin" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.rad62 \
        -in $top -x 60 -y 15 -anchor nw -bordermode ignore 
    place $top.rad63 \
        -in $top -x 195 -y 15 -anchor nw -bordermode ignore 
    place $top.but64 \
        -in $top -x 55 -y 75 -anchor nw -bordermode ignore 
    place $top.but65 \
        -in $top -x 228 -y 75 -anchor nw -bordermode ignore 
    place $top.cpd73 \
        -in $top -x 50 -y 40 -anchor nw -bordermode inside 
    place $top.but60 \
        -in $top -x 110 -y 75 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top62 {base} {
    if {$base == ""} {
        set base .top62
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m75" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 350x115+531+498; update
    wm maxsize $top 2545 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "3D SF Plot"
    vTcl:DefineAlias "$top" "Toplevel3" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    radiobutton $top.cpd63 \
        -text {Plot Best} -value 0 -variable ::redcat::plotBool3D 
    vTcl:DefineAlias "$top.cpd63" "Radiobutton1" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    radiobutton $top.cpd64 \
        -text {Plot All} -value 1 -variable ::redcat::plotBool3D 
    vTcl:DefineAlias "$top.cpd64" "Radiobutton2" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    button $top.cpd65 \
        \
        -command {variable ::redcat::3DMapDatGlobal
variable ::redcat::plot3DFile
variable ::redcat::3DCommand

if { $::redcat::3DMapDatGlobal== "" } {
    set 3dmap "map3D.dat"
    
} else {
    set 3dmap $::redcat::3DMapDatGlobal
    
}
if { $::redcat::plot3DFile == "" } { 
    set ::redcat::plot3DFile [open "|gnuplot" w+]
    flush $::redcat::plot3DFile
    fconfigure $::redcat::plot3DFile -buffering none
    
}
puts $::redcat::plot3DFile "set mouse"
#puts $::redcat::plot3DFile "unset key"
if {$::redcat::3DCommand == {}} {
    puts $::redcat::plot3DFile "set view 90,0"

}
puts $::redcat::plot3DFile "splot '$3dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"

::redcat::plot3D $::redcat::plotBool3D $::redcat::plot3DFile} \
        -text Plot 
    vTcl:DefineAlias "$top.cpd65" "Plot3DButton" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    button $top.cpd66 \
        \
        -command {variable ::redcat::plot3DFile

catch { close $::redcat::plot3DFile } result
set ::redcat::plot3DFile ""
::redcat::Window hide .top62} \
        -text Done 
    vTcl:DefineAlias "$top.cpd66" "Button2" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    frame $top.cpd74 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd74" "Frame5" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    set site_3_0 $top.cpd74
    label $site_3_0.01 \
        -anchor w -text Command: 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::3DCommand 
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    bind $site_3_0.02 <Key-Return> {
        variable ::redcat::plot3DFile
variable ::redcat::3DCOmmand

puts $::redcat::plot3DFile "$::redcat::3DCommand"
Plot3DButton invoke
set ::redcat::3DCommand {}
    }
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    menu $top.m75 \
        -tearoff 1 
    $top.m75 add command \
        -command ::redcat::save3DPlot -label Save 
    button $top.but60 \
        \
        -command {global ::redcat::3DMapDatGlobal
variable ::redcat::plot_type
set fout [tk_getSaveFile]
if { $fout == "" } {
    ::redcat::errorDialog "Save Name Error" "Please enter a file name to use save the plot."
    return
    
} else {
    if { $::redcat::3DMapDatGlobal == "" } {
        set 3dmap "map.dat"
    
    } else {
        set 3dmap $::redcat::3DMapDatGlobal
    
    }
    set plotImgFile [open "|gnuplot" w+]
    flush $plotImgFile
    fconfigure $plotImgFile -buffering none
        set convertCheck [exec which convert]
    if {$convertCheck == "" || $::redcat::plot_type=="ps"} {
        puts $plotImgFile "set term postscript color; set output '$fout';"
        puts $plotImgFile "set mouse"
        #puts $plotImgFile "unset key"
        puts $plotImgFile "splot '$3dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
        ::redcat::plot3D $::redcat::plotBool3D $plotImgFile
        catch { close $plotImgFile } result
        
    } else {
        puts $plotImgFile "set term pbm color; set output '$fout';"
        puts $plotImgFile "set mouse"
        #puts $plotImgFile "unset key"
        puts $plotImgFile "splot '$3dmap' notitle w d lt -1, '-' title 'Sxx', '-' title 'Syy', '-' title 'Szz'"
        ::redcat::plot3D $::redcat::plotBool3D $plotImgFile
        catch { close $plotImgFile } result
        if {$::redcat::plot_type=="gif"} {
	        append result [exec convert $fout temp.gif]
     	   append result [exec mv temp.gif $fout]
    	   } else {
             append result [exec convert $fout temp.png]
             append result [exec mv temp.png $fout]
    	   }
    }
    return result
    
}} \
        -text {Capture Image} 
    vTcl:DefineAlias "$top.but60" "Button3" ::redcat::vTcl:WidgetProc "Toplevel3" 1
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd63 \
        -in $top -x 60 -y 15 -anchor nw -bordermode inside 
    place $top.cpd64 \
        -in $top -x 195 -y 15 -anchor nw -bordermode inside 
    place $top.cpd65 \
        -in $top -x 55 -y 75 -anchor nw -bordermode inside 
    place $top.cpd66 \
        -in $top -x 228 -y 75 -anchor nw -bordermode inside 
    place $top.cpd74 \
        -in $top -x 50 -y 40 -anchor nw -bordermode inside 
    place $top.but60 \
        -in $top -x 110 -y 75 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

proc ::redcat::vTclWindow.top63 {base} {
    if {$base == ""} {
        set base .top63
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    ::redcat::vTcl:toplevel $top -class Toplevel \
        -menu "$top.m70" -highlightcolor black 
    wm withdraw $top
    wm focusmodel $top passive
    wm geometry $top 350x88+482+421; update
    wm maxsize $top 2545 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm title $top "Error Plot"
    vTcl:DefineAlias "$top" "errorPlotWin" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    ::redcat::vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "::redcat::vTcl:FireEvent $top <<DeleteWindow>>"

    frame $top.cpd66 \
        -borderwidth 1 -height 30 
    vTcl:DefineAlias "$top.cpd66" "Frame5" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    set site_3_0 $top.cpd66
    label $site_3_0.01 \
        -anchor w -text Command: 
    vTcl:DefineAlias "$site_3_0.01" "Label2" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable ::redcat::errorCommand 
    vTcl:DefineAlias "$site_3_0.02" "Entry1" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    button $top.but67 \
        -command {::redcat::plotError errorCommand} -text Plot 
    vTcl:DefineAlias "$top.but67" "Button1" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    button $top.but68 \
        \
        -command {set fout [tk_getSaveFile]
if { $fout == "" } {
    ::redcat::errorDialog "Save Name Error" "Please enter a valid name to save the plot."
    
} else {
    set convertCheck [exec which convert]
    if {$convertCheck == "" || $::redcat::plot_type=="ps"} {
        plotError "set term postscript color; set output '$fout';"
        
    } else {
        plotError "set term pbm color; set output 'temp.pbm';"
        if {$::redcat::plot_type=="gif"} {
	        append result [exec convert $fout temp.gif]
     	   append result [exec mv temp.gif $fout]
    	   } else {
             append result [exec convert $fout temp.png]
             append result [exec mv temp.png $fout]
    	   }
    }
    
}} \
        -text {Capture Image} 
    vTcl:DefineAlias "$top.but68" "Button2" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    button $top.but69 \
        -command {::redcat::Window hide .top63} -text Done 
    vTcl:DefineAlias "$top.but69" "Button3" ::redcat::vTcl:WidgetProc "errorPlotWin" 1
    menu $top.m70 \
        -tearoff 1 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.cpd66 \
        -in $top -x 50 -y 10 -anchor nw -bordermode inside 
    place $top.but67 \
        -in $top -x 55 -y 50 -anchor nw -bordermode ignore 
    place $top.but68 \
        -in $top -x 110 -y 50 -anchor nw -bordermode ignore 
    place $top.but69 \
        -in $top -x 228 -y 50 -anchor nw -bordermode ignore 

    ::redcat::vTcl:FireEvent $base <<Ready>>
}

#############################################################################
## Binding tag:  _TopLevel

bind "_TopLevel" <<Create>> {
    if {![info exists _topcount]} {set _topcount 0}; incr _topcount
    lappend ::redcat::windowList %W
}
bind "_TopLevel" <<DeleteWindow>> {
	set wN %W
	if { $wN == ".top32" } {
		::redcat::hideAll
		::redcat::Window hide $wN
		
	} else {
		::redcat::Window hide %W
		
	}
	
}
bind "_TopLevel" <Destroy> {
    if {[winfo toplevel ::redcat::%W] == "%W"} {incr _topcount -1}
}
#############################################################################
## Binding tag:  _vTclBalloon


if {![info exists ::redcat::vTcl(sourcing)]} {
bind "_vTclBalloon" <<KillBalloon>> {
    namespace eval ::redcat::vTcl::balloon {
        after cancel $id
        if {[winfo exists .vTcl.balloon]} {
            destroy .vTcl.balloon
        }
        set set 0
    }
}
bind "_vTclBalloon" <<vTclBalloon>> {
    if {$::redcat::vTcl::balloon::first != 1} {break}

    namespace eval ::redcat::vTcl::balloon {
        set first 2
        if {![winfo exists .vTcl]} {
            toplevel .vTcl; wm withdraw .vTcl
        }
        if {![winfo exists .vTcl.balloon]} {
            toplevel .vTcl.balloon -bg black
        }
        wm overrideredirect .vTcl.balloon 1
        label .vTcl.balloon.l  -text ${%W} -relief flat  -bg #ffffaa -fg black -padx 2 -pady 0 -anchor w
        pack .vTcl.balloon.l -side left -padx 1 -pady 1
        wm geometry  .vTcl.balloon  +[expr {[winfo rootx %W]+[winfo width %W]/2}]+[expr {[winfo rooty %W]+[winfo height %W]+4}]
        set set 1
    }
}
bind "_vTclBalloon" <Button> {
    namespace eval ::redcat::vTcl::balloon {
        set first 0
    }
    ::redcat::vTcl:FireEvent %W <<KillBalloon>>
}
bind "_vTclBalloon" <Enter> {
    namespace eval ::redcat::vTcl::balloon {
        ## self defining balloon?
        if {![info exists %W]} {
            ::redcat::vTcl:FireEvent %W <<SetBalloon>>
        }
        set set 0
        set first 1
        set id [after 500 {::redcat::vTcl:FireEvent %W <<vTclBalloon>>}]
    }
}
bind "_vTclBalloon" <Leave> {
    namespace eval ::redcat::vTcl::balloon {
        set first 0
    }
    ::redcat::vTcl:FireEvent %W <<KillBalloon>>
}
bind "_vTclBalloon" <Motion> {
    namespace eval ::redcat::vTcl::balloon {
        if {!$set} {
            after cancel $id
            set id [after 500 {::redcat::vTcl:FireEvent %W <<vTclBalloon>>}]
        }
    }
}
}

::redcat::Window show .
::redcat::Window show .fullReport
::redcat::Window show .plot_vectors
::redcat::Window show .top24
::redcat::Window show .top32
::redcat::Window show .top33
::redcat::Window show .top34
::redcat::Window show .top35
::redcat::Window show .top36
::redcat::Window show .top37
::redcat::Window show .top38
::redcat::Window show .top53
::redcat::Window show .top56
::redcat::Window show .top61
::redcat::Window show .top62
::redcat::Window show .top63

::redcat::main {} {}
#$argc $argv

}
