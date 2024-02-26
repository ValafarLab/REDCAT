#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {

    package require Tk
    switch $tcl_platform(platform) {
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
    
}

#############################################################################
# Visual Tcl v1.60 Project
#


#################################
# VTCL LIBRARY PROCEDURES
#

if {![info exists vTcl(sourcing)]} {
#############################################################################
## Library Procedure:  Window

proc ::Window {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    global vTcl
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
            } elseif {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[winfo exists $newname] && [wm state $newname] == "normal"} {
                vTcl:FireEvent $newname <<Show>>
            }
        }
        hide    {
            if {$exists} {
                wm withdraw $newname
                vTcl:FireEvent $newname <<Hide>>
                return}
        }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}
#############################################################################
## Library Procedure:  vTcl:DefineAlias

proc ::vTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    global widget
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

proc ::vTcl:DoCmdOption {target cmd} {
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

proc ::vTcl:FireEvent {target event {params {}}} {
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

proc ::vTcl:Toplevel:WidgetProc {w args} {
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
                return [set ::${w}::${varname}]
            } else {
                return [set ::${w}::${varname} $value]
            }
        }
        "hide" - "show" {
            Window [string tolower $command] $w
        }
        "showmodal" {
            ## modal dialog ends when window is destroyed
            Window show $w; raise $w
            grab $w; tkwait window $w; grab release $w
        }
        "startmodal" {
            ## ends when endmodal called
            Window show $w; raise $w
            set ::${w}::_modal 1
            grab $w; tkwait variable ::${w}::_modal; grab release $w
        }
        "endmodal" {
            ## ends modal dialog started with startmodal, argument is var name
            set ::${w}::_modal 0
            Window hide $w
        }
        default {
            uplevel $w $command $args
        }
    }
}
#############################################################################
## Library Procedure:  vTcl:WidgetProc

proc ::vTcl:WidgetProc {w args} {
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
## Library Procedure:  vTcl:toplevel

proc ::vTcl:toplevel {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    uplevel #0 eval toplevel $args
    set target [lindex $args 0]
    namespace eval ::$target {set _modal 0}
}
}


if {[info exists vTcl(sourcing)]} {

proc vTcl:project:info {} {
    set base .top47
    namespace eval ::widgets::$base {
        set set,origin 1
        set set,size 1
        set runvisible 1
    }
    namespace eval ::widgets::$base.but50 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$base.but47 {
        array set save {-command 1 -text 1}
    }
    namespace eval ::widgets::$base.statusLable {
        array set save {-text 1}
    }
    namespace eval ::widgets::$base.fileName {
        array set save {-text 1}
    }
    namespace eval ::widgets::$base.xColumn {
        array set save {-text 1}
    }
    namespace eval ::widgets::$base.yColumn {
        array set save {-text 1}
    }
    namespace eval ::widgets::$base.title {
        array set save {-text 1}
    }
    namespace eval ::widgets::$base.fName1 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x1 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y1 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t1 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.fname2 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x2 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y2 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t2 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.fname3 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x3 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y3 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t3 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.fname4 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x4 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y4 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t4 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.fname5 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x5 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y5 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t5 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.fname6 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.x6 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.y6 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.t6 {
        array set save {-background 1 -insertbackground 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.cpd64 {
        array set save {-borderwidth 1 -height 1}
    }
    set site_3_0 $base.cpd64
    namespace eval ::widgets::$site_3_0.01 {
        array set save {-anchor 1 -text 1}
    }
    namespace eval ::widgets::$site_3_0.02 {
        array set save {-cursor 1 -textvariable 1}
    }
    namespace eval ::widgets::$base.button1 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.button2 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.button3 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.button4 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.button5 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.button6 {
        array set save {-text 1 -variable 1}
    }
    namespace eval ::widgets::$base.capture {
        array set save {-command 1 -disabledforeground 1 -text 1}
    }
    namespace eval ::widgets::$base.m47 {
        array set save {-disabledforeground 1 -tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -value 1 -variable 1}
        }
    }
    namespace eval ::widgets_bindings {
        set tagslist _TopLevel
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            init
            main
        }
        set compounds {
        }
        set projectType single
    }
}
}

#################################
# USER DEFINED PROCEDURES
#
#############################################################################
## Procedure:  main

proc ::main {argc argv} {
global ImageType
global fileid
global SelectButton
global fname
global x
global y
global t
global stat

set ImageType "png"

set stat 0
if { ($::argc == 1) } {
   set stat [lindex $argv 0] 
}

set fileid [open "|gnuplot" w+]
flush $fileid 
fconfigure $fileid -buffering none
puts $fileid "set mouse"

if { $stat == 0 } {
   set SelectButton(1) 1
   set SelectButton(2) 1
   set SelectButton(3) 1
   set SelectButton(4) 0
   set SelectButton(5) 0
   set SelectButton(6) 0

   set fname(1) "Results.dat"
   set fname(2) "Results.dat"
   set fname(3) "Results.dat"

   set x(1) 1
   set y(1) 2

   set x(2) 3
   set y(2) 4

   set x(3) 5
   set y(3) 6

   set t(1) "Sxx"
   set t(2) "Syy"
   set t(3) "Szz"
}
if { $stat == 1 } {
   set SelectButton(1) 1
   set SelectButton(2) 0
   set SelectButton(3) 0
   set SelectButton(4) 0
   set SelectButton(5) 0
   set SelectButton(6) 0

   set fname(1) "Errors.dat"

   set x(1) 5
   set y(1) 6

   set t(1) "Violations"
}
}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {

}

init $argc $argv

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $top passive
    wm geometry $top 1x1+0+0; update
    wm maxsize $top 1265 770
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm withdraw $top
    wm title $top "vtcl.tcl #2"
    bindtags $top "$top Vtcl.tcl all"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    ###################
    # SETTING GEOMETRY
    ###################

    vTcl:FireEvent $base <<Ready>>
}

proc vTclWindow.top47 {base} {
    if {$base == ""} {
        set base .top47
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -menu "$top.m47" -highlightcolor black 
    wm focusmodel $top passive
    wm geometry $top 533x288+745+435; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "2D Gnuplot Interface"
    vTcl:DefineAlias "$top" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    button $top.but50 \
        \
        -command {set mapDat "map.dat"
if {$argc == 2} {
set mapDat [lindex $argv 1]
}
if {$stat == 0 } {
   puts -nonewline $fileid "plot '$mapDat' w d lt -1"
   for {set i 1} {$i <= 6} {incr i} {
       if {$SelectButton($i) == 1} {
          puts -nonewline $fileid ", '$fname($i)' using $x($i):$y($i) t '$t($i)'"
       }
   }
   puts $fileid ""
}

if {$stat == 1 } {
   puts -nonewline $fileid "plot "
   for {set i 1} {$i <= 6} {incr i} {
       if {$SelectButton($i) == 1} {
          puts -nonewline $fileid "'$fname($i)' using $x($i):$y($i) w i t '$t($i)'"
       }
   }
   puts $fileid ""
}} \
        -text Plot 
    vTcl:DefineAlias "$top.but50" "Button2" vTcl:WidgetProc "Toplevel1" 1
    labelframe $top.but50.lab53 \
        -foreground black -text Label -highlightcolor black 
    vTcl:DefineAlias "$top.but50.lab53" "Labelframe1" vTcl:WidgetProc "Toplevel1" 1
    button $top.but47 \
        \
        -command {if {$fileid != ""} {
   puts $fileid "quit"
   close $fileid
}

exit} \
        -text Quit 
    vTcl:DefineAlias "$top.but47" "Button3" vTcl:WidgetProc "Toplevel1" 1
    label $top.statusLable \
        -text Status 
    vTcl:DefineAlias "$top.statusLable" "Label1" vTcl:WidgetProc "Toplevel1" 1
    label $top.fileName \
        -text {File Name} 
    vTcl:DefineAlias "$top.fileName" "Label2" vTcl:WidgetProc "Toplevel1" 1
    label $top.xColumn \
        -text {X Column} 
    vTcl:DefineAlias "$top.xColumn" "Label3" vTcl:WidgetProc "Toplevel1" 1
    label $top.yColumn \
        -text {Y Column} 
    vTcl:DefineAlias "$top.yColumn" "Label4" vTcl:WidgetProc "Toplevel1" 1
    label $top.title \
        -text Title 
    vTcl:DefineAlias "$top.title" "Label5" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fName1 \
        -background white -insertbackground black -textvariable fname(1) 
    vTcl:DefineAlias "$top.fName1" "Entry1" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x1 \
        -background white -insertbackground black -textvariable x(1) 
    vTcl:DefineAlias "$top.x1" "Entry2" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y1 \
        -background white -insertbackground black -textvariable y(1) 
    vTcl:DefineAlias "$top.y1" "Entry3" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t1 \
        -background white -insertbackground black -textvariable t(1) 
    vTcl:DefineAlias "$top.t1" "Entry4" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fname2 \
        -background white -insertbackground black -textvariable fname(2) 
    vTcl:DefineAlias "$top.fname2" "Entry5" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x2 \
        -background white -insertbackground black -textvariable x(2) 
    vTcl:DefineAlias "$top.x2" "Entry6" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y2 \
        -background white -insertbackground black -textvariable y(2) 
    vTcl:DefineAlias "$top.y2" "Entry7" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t2 \
        -background white -insertbackground black -textvariable t(2) 
    vTcl:DefineAlias "$top.t2" "Entry8" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fname3 \
        -background white -insertbackground black -textvariable fname(3) 
    vTcl:DefineAlias "$top.fname3" "Entry9" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x3 \
        -background white -insertbackground black -textvariable x(3) 
    vTcl:DefineAlias "$top.x3" "Entry10" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y3 \
        -background white -insertbackground black -textvariable y(3) 
    vTcl:DefineAlias "$top.y3" "Entry11" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t3 \
        -background white -insertbackground black -textvariable t(3) 
    vTcl:DefineAlias "$top.t3" "Entry12" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fname4 \
        -background white -insertbackground black -textvariable fname(4) 
    vTcl:DefineAlias "$top.fname4" "Entry13" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x4 \
        -background white -insertbackground black -textvariable x(4) 
    vTcl:DefineAlias "$top.x4" "Entry14" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y4 \
        -background white -insertbackground black -textvariable y(4) 
    vTcl:DefineAlias "$top.y4" "Entry15" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t4 \
        -background white -insertbackground black -textvariable t(4) 
    vTcl:DefineAlias "$top.t4" "Entry16" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fname5 \
        -background white -insertbackground black -textvariable fname(5) 
    vTcl:DefineAlias "$top.fname5" "Entry17" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x5 \
        -background white -insertbackground black -textvariable x(5) 
    vTcl:DefineAlias "$top.x5" "Entry18" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y5 \
        -background white -insertbackground black -textvariable y(5) 
    vTcl:DefineAlias "$top.y5" "Entry19" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t5 \
        -background white -insertbackground black -textvariable t(5) 
    vTcl:DefineAlias "$top.t5" "Entry20" vTcl:WidgetProc "Toplevel1" 1
    entry $top.fname6 \
        -background white -insertbackground black -textvariable fname(6) 
    vTcl:DefineAlias "$top.fname6" "Entry21" vTcl:WidgetProc "Toplevel1" 1
    entry $top.x6 \
        -background white -insertbackground black -textvariable x(6) 
    vTcl:DefineAlias "$top.x6" "Entry22" vTcl:WidgetProc "Toplevel1" 1
    entry $top.y6 \
        -background white -insertbackground black -textvariable y(6) 
    vTcl:DefineAlias "$top.y6" "Entry23" vTcl:WidgetProc "Toplevel1" 1
    entry $top.t6 \
        -background white -insertbackground black -textvariable t(6) 
    vTcl:DefineAlias "$top.t6" "Entry24" vTcl:WidgetProc "Toplevel1" 1
    frame $top.cpd64 \
        -borderwidth 1 -height 28 
    vTcl:DefineAlias "$top.cpd64" "Frame5" vTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.cpd64
    label $site_3_0.01 \
        -anchor w -text Command: 
    vTcl:DefineAlias "$site_3_0.01" "Label6" vTcl:WidgetProc "Toplevel1" 1
    entry $site_3_0.02 \
        -cursor {} -textvariable command 
    vTcl:DefineAlias "$site_3_0.02" "Entry25" vTcl:WidgetProc "Toplevel1" 1
    bind $site_3_0.02 <Key-Return> {
        # TODO: your event handler here
global fileid

puts $fileid "$command"
set command ""
    }
    pack $site_3_0.01 \
        -in $site_3_0 -anchor center -expand 0 -fill none -padx 2 -pady 2 \
        -side left 
    pack $site_3_0.02 \
        -in $site_3_0 -anchor center -expand 1 -fill x -padx 2 -pady 2 \
        -side right 
    checkbutton $top.button1 \
        -text {Plot 1} -variable SelectButton(1) 
    vTcl:DefineAlias "$top.button1" "Checkbutton1" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $top.button2 \
        -text {Plot 2} -variable SelectButton(2) 
    vTcl:DefineAlias "$top.button2" "Checkbutton2" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $top.button3 \
        -text {Plot 3} -variable SelectButton(3) 
    vTcl:DefineAlias "$top.button3" "Checkbutton3" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $top.button4 \
        -text {Plot 4} -variable SelectButton(4) 
    vTcl:DefineAlias "$top.button4" "Checkbutton4" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $top.button5 \
        -text {Plot 5} -variable SelectButton(5) 
    vTcl:DefineAlias "$top.button5" "Checkbutton5" vTcl:WidgetProc "Toplevel1" 1
    checkbutton $top.button6 \
        -text {Plot 6} -variable SelectButton(6) 
    vTcl:DefineAlias "$top.button6" "Checkbutton6" vTcl:WidgetProc "Toplevel1" 1
    button $top.capture \
        \
        -command {if {$ImageType == "postscript"} {
   set filename "Picture.ps"
}
if {$ImageType == "png"} {
   set filename "Picture.png"
}

puts $fileid "set term push"
puts $fileid "set term $ImageType color"
puts $fileid "set output '$filename'"
puts $fileid "replot"
puts $fileid "set term pop"} \
        -disabledforeground #a2a2a2 -text {Capture Image} 
    vTcl:DefineAlias "$top.capture" "Button1" vTcl:WidgetProc "Toplevel1" 1
    menu $top.m47 \
        -disabledforeground #a2a2a2 -tearoff 1 
    $top.m47 add radiobutton \
        -value postscript -variable ImageType \
        -command {# TODO: Your menu handler here} -label Postscript 
    $top.m47 add radiobutton \
        -value png -variable ImageType \
        -command {# TODO: Your menu handler here} -label PNG 
    ###################
    # SETTING GEOMETRY
    ###################
    place $top.but50 \
        -in $top -x 130 -y 255 -anchor nw -bordermode ignore 
    place $top.but50.lab53 \
        -in $top.but50 -x 25 -y 15 -anchor nw -bordermode ignore 
    place $top.but47 \
        -in $top -x 370 -y 255 -anchor nw -bordermode ignore 
    place $top.statusLable \
        -in $top -x 20 -y 15 -anchor nw -bordermode ignore 
    place $top.fileName \
        -in $top -x 115 -y 15 -anchor nw -bordermode ignore 
    place $top.xColumn \
        -in $top -x 220 -y 15 -anchor nw -bordermode ignore 
    place $top.yColumn \
        -in $top -x 285 -y 15 -anchor nw -bordermode ignore 
    place $top.title \
        -in $top -x 420 -y 15 -anchor nw -bordermode ignore 
    place $top.fName1 \
        -in $top -x 90 -y 40 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x1 \
        -in $top -x 235 -y 40 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y1 \
        -in $top -x 300 -y 40 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t1 \
        -in $top -x 370 -y 40 -anchor nw -bordermode ignore 
    place $top.fname2 \
        -in $top -x 90 -y 70 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x2 \
        -in $top -x 235 -y 70 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y2 \
        -in $top -x 300 -y 70 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t2 \
        -in $top -x 370 -y 71 -width 148 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.fname3 \
        -in $top -x 90 -y 100 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x3 \
        -in $top -x 235 -y 100 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y3 \
        -in $top -x 300 -y 100 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t3 \
        -in $top -x 370 -y 100 -anchor nw -bordermode ignore 
    place $top.fname4 \
        -in $top -x 90 -y 130 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x4 \
        -in $top -x 235 -y 130 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y4 \
        -in $top -x 300 -y 130 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t4 \
        -in $top -x 370 -y 130 -anchor nw -bordermode ignore 
    place $top.fname5 \
        -in $top -x 90 -y 160 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x5 \
        -in $top -x 235 -y 160 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y5 \
        -in $top -x 300 -y 160 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t5 \
        -in $top -x 370 -y 160 -anchor nw -bordermode ignore 
    place $top.fname6 \
        -in $top -x 90 -y 190 -width 113 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.x6 \
        -in $top -x 235 -y 190 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.y6 \
        -in $top -x 300 -y 190 -width 33 -height 22 -anchor nw \
        -bordermode ignore 
    place $top.t6 \
        -in $top -x 370 -y 190 -anchor nw -bordermode ignore 
    place $top.cpd64 \
        -in $top -x 15 -y 215 -width 506 -height 28 -anchor nw \
        -bordermode inside 
    place $top.button1 \
        -in $top -x 15 -y 40 -anchor nw -bordermode ignore 
    place $top.button2 \
        -in $top -x 15 -y 70 -anchor nw -bordermode ignore 
    place $top.button3 \
        -in $top -x 15 -y 100 -anchor nw -bordermode ignore 
    place $top.button4 \
        -in $top -x 15 -y 130 -anchor nw -bordermode ignore 
    place $top.button5 \
        -in $top -x 15 -y 160 -anchor nw -bordermode ignore 
    place $top.button6 \
        -in $top -x 15 -y 190 -anchor nw -bordermode ignore 
    place $top.capture \
        -in $top -x 215 -y 255 -anchor nw -bordermode ignore 

    vTcl:FireEvent $base <<Ready>>
}

#############################################################################
## Binding tag:  _TopLevel

bind "_TopLevel" <<Create>> {
    if {![info exists _topcount]} {set _topcount 0}; incr _topcount
}
bind "_TopLevel" <<DeleteWindow>> {
    if {[set ::%W::_modal]} {
                vTcl:Toplevel:WidgetProc %W endmodal
            } else {
                destroy %W; if {$_topcount == 0} {exit}
            }
}
bind "_TopLevel" <Destroy> {
    if {[winfo toplevel %W] == "%W"} {incr _topcount -1}
}

Window show .
Window show .top47

main $argc $argv
