# flow_chart.r - functions for building and wiring up a flow chart
# SHRP 2 C16 GUI    - Jeremy Wallis, Chris Hoffman RSG, Inc.

#This program was developed as part of the SHRP 2 C16 project, The Effect of Smart Growth Policies on Travel Demand 
#See http://apps.trb.org/cmsfeed/TRBNetProjectDisplay.asp?ProjectID=2355
#The prime contractor for this project is RSG, Inc., http://www.rsginc.com
#Authors: Maren Outwater, Colin Smith, Michael Geilich, Chris Hoffman, Jeremy Wallis (all RSG), Christopher Gray, Jerry Walters (both Fehr and Peers), Rich Kuzmyak (Renaissance Planning Group), Dr. Robert Cervero, Dr. Kara Kockelman      

#This software is a significant modification of GreenSTEP, developed by Oregon Department of Transportation
#GreenSTEP was authored by Brian Gregor, Brian.J.Gregor@odot.state.or.us
#We acknowledge his great work in developing GreenSTEP and wish to thank him for his contribution to the practice by making GreenSTEP available as open source software 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program (see "license.txt"). If not, see <http://www.gnu.org/licenses/>.

# Copyright 2011-2012 RSG, Inc.


###############################################################################
# Globals
###############################################################################

curProcess <- NULL
prevProcess <- NULL

# define step box coloring
normalStepConfig = list()
normalStepConfig$background="blue"
normalStepConfig$foreground="white"
normalStepConfig$relief="raised"

selectedStepConfig = list()
selectedStepConfig$background="yellow"
selectedStepConfig$foreground="black"
selectedStepConfig$relief="sunken"

runningStepConfig = list()
runningStepConfig$background="yellow"
runningStepConfig$foreground="black"
runningStepConfig$relief="sunken"

completedStepConfig = list()
completedStepConfig$background="green"
completedStepConfig$foreground="black"
completedStepConfig$relief="raised"

stalecompletedStepConfig = list()
stalecompletedStepConfig$background="forest green"
stalecompletedStepConfig$foreground="black"
stalecompletedStepConfig$relief="raised"

inputchangedStepConfig = list()
inputchangedStepConfig$background="purple"	#purple4, slate blue, medium purple, tomato, tan3, firebrick
inputchangedStepConfig$foreground="black"
inputchangedStepConfig$relief="raised"

errorStepConfig = list()
errorStepConfig$background="red"
errorStepConfig$foreground="white"
errorStepConfig$relief="sunken"

stepConfigs = list()
stepConfigs$normal = normalStepConfig
stepConfigs$selected = selectedStepConfig
stepConfigs$running = runningStepConfig
stepConfigs$completed = completedStepConfig
stepConfigs$stalecompleted = stalecompletedStepConfig
stepConfigs$inputchanged = inputchangedStepConfig
stepConfigs$error = errorStepConfig


# lists of process statuses and process widgets
processStatuses <- list()
processLabels <- list()
processClocks <- list()

flowChartEnabled <- T

# container
flowChartContainer <- NULL

###############################################################################
# Initialization
#
# These functions should only get called once, and are responsible for
# building the flowchart-related GUI components and storing them in
# a list for later modification.
###############################################################################
initFlowChart <- function(container, processes) {
    flowChartContainer <<- container

    # create a down-arrow to be used between processes
    arrowImg <- tclVar()
    tcl("image","create","photo",arrowImg, file="../images/arrow.gif")

    # loads up the processLabels global
    initFlowChartProcesses(processes)

    # lay the elements out
    for(i in 1:length(processLabels)){
		plbl <- processLabels[[i]]
		pClock <- processClocks[[i]]
        tkgrid(plbl, pClock)
		tkgrid.configure(pClock, padx=10, sticky="w")
        # only put arrows between processes
        if(i < length(processLabels)){
            tkgrid(tklabel(flowChartContainer, image=arrowImg), column = 0)
        }
    }
	tkbind(container, "<Button-1>", function() clickOutsideProcess())
}

initFlowChartProcesses <- function(processes) {
    for(i in 1:nrow(processes)){
        p <- processes[i,]

        # create the flow-chart step
        lbl <- do.call(tklabel, as.list(c(list(parent=flowChartContainer, text=p$name, wraplength=150, width=21, cursor="hand2",
          font="FlowChartFont"), normalStepConfig))
        )

        # initialize status of process
        processStatuses[[i]] <<- "normal"

        # define the right-click menu
        processActionPopupMenu <- eval(parse(text=paste("getProcessActionPopupMenu('",p$step,"')",sep="")))

        processLabels[[i]] <<- lbl
		
		processClocks[[i]] <<- tklabel(flowChartContainer, text="")

        lbl <- processLabels[[i]]

        # bind events to left and right mouse clicks
        # why the eval(parse('function as string')) business?  Here's why: http://bit.ly/cr9VJP
        tkbind(lbl, "<Button-1>", eval(parse(text=paste(
            'function(){clickOnProcess("',p$name,'")}',sep="")))
        )

        # button 3 is right click for some reason
        tkbind(lbl, "<Button-3>", eval(parse(text=paste(
              'function(x,y) { 
                  openProcessActionPopupMenu(x, y, "',lbl$ID,'","',
                      processActionPopupMenu$ID,'","', p$name, '")
				}',sep='')
            ))
        )
    }
}

getProcessActionPopupMenu <- function(step) {
    menu <- tkmenu(flowChartContainer, tearoff = FALSE)
    tkadd(
        menu, "command", label="Run Step",
        command = function() {
            runClicked(step)
        }
    )
    menu
}

###############################################################################
# Event Processing
###############################################################################

openProcessActionPopupMenu <- function(x, y, lbl, menu, pname) {
    if(!running && runnable()) { 
		idx <- which(processes$name == pname)
		processStatuses[[idx]] <<- "normal"	# force click to behave as select
		clickOnProcess(pname)
        rootx <- as.integer(tkwinfo("rootx", lbl))
        rooty <- as.integer(tkwinfo("rooty", lbl))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        .Tcl(paste("tk_popup", .Tcl.args(menu, xTxt, yTxt)))
    }
}

clickOnProcess <- function(pname) {
    if(!running){
        idx <- which(processes$name == pname)
        status <- processStatuses[[idx]]
        if(status != "selected"){
          curProcess <<- pname  
        } else {
			curProcess <<- NULL  
		}
		refreshFlowChart()
		highlightProcessInputFiles()
    }
}

clickOutsideProcess <- function() {
	if(!running) {
		curProcess <<- NULL
		refreshFlowChart()
		highlightProcessInputFiles()
	}
}


refreshFlowChart <- function () {
	if(!running) {
		df.inputFiles <- getInputFiles()
		curProcessIndex <- which(processes$name == curProcess)
		for(i in 1:length(processStatuses)){
			processStatuses[[i]] <<- 'normal'	### in future, we may use other states such as "stalecompleted" or "inputchanged", but for now use "normal"
			if(!is.null(curProcess)) {
				if(i == curProcessIndex) {
					 processStatuses[[i]] <<- 'selected'
				}
			} 
			indicateProcessStatus(i, processStatuses[[i]])
		}
	}
}


# (call this right before a run) clears curProcess and sets all statuses to 'normal' , clears cursor
prerunFlowChart <- function () {
	curProcess <<- NULL
	for(i in 1:length(processStatuses)){	
		processStatuses[[i]] <<- "normal"
		tkconfigure(processClocks[[i]], text = "")
	}
    # clear highlighting
    for(pwidget in processLabels){
    	do.call(tkconfigure, as.list(c(list(widget=pwidget), stepConfigs[["normal"]])))
		tkconfigure(pwidget, cursor="")
    }
}
# (call this after a run) sets cursor back to hand
postrunFlowChart <- function () {
	for(pwidget in processLabels){
		tkconfigure(pwidget, cursor="hand2")
	}
}

indicateProcessStatus <- function(idx, status) {
	lbl <- processLabels[[idx]]
	processStatuses[[idx]] <<- status	
	do.call(tkconfigure, as.list(c(list(widget=lbl), stepConfigs[[status]])))
	tkconfigure(processClocks[[idx]], text = elapsedTime(runStartTime))
}

updateFlowChartStep <- function(currentProcess, previousProcess=NULL, cancelled=FALSE) {
	if(!is.null(previousProcess)) {
		idx <- which(processes$name == previousProcess)
		endTimer()
		indicateProcessStatus(idx, "completed")
	}
	if(!is.null(currentProcess)) {
		idx <- which(processes$name == currentProcess)
		if (cancelled) {
			indicateProcessStatus(idx, "error")
			endTimer()
		}
		else {
			indicateProcessStatus(idx, "running")
			startTimer()
		}
	}
}

startTimer <- function () {
	refreshElapsedTime()
	tclTaskSchedule(1000, refreshElapsedTime(), id = "flowChartTimer", redo = TRUE)
}

endTimer <- function(){
	tclTaskDelete("flowChartTimer")
}

refreshElapsedTime <- function () {	
	if(!is.null(curProcess) & !is.null(runStartTime)) {
		tkconfigure(processClocks[[which(processes$name == curProcess)]], text = elapsedTime(runStartTime))
	}
}
