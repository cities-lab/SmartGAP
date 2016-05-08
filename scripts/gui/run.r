# run.r - runs model in external R process while maintaining GUI responsiveness
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

#Module globals
running <- FALSE
runStartTime <- NULL
processId <- 0

runClicked <- function(step=NULL) {
	if(running) {
		cancelRun()
	} else {
		preRun(step)
		runScenario(step)
	}
}

refreshRunButton <- function() {
	if(running) {
		tkconfigure(runButton, text="Cancel")
	} else {
		tkconfigure(runButton, text="Run")
	}
	if(runnable()) {
		tkconfigure(runButton, state="normal", cursor = "hand2")
	} else {
		tkconfigure(runButton, state="disabled", cursor = "")
	}
}

runnable <- function() {
	noInputsAreMissing()	# cannot run if any input is missing (in neither scenario nor base)
}

# set up before run
preRun <- function(step=NULL) {
	prerunFlowChart()
	running <<- TRUE
	runStartTime <<- Sys.time()
	prevProcess <<- NULL
	if(is.null(step)) {
		curProcess <<- processes[1,]$name	
		deleteOutputFiles()
	} else {
		curProcess <<- processes[which(processes$step == step),]$name
		deleteOutputFiles(curProcess)
	}
	
	refreshMain(doFlowChart=FALSE)
	
	updateFlowChartStep(curProcess, prevProcess)	
	
	tcl(ioNotebook, "select", 1)	#Switch to Outputs tab.
}

# clean up after run
endRun <- function() {
	postrunFlowChart()
	running <<- FALSE
	runStartTime <<- NULL
	processId <<- 0			#Model no longer running
	curProcess <<- NULL
	prevProcess <<- NULL

	refreshMain(doFlowChart=FALSE) #leave flow chart run results alone until a later refresh
}

# clean up after run
postRun <- function() {
	endRun()
	tkmessageBox(parent=tt, message="Run Complete!")
}

# subprocess exited prematurely
abortRun <- function() {
	if (!is.null(prevProcess)) {
		updateFlowChartStep(prevProcess, NULL, TRUE)
		deleteOutputFiles(prevProcess)
	}
	endRun()
	tkmessageBox(parent=tt, message="Run exited without finishing")
}

cancelRun <- function() {
	#Using the module global processId, stop the running model.
	if (processId != 0) {
		system(paste("taskkill /f /t /pid ", processId))
	}
	if (!is.null(prevProcess)) {
		updateFlowChartStep(prevProcess, NULL, TRUE)
		deleteOutputFiles(prevProcess)
	}
	endRun()
	tkmessageBox(parent=tt, message="Run Cancelled!")
}

runScenario <- function(step=NULL) {
    # make a system call so we can start another process and monitor it
    # via the *fileevent* tcl construct
    cmd <- paste("
        proc sock2Var { pipe } {
           global done
           if {[eof $pipe]} {
              catch {close $pipe}
              set done 1
           ", .Tcl.callback(checkExitStatus), "		
              return
           }
           gets $pipe line
           ", .Tcl.callback(updateProgress), "
        }
        cd \"", dirs$curScenario(), "\"
        set pipe [",
            "open \"|Rscript.exe run_SmartGAP.r",  
            # option to run single step
            ifelse(is.null(step),"",paste(" -p", step)),
        " 2>@1\"]
        cd \"", dirs$gui(), "\"
        fileevent $pipe readable [list sock2Var $pipe]
	    set procId [pid $pipe]
    ", sep="")
    .Tcl(cmd)

	#Store the process ID in case we need to kill it later
	processId <<- tclvalue("procId")
}


checkExitStatus <- function() {
	#Abnormal termination: The pipe closed before the SUCCESS message was delivered.
	if (running == TRUE) {
		abortRun()
	}
}

updateProgress <- function() {
    print(tclvalue("line"))

    # remove superfluous syntax from print statements
    line <- sub("^[^\"]*\"([^\"]*)\"","\\1", tclvalue("line"))
    process <- subset(processes, codeHeader == line)

    if(nrow(process) > 0) {
    # update GUI
      setEnableExistingOutputFiles()
	  curProcess <<- process$name
      updateFlowChartStep(curProcess, prevProcess)
	  updateOutputsStep(curProcess)
      prevProcess <<- curProcess
    }
	
    # see if run complete
    if(line == "RUN COMPLETED") {	
	  curProcess <<- NULL
      updateFlowChartStep(curProcess, prevProcess)
      postRun()
    }
}
