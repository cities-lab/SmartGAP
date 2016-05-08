# main.r - main executing code block, parses options, builds the GUI
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

options(warn=1)
options(error=traceback)

# note: put all source and package dependencies in 'dependencies.r'
source("dependencies.r")


###############################################################################
# Globals 
###############################################################################

version <- "0.90"

dirs <- list()
dirs$programRootDir <- normalizePath("../../", winslash="/")
dirs$programRootDir <- substr(dirs$programRootDir, 1,nchar(dirs$programRootDir)-1)
dirs$gui <- function(...) paste(dirs$programRootDir,"scripts","gui",...,sep="/") 
dirs$projects <- function(...) paste(dirs$programRootDir,"projects",...,sep="/") 
dirs$curProject <- function(...) paste(dirs$projects(curProject),...,sep="/")
dirs$curScenario <- function(...) paste(dirs$curProject(curScenario),...,sep="/")
dirs$parameters <- function(...) paste(dirs$curProject("parameters"),...,sep="/")
dirs$base <- function(...) paste(dirs$curProject("base"),...,sep="/")
dirs$baseInputs <- function(...) paste(dirs$base("inputs"),...,sep="/")
dirs$curScenInputs <- function(...) paste(dirs$curScenario("inputs"),...,sep="/")
dirs$curScenOutputs <- function(...) paste(dirs$curScenario("outputs"),...,sep="/")

###############################################################################
# Parameters
###############################################################################

runScriptName <- "run_SmartGAP.r"
demoProject <- "Demo Project"

###############################################################################
# Command-line Parsing
###############################################################################

option_list <- list(
    make_option(c("-p", "--project"), help="Project to run"),
    make_option(c("-s", "--scenario"), help="Scenario to run")
)
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
opt <- parse_args(OptionParser(option_list=option_list))

###############################################################################
# GUI Construction
###############################################################################

# Top Window
tt <- tktoplevel()
tkwm.state(tt,"withdrawn") # was at beginneing
tktitle(tt) <- "Smart Growth Area Planning (SmartGAP)"

# Load prefs (needs to happen after top window creation in case we need to show an error message)
getStartupProjectScenario()

# Menu
topMenu <- tkmenu(tt)
tkconfigure(tt, menu = topMenu) 
initProjectMenu(topMenu)
initScenarioMenu(topMenu)
helpMenu <- tkmenu(topMenu, tearoff = FALSE)
tkadd(helpMenu, "command", label = "About", command = function() openAboutWindow(tt))
tkadd(helpMenu, "command", label = "User Guide", command = function() .Tcl("exec {*}[auto_execok cmd.exe] /Q /C \"user_guide.pdf\" &"))
tkadd(topMenu, "cascade", label = "Help", menu = helpMenu)

# layout
leftFrame <- tkframe(tt) 	#, borderwidth=3, relief="ridge")
rightFrame <- tkframe(tt)
ioNotebook <- ttknotebook(rightFrame)
#tkgrid(ioNotebook)
tkpack(ioNotebook, fill="both", expand=TRUE)

# Project - Scenario header
projectScenarioFrame <- tkframe(leftFrame)
projectScenarioTitle <- tklabel(projectScenarioFrame, text="Project: Scenario", font='ProjectScenarioTitleFont')
projectScenarioInfo <- tklabel(projectScenarioFrame, text="Please enter project scenario comment", foreground="dark gray", font='CommentFont')
tkgrid(projectScenarioTitle)
tkgrid(projectScenarioInfo)
tkgrid(projectScenarioFrame)

# get process list to feed into flow chart
processes <- getProcesses()

# Model Flow Chart & Run button
modelFlowChartFrame <- tkframe(leftFrame)
flowchartLabel <- tklabel(modelFlowChartFrame, text="Model Flow  ", font="FlowChartTitleFont")
runButton <- tkbutton(modelFlowChartFrame, text="Run", command=function() runClicked(), font="ButtonHeightFont")
tkgrid(flowchartLabel, runButton, pady=10)
modelFlowChart <- initFlowChart(modelFlowChartFrame, processes)
tkgrid(modelFlowChartFrame)

# Input Files
inputFilesFrame <- tkframe(ioNotebook)
tkadd(ioNotebook, inputFilesFrame, text="Inputs")

# Output Files
outputFilesFrame <- tkframe(ioNotebook)
tkadd(ioNotebook, outputFilesFrame, text="Outputs")

# Reports
reportsFrame <- tkframe(ioNotebook)
tkadd(ioNotebook, reportsFrame, text="Reports")

#Footer with RSG Logo & Licensing blurb
footerFrame <- tkframe(tt, borderwidth=3, relief="ridge")
logoImg <- tclVar()
tcl("image","create","photo",logoImg, file="../images/RSGlogo.gif")
rsgLogo <- tklabel(footerFrame, image=logoImg)
createdByLabel <- tklabel(footerFrame, text="This software was created by:", font="LicenseFont")
licensingLabel <- tklabel(footerFrame, text="Copyright © 2011-2012 RSG Inc., All rights reserved.", font="LicenseFont")
tkpack(createdByLabel,rsgLogo, licensingLabel, side="left", expand=TRUE)

###############################################################################
# Layout
###############################################################################
# we use tkpack here because it plays better with window rezizing
tkpack(footerFrame, side="bottom", fill="x")
tkpack(leftFrame, side="left", anchor="n")
tkpack(rightFrame, side="right", anchor="n", fill="both", expand=TRUE)

###############################################################################
# Refresh all app items
###############################################################################
refreshMain <- function (doFlowChart=TRUE) {
	initProjectScenarioHeader()
	refreshProjectScenarioMenus()
	refreshInputs(inputFilesFrame)
	if(doFlowChart){	# after a run we'll want to leave flow chart alone until next refresh
		refreshFlowChart()
	}
	refreshRunButton()
	refreshOutputs(outputFilesFrame)
	refreshReportsFrame(reportsFrame)
}
refreshMain()

# now show the main window
tkwm.state(tt,"normal") # visible = T

# if not tall enough (inputs scroll frame can make it short) then bump it up so inputs scroll not needed
geom <- as.numeric(unlist(strsplit(as.character(tkwm.geometry(tt)),c('[+x]'))))
if(geom[2] < 775 ) {
	tkwm.geometry(tt, paste(geom[1],"x",775,"+",geom[3],"+",geom[4],sep=""))
}

###############################################################################
# our foreign key constraints in our csv files are not always synced up, check them for errors
###############################################################################

checkCSVKeyConstraints()

###############################################################################
# SCENARIO PROCESSING
#   If the user specified a scenario to run, run it, then exit.
###############################################################################
if(!is.null(opt$project) && !is.null(opt$scenario)){
    curProject <- opt$project
    curScenario <- opt$scenario
    runScenario()
	
	.Tcl("vwait done")
	tkdestroy(tt)
        quit(save = "default", status = 0, runLast = TRUE)
}

###############################################################################
# MAIN EVENT LOOP
###############################################################################

.Tcl("vwait quit")

# Here is where you would put code that saves the workspace etc.
# e.g., save the progress from open scenarios as a *project.RData* file

tkdestroy(tt)
