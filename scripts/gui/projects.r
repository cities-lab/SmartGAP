# projects.r - functions for handling the project level
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

projects <- NULL
scenarios <- NULL
curProject <- NULL
curScenario <- NULL

projectMenu <- NULL
openProjectMenu <- NULL
projectParametersMenu <- NULL
		

editProjectScenarioComment <- function (){
	comment <- getComment("scenario")
	returnVal <- modalInputDialog(title="Project Scenario", question="Please enter a comment describing this project scenario: ", 
			entryInit = comment, entryWidth = 64)
	if (returnVal == "ID_CANCEL") return()
	
	setComment("scenario", returnVal)
	initProjectScenarioHeader()
}

initProjectScenario <- function (project, scenario) {
	curProject <<- project
	curScenario <<- scenario
	initComments()
	saveStartupProjectScenario()
}

changeProjectScenario <- function (project, scenario) {
	initProjectScenario(project, scenario)
	
	refreshMain()
	
	checkComments()	# since comments are scenario specific we check them every time they are loaded
}

refreshProjectScenarioMenus <- function() {	
	if(running) {
		disableProjectMenu()
		disableScenarioMenu()
		return
	} else {
		enableProjectMenu()
		enableScenarioMenu()
	}
	
	initProjectsOpenMenu()
	initScenarioOpenMenu()
	updateMenusForDemoProject()
	
	if(!running && runnable()) {
		tkentryconfigure(scenarioMenu, "Run", state="normal")
	} else {
		tkentryconfigure(scenarioMenu, "Run", state="disabled")
	}
}

initProjectScenarioHeader <- function () {
	comment <- getComment("scenario")
	tkconfigure(projectScenarioTitle, text=paste(curProject, ": ", curScenario,sep=""))
	if(isDemoBase()) {	# not allowed to edit Demo base
		tkconfigure(projectScenarioInfo, text=comment, cursor="")
		tkbind(projectScenarioInfo, "<Button-1>",  "")
	} else {
		tkconfigure(projectScenarioInfo, text=comment, cursor="hand2")
		tkbind(projectScenarioInfo, "<Button-1>",  editProjectScenarioComment)
	}
}

initProjectMenu <- function(container) {	
	projectMenu <<- tkmenu(container, tearoff = FALSE)
	openProjectMenu <<- tkmenu(container, tearoff = FALSE) 
	projectParametersMenu <<- tkmenu(container, tearoff = FALSE)
	tkadd(projectMenu, "command", label = "New", command = function() newProject())
	initProjectsOpenMenu()
	tkadd(projectMenu, "cascade", label = "Open", menu = openProjectMenu)
	tkadd(projectMenu, "command", label = "Save As", command = function() saveAsProject())
	tkadd(projectMenu, "command", label = "Rename", command = function() renameProject())
	tkadd(projectMenu, "command", label = "Delete", command = function() deleteProject())
	initProjectParametersMenu()
	tkadd(projectMenu, "cascade", label = "Parameters", menu = projectParametersMenu)
	tkadd(projectMenu, "command", label = "Quit", command = function() q("no"))
	tkadd(container, "cascade", label = "Project", menu = projectMenu)
}


initProjectsOpenMenu <- function () {
	tkdelete(openProjectMenu, 0, 9999)	# remove all old entries
	projects <<- dir(dirs$projects())
	for (project in projects) {	
		tkadd(openProjectMenu, "command", label = project, 
				command = eval(parse(text=paste('function(){openProject("',project,'")}',sep="")))) 
				# why the eval(parse('function as string')) business?  Here's why: http://bit.ly/cr9VJP
	}
}
	
disableProjectMenu <- function() setProjectMenuState("disabled")
enableProjectMenu <- function() setProjectMenuState("normal")

setProjectMenuState <- function(state) {
    menuItems <- list(
        "New",
        "Open",
        "Save As",
        "Rename",
        "Delete",
        "Parameters",
        "Quit"
    )

    lapply(menuItems, function(mi) tkentryconfigure(projectMenu, mi, state=state))
}

# for Demo Project & Base scenarios we disable certain menu items 
updateMenusForDemoProject <- function () {
	if(isDemoProject()) {
		tkentryconfigure(projectMenu, "Rename", state="disabled")
		tkentryconfigure(projectMenu, "Delete", state="disabled")
		tkentryconfigure(projectMenu, "Parameters", state="disabled")
	} else {
		tkentryconfigure(projectMenu, "Rename", state="normal")
		tkentryconfigure(projectMenu, "Delete", state="normal")
		tkentryconfigure(projectMenu, "Parameters", state="normal")
	}
	if(curScenario == "base") {
		tkentryconfigure(scenarioMenu, "Rename", state="disabled")
		tkentryconfigure(scenarioMenu, "Delete", state="disabled")
	} else {
		tkentryconfigure(scenarioMenu, "Rename", state="normal")
		tkentryconfigure(scenarioMenu, "Delete", state="normal")
	}
}

projectExistsWarning <- function(name) {
	if(name %in% projects){
		tkmessageBox(parent=tt, message = paste('"', name, '" already exists. Please use a different name.',sep=""), icon = "error")
		return(TRUE)
	}
	return(FALSE)
}

newProject <- function() {
	returnVal <- modalInputDialog("Project", "Please enter a project name: ", "")
	if (returnVal == "ID_CANCEL") return()
	if (projectExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()
	
	Try(.Tcl(paste('file mkdir "', paste(dirs$projects(returnVal),'base/inputs',sep='/'), '"', sep='')))
	Try(.Tcl(paste('file mkdir "', paste(dirs$projects(returnVal),'base/outputs',sep='/'), '"', sep='')))
	Try(.Tcl(paste('file copy "', dirs$projects(demoProject), '/base/', runScriptName, '" "', dirs$projects(returnVal),'/base"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$gui('comments.csv'), '" "', dirs$projects(returnVal),'/base"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$projects(demoProject), '/parameters" "', dirs$projects(returnVal),'"',sep='')))

	changeProjectScenario(returnVal, "base")
}

openProject <- function(name) {
	changeProjectScenario(name, "base")
}

saveAsProject <- function() {
	returnVal <- modalInputDialog("Project", "Please enter a new project name: ", curProject)
	if (returnVal == "ID_CANCEL") return()
	if (projectExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()
	
	Try(.Tcl(paste('file mkdir "', paste(dirs$projects(returnVal),'base',sep='/'), '"', sep='')))
	Try(.Tcl(paste('file copy "', dirs$projects(demoProject), '/base/', runScriptName, '" "', dirs$projects(returnVal),'/base"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$base('comments.csv'), '" "', dirs$projects(returnVal),'/base"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$curProject('parameters'), '" "', dirs$projects(returnVal),'"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$baseInputs(), '" "', dirs$projects(returnVal),'/base"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$base("outputs"), '" "', dirs$projects(returnVal),'/base"',sep='')))

	changeProjectScenario(returnVal, "base")
}

renameProject <- function() {
	returnVal <- modalInputDialog("Project", "Please enter a new project name: ", curProject)
	if (returnVal == "ID_CANCEL") return()
	if (projectExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()

	Try(.Tcl(paste('file rename -force "', dirs$curProject(), '" "', dirs$projects(returnVal),'"',sep='')))

	changeProjectScenario(returnVal, "base")
}

deleteProject <- function() {
	returnVal <- tkmessageBox(parent=tt, title="Warning", icon="warning", type="yesno",
			message=paste("Are you sure you want to delete ", curProject, "? This will remove the project and all project scenarios."))	
 
	if (tclvalue(returnVal) != "yes") return()

	Try(.Tcl(paste('file delete -force "', dirs$curProject(), '"', sep='')))
	
	changeProjectScenario(demoProject, "base")
}
