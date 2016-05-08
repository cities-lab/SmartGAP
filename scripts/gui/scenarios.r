# scenarios.r - functions for handling the scenario level
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


projectMenu <- NULL
openScenarioMenu <- NULL

initScenarioMenu <- function(container) {	
	scenarioMenu <<- tkmenu(container, tearoff = FALSE)
	openScenarioMenu <<- tkmenu(container, tearoff = FALSE)
	tkadd(scenarioMenu, "command", label = "New", command = function() newScenario())
	initScenarioOpenMenu()
	tkadd(scenarioMenu, "cascade", label = "Open", menu = openScenarioMenu)
	tkadd(scenarioMenu, "command", label = "Save As", command = function() saveAsScenario())
	tkadd(scenarioMenu, "command", label = "Run", command = function() runClicked())
	tkadd(scenarioMenu, "command", label = "Rename", command = function() renameScenario())
	tkadd(scenarioMenu, "command", label = "Delete", command = function() deleteScenario())
	tkadd(container, "cascade", label = "Scenario", menu = scenarioMenu)
}


initScenarioOpenMenu <- function () {
	tkdelete(openScenarioMenu, 0, 9999)	# remove all old entries
	scenarios <<- dir(dirs$curProject())
	scenarios <<- subset(scenarios, scenarios != "parameters")
	for (scenario in scenarios) {	
		tkadd(openScenarioMenu, "command", label = scenario, 
			command = eval(parse(text=paste('function(){openScenario("',scenario,'")}',sep="")))) 
			# why the eval(parse('function as string')) business?  Here's why: http://bit.ly/cr9VJP
	}
	
}

disableScenarioMenu <- function() setScenarioMenuState("disabled")
enableScenarioMenu <- function() setScenarioMenuState("normal")

setScenarioMenuState <- function(state) {
    menuItems <- list(
        "New",
        "Open",
        "Save As",
        "Run",
        "Rename",
        "Delete"
    )

    lapply(menuItems, function(mi) tkentryconfigure(scenarioMenu, mi, state=state))
}

scenarioExistsWarning <- function(name) {
	if(name %in% scenarios){
		tkmessageBox(parent=tt, message = paste('"', name, '" already exists. Please use a different name.',sep=""), icon = "error")
		return(TRUE)
	}
	return(FALSE)
}

newScenario <- function() {
	returnVal <- modalInputDialog("Scenario", "Please enter a scenario name: ", "")
	if (returnVal == "ID_CANCEL") return()
	if (scenarioExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()
	
	Try(.Tcl(paste('file mkdir "', paste(dirs$curProject(returnVal),'inputs',sep='/'), '"', sep='')))
	Try(.Tcl(paste('file mkdir "', paste(dirs$curProject(returnVal),'outputs',sep='/'), '"', sep='')))
	Try(.Tcl(paste('file copy "', dirs$projects(demoProject), '/base/', runScriptName, '" "', dirs$curProject(returnVal),'"',sep='')))
	Try(.Tcl(paste('file copy "', dirs$gui('comments.csv'), '" "', dirs$curProject(returnVal),'"',sep='')))
	
	changeProjectScenario(curProject, returnVal)
}

openScenario <- function(name) {
	changeProjectScenario(curProject, name)
}

saveAsScenario <- function() {
	returnVal <- modalInputDialog("Scenario", "Please enter a new scenario name: ", curScenario)
	if (returnVal == "ID_CANCEL") return()
	if (scenarioExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()
	
	Try(.Tcl(paste('file mkdir "', dirs$curProject(returnVal), '"', sep='')))
	Try(.Tcl(paste('file copy "', dirs$projects(demoProject), '/base/', runScriptName, '" "', dirs$curProject(returnVal),'"',sep='')))
	if(curScenario == "base") {	# we don't copy base inputs since they are default
		Try(.Tcl(paste('file copy "', dirs$gui('comments.csv'), '" "', dirs$curProject(returnVal),'"',sep='')))
		Try(.Tcl(paste('file mkdir "', dirs$curProject(returnVal), '/inputs"', sep='')))
	} else {
		Try(.Tcl(paste('file copy "', dirs$curScenario('comments.csv'), '" "', dirs$curProject(returnVal),'"',sep='')))
		Try(.Tcl(paste('file copy "', dirs$curScenInputs(), '" "', dirs$curProject(returnVal),'"',sep='')))
	}
	Try(.Tcl(paste('file copy "', dirs$curScenOutputs(), '" "', dirs$curProject(returnVal),'"',sep='')))
	
	changeProjectScenario(curProject, returnVal)
}

renameScenario <- function() {
	returnVal <- modalInputDialog("Scenario", "Please enter a new scenario name: ", curScenario)
	if (returnVal == "ID_CANCEL") return()
	if (scenarioExistsWarning(returnVal)) return()
	if (invalidNameWarning(returnVal)) return()
	
	Try(.Tcl(paste('file rename -force "', dirs$curScenario(), '" "', dirs$curProject(returnVal),'"',sep='')))

	changeProjectScenario(curProject, returnVal)
}

deleteScenario <- function() {
	returnVal <- tkmessageBox(parent=tt, title="Warning", icon="warning", type="yesno",
			message=paste("Are you sure you want to delete ", curScenario, "? This will remove all scenario data."))	
 
	if (tclvalue(returnVal) != "yes") return()
		
	Try(.Tcl(paste('file delete -force "', dirs$curScenario(), '"', sep='')))

	changeProjectScenario(demoProject, "base")
}
