# parameters.r - functions for handling model parameters (at the project level) 
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

parameterFileMap <- NULL		# 2 columns: process name, parameter file name

initParameters <- function() {
	if(file.exists(dirs$gui("parameters.csv"))) {
		parameterFileMap <<- read.csv(dirs$gui("parameters.csv"), header=T, sep=",", colClasses=rep("character",2))
	} else {
		parameterFileMap <<- NULL
	}
}

initProjectParametersMenu <- function () {
	initParameters()
	tkdelete(projectParametersMenu, 0, 9999)	# remove all old entries
	processes <- getProcesses()
	for (proc in processes$name) {			
		parameterFiles <- subset(parameterFileMap, process == proc)$filename
		projectParametersFileMenu <<- tkmenu(projectParametersMenu, tearoff = FALSE)	
		for (filename in parameterFiles) {		
			tkadd(projectParametersFileMenu, "command", label = filename, 
					command = eval(parse(text=paste('function(){editParameters("',filename,'")}',sep="")))) 
		}
		tkadd(projectParametersMenu, "cascade", label = proc, menu = projectParametersFileMenu)	
	}
}

editParameters <- function (filename) {
	
	returnVal <- tkmessageBox(parent=tt, title="Warning", icon="warning", type="yesno",
		message=paste("Warning, the default parameters have been derived from documented research sources.",
		"Editing modeling parameters should be based only on research pertaining to local data sources and may result in unpredictable results.",  
		"Are you sure you want to continue?"))
	
	if (tclvalue(returnVal) != "yes") return()
	
	editCSV(dirs$parameters(filename)) 
}

# do some validation (not null,  whitespace, process matches process name, parameter file exists in project)
checkParameters <- function() {
	if(is.null(parameterFileMap)) {
		tkmessageBox(parent=tt, message = '"parameterFileMap" is null!', icon = "error")
		return(FALSE)
	}
	result <- checkDFForHiddenWhitespace(parameterFileMap)
	if(result != "") {
		tkmessageBox(parent=tt, message = paste('Parameters.csv: hidden whitespace found "', result, '"', sep=''), icon = "error")
	}
	for(process in unique(parameterFileMap$process)) {
		if(length(processes$name[processes$name==process]) == 0){
			tkmessageBox(parent=tt, message = paste('Parameters.csv: process "', process, '" not found.', sep=''), icon = "error")
		}
	}
	for(filename in unique(parameterFileMap$filename)) {
		if(!file.exists(dirs$parameters(filename))) {
			tkmessageBox(parent=tt, message = paste('Parameters.csv: file "', filename, '" not found in project.', sep=''), icon = "error")
		}
	}
}
