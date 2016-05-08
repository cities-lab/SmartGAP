# prefs.r - functions for getting and setting prefs
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

prefs <- NULL

initprefs <- function() {
	if(file.exists(dirs$gui("prefs.csv"))) {
		prefs <<- read.csv(dirs$gui("prefs.csv"), header=T, sep=",", colClasses=rep("character",2))
	} else {
		prefs <<- NULL
	}
}

getPref <- function(name) {
	row = prefs[prefs$name == name,]
	row[1,]$value
}

setPref <- function(name, value) {
	prefs[prefs$name == name,][1,]$value <<- value
	write.csv(prefs, dirs$gui("prefs.csv"), row.names=F)
}

getStartupProjectScenario<- function() {
	initprefs()
	if(is.null(prefs)) {	
		initProjectScenario(demoProject, "base")	
		tkmessageBox(parent=tt, message = 'Error: The preference file "Prefs.csv is missing!', icon = "error")
		return
	}
	proj <- getPref("Startup Project")
	scen <- getPref("Startup Scenario")
	if(file.exists(paste(dirs$projects(proj),scen,runScriptName,sep="/"))) {
		initProjectScenario(proj, scen)	
	} else {
		initProjectScenario(demoProject, "base")	
	}
}

saveStartupProjectScenario<- function() {
	setPref("Startup Project", curProject)
	setPref("Startup Scenario", curScenario)
}
