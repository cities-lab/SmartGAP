# comments.r - functions for getting and setting comments (inputs, project/scenario)
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

comments <- NULL

initComments <- function() {
	if(file.exists(dirs$curScenario("comments.csv"))) {
		comments <<- read.csv(dirs$curScenario("comments.csv"), header=T, sep=",", colClasses=rep("character",2))
	} else {
		comments <<- NULL
	}
}

getComment <- function(name) {
	row = comments[comments$name == name,]
	row[1,]$comment
}

setComment <- function(name, comment) {
	comments[comments$name == name,][1,]$comment <<- comment
	write.csv(comments, dirs$curScenario("comments.csv"), row.names=F)
}


# do some validation (not null, whitespace, name exists in inputs name)
checkComments <- function() {
	if(is.null(comments)) {
		tkmessageBox(parent=tt, message = '"comments" is null!', icon = "error")
		return(FALSE)
	}
	result <- checkDFForHiddenWhitespace(comments)
	if(result != "") {
		tkmessageBox(parent=tt, message = paste('Comments.csv: hidden whitespace found "', result, '"', sep=''), icon = "error")
	}
	for(input in unique(comments$name)) {
		if(input != 'scenario') {
			if(length(inputFiles$name[inputFiles$name==input]) == 0){
				tkmessageBox(parent=tt, message = paste('Comments.csv: input "', input, '" not found.', sep=''), icon = "error")
			}
		}
	}
}