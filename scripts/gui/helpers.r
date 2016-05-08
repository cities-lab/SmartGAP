# helpers.r - helper functions, general utils
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


# all commands that are not implemented yet should call this function
notimplemented <- function(feature) {
    m <- paste(feature, "has not been implemented yet.")
    tkmessageBox(parent=tt, message = m, icon = "error")
}


isDemoProject <- function() { curProject == demoProject }

isDemoBase <- function() { curProject == demoProject && curScenario == "base" }

isBase <- function() { curScenario == "base" }


# adapted from http://www.tek-tips.com/viewthread.cfm?qid=552761&page=57
clearWidgets <- function(container) {
    childrenApply(container, "destroy $child")
}

childrenApply <- function(container, tclcmd) {
    cmd <- paste("foreach child [winfo children ", .Tk.ID(container), "] {
        ", tclcmd, "
    }")
    .Tcl(cmd)
}

# CSV edit dialog
editCSV <- function(fromPath, toPath=fromPath, onSuccess=function() "nothing") {
  # load data from csv into data frame for display/edit
  df.edit <- read.csv(fromPath)
  win <- gwindow(paste("Edit",basename(fromPath)))
  size(win) <- c(700,500)
  # make this a modal window, and position it correctly
  modalWindow(getToolkitWidget(win))
  editTableWidget <- gdf(df.edit, container=win)
  # need to set the background color on the underlying tktable widget
  tktable <- getToolkitWidget(editTableWidget)
  #tcl(tktable, "tag", "configure", "active", foreground='black')
  # save / cancel buttons
  cmd.grp <- ggroup(container=win)
  save.btn <- gbutton(
    container=cmd.grp, 
    text="Save", 
    handler=function(obj) {
        # gdf can be indexed like a data frame
        # so we copy it to another df for saving
        out.df <- editTableWidget[1:nrow(editTableWidget),]
        write.csv(out.df, file=toPath, row.names=F)
        dispose(win)
        onSuccess()
    }
  )
  cancel.btn <- gbutton(
    container=cmd.grp, 
    text="Cancel", 
    handler=function(...) dispose(win)
  )
}

# borrowed from http://rwiki.sciviews.org/doku.php?id=tips:data-frames:sort
sort.data.frame <- function(x, by){
    # Author: Kevin Wright
    # with some ideas from Andy Liaw
    # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
 
    # x: A data.frame
    # by: A one-sided formula using + for ascending and - for descending
    #     Sorting is left to right in the formula
  
    # Useage is:
    # library(nlme);
    # data(Oats)
    # sort(Oats, by= ~nitro-Variety)
 
    if(by[[1]] != "~")
        stop("Argument 'by' must be a one-sided formula.")
 
    # Make the formula into character and remove spaces
    formc <- as.character(by[2]) 
    formc <- gsub(" ", "", formc) 
    # If the first character is not + or -, add +
    if(!is.element(substring(formc, 1, 1), c("+", "-")))
        formc <- paste("+", formc, sep = "")
 
    # Extract the variables from the formula
    vars <- unlist(strsplit(formc, "[\\+\\-]"))    
    vars <- vars[vars != ""] # Remove any extra "" terms
 
    # Build a list of arguments to pass to "order" function
    calllist <- list()
    pos <- 1 # Position of + or -
    for(i in 1:length(vars)){
        varsign <- substring(formc, pos, pos)
        pos <- pos + 1 + nchar(vars[i])
        if(is.factor(x[, vars[i]])){
            if(varsign == "-") {
                calllist[[i]] <- -rank(x[, vars[i]])
            } else {
                calllist[[i]] <- rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") {
                calllist[[i]] <- -x[, vars[i]]
            } else {
                calllist[[i]] <- x[,vars[i]]
            }
        }
    }
    return(x[do.call("order", calllist), ])
}

is.interactive <- function() {
    is.null(opt$scenario)
}

# adapted from http://www.sciviews.org/_rgui/tcltk/ModalDialog.html
modalInputDialog <- function(title, question, entryInit, entryWidth = 20, returnValOnCancel = "ID_CANCEL") {
    dlg <- modalWindow()
    tktitle(dlg) <- title
	textEntryVarTcl <- tclVar(paste(entryInit))
	textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
			textvariable = textEntryVarTcl)
	tkgrid(tklabel(dlg, text = "       "))
	q <- tklabel(dlg, text = question)
	if(entryWidth < 32) {
		tkgrid(q, textEntryWidget)
		tkgrid.configure(q, padx=5)
		tkgrid.configure(textEntryWidget, padx=c(0,5))
	} else {
		tkgrid(q, padx=5, sticky="w", columnspan=2)
		tkgrid(textEntryWidget, padx=c(5,5), pady=c(10, 0), columnspan=2)
	}
	tkgrid(tklabel(dlg, text = "       "))
	tkfocus(textEntryWidget)
	#tkselection.set(textEntryWidget, "0.0", "end") ### TODO: how to select all of text?
	ReturnVal <- returnValOnCancel
	
	onOK <- function() {
		ReturnVal <<- trim(tclvalue(textEntryVarTcl))
		tkgrab.release(dlg)
		tkdestroy(dlg)
		tkfocus(tt)
	}
	onCancel <- function() {
		ReturnVal <<- returnValOnCancel
		tkgrab.release(dlg)
		tkdestroy(dlg)
		tkfocus(tt)
	}
	OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
	Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
	tkgrid(OK.but, Cancel.but)
	tkgrid(tklabel(dlg, text = "    "))
	
	tkfocus(dlg)
	tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
	tkbind(textEntryWidget, "<Return>", onOK)
	tkwait.window(dlg)
	
	return(ReturnVal)
}

# from http://www.sciviews.org/_rgui/tcltk/ExceptionHandling.html
Try <- function(expr) {
	if (data.class(result<-try(expr,TRUE))=="try-error")
		tkmessageBox(parent=tt, title="An error has occured!",message=as.character(result),icon="error",type="ok")
	else
		return (result)
}

# wraps str in quotes (e.g., for .Tcl cmd)
quotestr <- function (str) paste("\"",str,"\"", sep="")

# trim space from both ends of a string
trim <- function (str) gsub("(^ +)|( +$)", "", str)

# is name valid for our purposes (e.g. Project name or Scenario name, a valid Windows directory name)
isValidName <- function (name) length(grep('^[[:alnum:]_\\s-]+$', name, perl=TRUE)) > 0	# allow alphanumeric, underscore, space, dash

invalidNameWarning <- function(name) {
	if(!isValidName(name)){
		tkmessageBox(parent=tt, message = paste('"', name, '" contains invalid characters. Please use only letters, numbers, underscores, spaces, and dashes.',sep=""), icon = "error")
		return(TRUE)
	}
	return(FALSE)
}

# given a start time returns elapsed time as a string  
elapsedTime <- function(startTime) {
	if(is.null(startTime)) {
		return("")
	}
	DT <- difftime(Sys.time(), startTime, units='secs')[[1]]  #(There's got to be a better way!)
	h <- floor(DT/3600)
	m <- floor((DT - h*3600)/60)
	s <- round(DT - h*3600 - m*60)
	placeholder <- ISOdatetime(year=2000, month=1, day=1, hour=h, min =m, sec=s)
	if(h > 0) {
		format(placeholder, "%H:%M:%S")   
	} else {
		format(placeholder, "%M:%S")
	}
}

# function to display a modal dialog
modalWindow <- function(dlg=tktoplevel()) {
	xWin <- as.integer(tkwinfo("rootx",tt)) + 25
	yWin <- as.integer(tkwinfo("rooty",tt)) + 100
	tkwm.deiconify(dlg)
	tkwm.geometry(dlg, paste('+', xWin, '+', yWin, sep=''))
	tkgrab.set(dlg)
	tkfocus(dlg)
    tcl("update") 
    dlg
}

modalDialog <- function(title, msg) {
    dlg <- modalWindow()
	tkwm.title(dlg, title)
    tkgrid(tklabel(dlg, text=msg, width=50, height=5))
    dlg
}

closeModalDialog <- function(dlg) {
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
}

checkForHiddenWhitespace <- function(df) {
	
}

# given a complete data frame of strings, check for hidden whitespace at start and end of strings
checkDFForHiddenWhitespace <- function(df) {
	c <- ncol(df)
	r <- nrow(df)
	for(j in 1:c) {
		for(i in 1:r) {
			if(nchar(df[i,j]) != nchar(trim(df[i,j])))
				return(df[i,j])
		}
	}
	return("")
}

# our foreign key constraints in our csv files are not always synced up, check them for errors
checkCSVKeyConstraints <- function() {	
	checkProcesses()
	checkParameters()
	checkInputs()
	checkOutputs()
	#checkComments() # comments are scenario specific so check every time we change scenario
}

# as.TclList & tkcombobox: Modified by Chris Hoffman from http://www.sciviews.org/_rgui/tcltk/DropDown.html
# combobox requires tclRequire("BWidget")

# Define an S3 generic function and a method to convert an R list to a Tcl list.
as.TclList <- function(object,...) UseMethod("as.TclList")
as.TclList.list <- function(stringList) {
	result <-"{"
	for (i in (1:length(stringList)))
		result <- paste(result,"{",stringList[[i]],"} ",sep="")
	result <- paste(result,"}",sep="")
	result
}

tkcombobox <- function(container, items, selected=NULL, command, label) {
	comboBox <- .Tk.subwin(container)
	
	# The following Tcl wrapper code merely passes the choice value to the predefined function
	cmd <- paste("proc", 
			paste("comboChanged", .Tk.ID(comboBox), sep=""),
			"{ comboBox } {
					set label {", label, "}
					set choice [$comboBox get]
					", .Tcl.callback(command), "
					}"
	)
	.Tcl(cmd)
	
	itemsTclList <- as.TclList(items)
	
	cmd <- paste(
			"ComboBox",.Tk.ID(comboBox),
			"-editable false",
			"-width",10,
			"-values",itemsTclList,
			"-modifycmd {",
			# NB: this is *not* a typo; the .Tk.ID(comboBox) statements are *both* needed
			paste("comboChanged",.Tk.ID(comboBox),sep=""), .Tk.ID(comboBox),
			"}"
	)
	.Tcl(cmd)
	# pre-select option if specified
	# adapted from http://docs.activestate.com/activetcl/8.5/bwidget/ComboBox.html#setvalue
	if(!is.null(selected)){
		cmd <- paste(.Tk.ID(comboBox), "setvalue", paste("@",which(items == selected) - 1,sep=""))
		.Tcl(cmd)
	}
	comboBox
}





