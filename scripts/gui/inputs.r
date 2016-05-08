# inputs.r - functions for building and wiring up the model inputs
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

actionsButtons <- list()
inputLabels <- list()
inputMarkers <- list()
inputFiles <- NULL

# container
inputsFrame <- NULL

getInputFiles <- function() {
    if(is.null(inputFiles)){
      inputFiles <<- read.csv(dirs$gui("inputs.csv"), header=T, sep=",", colClasses=rep("character",4))
  }
  inputFiles
}

inputExists <- function(iname) {
	inputFiles.df <- getInputFiles()
	inputFile <- inputFiles.df[inputFiles.df$name == iname,]
	scenarioFiles <- dir(dirs$curScenInputs())
	length(scenarioFiles[scenarioFiles == unique(inputFile$filename)]) > 0
}

refreshInputs <- function(container) {
	initInputFilesFrame(container)
	highlightProcessInputFiles()
}

initInputFilesFrame <- function(container, process=NULL) {
    # make sure frame is clear
    clearWidgets(container)

    # make a scrollable frame
    sw <- tkwidget(container,"ScrolledWindow", borderwidth=5)
    inputsFrame <<- tkwidget(sw, "ScrollableFrame")
    inputsFrameID <- tclvalue(tcl(inputsFrame,"getframe"))
    tcl(sw,"setwidget",inputsFrame)
	
    df.inputFiles <- getInputFilesWithDates()
	initComments()
	
	baseFiles <- dir(dirs$baseInputs())

    inputFiles <- sort(df.inputFiles, by = ~ type + name)
	
	minOutputDate <- getMinOutputDate()

    lastInputFile <- NULL
    for(i in 1:nrow(inputFiles)){
        inputFile <- inputFiles[i,]

        # ignore duplicate input files
        # TODO: replace with unique reduction, based on name, above
        if(!is.null(lastInputFile) && inputFile$name == lastInputFile$name){
            next
        }

        # see if we have to insert category label
        if(is.null(lastInputFile) || inputFile$type != lastInputFile$type){
            typeLabel <- tcl("label", paste(inputsFrameID,".","type",i, sep=""))
            tkconfigure(typeLabel, text=inputFile$type, font='InputTypeLabelFont')
            tkgrid(typeLabel, pady=c(0,0), columnspan=4)
        }
        inputFrame <- tcl("frame",paste(inputsFrameID,i,sep="."))
		
		marker <- tcl("label", paste(inputsFrameID,i,1,sep="."))
		tkconfigure(marker, text="", font="InputLabelFont", width=1)
		
		actionsButton <- tcl("button", paste(inputsFrameID,i,2,sep="."))
        tkconfigure(actionsButton, font="ButtonHeightFont", width=8)
		
        # build action buttons	
    	actionCmd <- eval(parse(text=paste("function() inputAction(\"", inputFile$name, "\")", sep="")))
  
		if(isDemoBase()) {	# not allowed to edit Demo base
        	tkconfigure(actionsButton, text="Base", state="disabled")
		} else if (!isBase() && inputExists(inputFile$name)) {
	        tkconfigure(
	            actionsButton, 
	            text="Scenario...", 
	            background="white", 
	            cursor = "hand2",
	            command = actionCmd
        	)
		} else {
	        if(length(baseFiles[baseFiles == inputFile$filename]) > 0) {
	            tkconfigure(
	                actionsButton, 
	                text="Base...", 
	                cursor = "hand2",
	                command = actionCmd
	            )
	        } else {	# missing required base input
	            tkconfigure(
	                actionsButton, 
	                text="Missing...", 
	                cursor = "hand2",
	                background="red", 
	                foreground="white",
	                command = actionCmd
	            )
				if(!isBase()) {
					tkconfigure(actionsButton, state="disabled", cursor = "")
				}
	        }
		}
		if(running) {
			tkconfigure(actionsButton, state="disabled", cursor = "")
		}

	    inputLabel <- tcl("label", paste(inputsFrameID,i,3,sep="."))
	    tkconfigure(inputLabel, text=inputFile$name, font="InputLabelFont", anchor='w', width=50)
			
		# add comment, but not for Base scenarios (unless input is missing)
		if(length(baseFiles[baseFiles == inputFile$filename]) == 0) {	# missing required base input
				comment <- paste('The input file "', inputFile$filename, '" is missing from the base scenario.', sep='')
		}else if (isBase()) {
			comment <- ""
		}
		else {
			comment <- getComment(inputFile$name)
		}
	
		commentLabel <- tcl("label", paste(inputsFrameID,i,4,sep="."))
	    tkconfigure(commentLabel, text=comment, foreground="dark gray", font="CommentFont")
			
		if (!isBase() & nchar(comment) > 0 & !running) {
			tkconfigure(commentLabel, cursor="hand2")
			tkbind(commentLabel, '<Button-1>',  eval(parse(text=paste('function() commentInput("', inputFile$name, '")', sep=''))))
		}
		
		dateLabel <- tcl("label", paste(inputsFrameID,i,5,sep="."))
		tkconfigure(dateLabel, text=inputFile$modDateStr)
		
		if(!is.na(minOutputDate) & !is.na(inputFile$modDate)) {
			if(inputFile$modDate > minOutputDate) {
				tkconfigure(dateLabel, foreground="red")
			}
		}
		

        # arrange things
        tkgrid(marker, row=1, column=1, rowspan=2)
		tkgrid(actionsButton, row=1, column=2, rowspan=2)
        tkgrid(inputLabel, row=1, column=3, sticky="w")
		tkgrid(commentLabel, row=2, column=3, sticky="w")
		tkgrid(dateLabel, row=1, column=4, rowspan=2, sticky="e")

        tkgrid(inputFrame, sticky="w")
        tkbind(inputFrame, "<FocusIn>", function() tcl(inputsFrame,"see",inputFrame))

        # add to global for use in highlightProcessInputFiles
        actionsButtons[[inputFile$name]] <<- actionsButton
        inputLabels[[inputFile$name]] <<- inputLabel
		inputMarkers[[inputFile$name]] <<- marker

        # set up for next loop
        lastInputFile <- inputFile
    }
    tkpack(sw, fill="both", expand="yes")
}

highlightProcessInputFiles <- function(){
    df.inputFiles <- getInputFiles()

    if(is.null(curProcess)) {
		processFiles <- list()
    } else { 
        processFiles <- subset(
            df.inputFiles,
            usedby == curProcess
        )$name
    }

    for(i in 1:length(actionsButtons)) {
        if(names(actionsButtons)[[i]] %in% processFiles){
			#tkconfigure(inputLabels[[i]], background="LightYellow")
			tkconfigure(inputMarkers[[i]], text='*') #, background="yellow")
        } else {
			#tkconfigure(inputLabels[[i]], background="systemButtonFace")
			tkconfigure(inputMarkers[[i]], text='') #, background="systemButtonFace")
        }
    }
}

inputAction <- function (input) {
    inputActionPopupMenu <- tkmenu(inputsFrame, tearoff = FALSE, background="white")
    rootx <- as.integer(tkwinfo("rootx", actionsButtons[[input]]))
    rooty <- as.integer(tkwinfo("rooty", actionsButtons[[input]]))

    if(!(isBase() && !inputExists(input))){ # can't edit if missing from base
      tkadd(inputActionPopupMenu, "command", label="Edit...", command = function() { editInput(input) })
  	}
    tkadd(inputActionPopupMenu, "command", label="Browse...", command = function() { browseInput(input) })
    if(!isBase() && inputExists(input)) { # can only comment, revert scenario files
        tkadd(inputActionPopupMenu, "command", label="Comment...", command = function() { commentInput(input) })
        tkadd(inputActionPopupMenu, "command", label="Revert...", command = function() { revertInput(input) })
    }
    .Tcl(paste("tk_popup", .Tcl.args(inputActionPopupMenu, rootx, rooty)))
}

editInput <- function (input) {
    filename <- inputFiles[inputFiles$name == input,]$filename
	fromBase <- !inputExists(input)

    editCSV(
        fromPath=ifelse(fromBase,dirs$baseInputs(filename),dirs$curScenInputs(filename)),
        toPath=dirs$curScenInputs(filename),
        onSuccess=function() {
			if(fromBase) {
				setComment(input, "Click here to enter a comment describing scenario changes to this input..")
			}
			if(isBase()) {
				refreshMain()
			} else {
				refreshInputs(inputFilesFrame)	
				refreshOutputs(outputFilesFrame)
			}
        }
    )
}

browseInput <- function (input) {
	destFile <- inputFiles[inputFiles$name == input,]$filename
  	destPath <- paste(dirs$curScenInputs(),destFile,sep="/")
	wasBase <- !inputExists(input)
	
	srcPath <- tclvalue(
    tkgetOpenFile(
      defaultextension=".csv", 
      filetypes = "{{CSV Files} {.csv}}",
			initialdir= path.expand("~"),
      initialfile=destFile,
	  parent=tt
      )
    ) 	
	
	if(nchar(srcPath)) {
    if(file.exists(destPath)) {
      returnVal <- tkmessageBox(parent=tt, 
          title="Warning", 
          icon="warning", 
          type="yesno",
          message=paste("The file", quotestr(destFile), "already exists for this scenario. Overwrite?")
      )	
      if (tclvalue(returnVal) == "yes"){
        tcl("file", "delete", "-force", destPath)
      } else {
        return
      }
    }
	
    tcl("file", "copy", srcPath, destPath)
	
    if(wasBase) {	
      setComment(input, "Click here to enter a comment describing scenario changes to this input..")
    }
	if(isBase()) {
		refreshMain()
	} else {
    	refreshInputs(inputFilesFrame)	
		refreshOutputs(outputFilesFrame)
	}
  }
}

revertInput <- function (input) {
	rows = inputFiles[inputFiles$name == input,]
	filename <- rows[1,]$filename
	returnVal <- tkmessageBox(parent=tt, title="Warning", icon="warning", type="yesno",
			message=paste("Revert back to the Base version of", filename, "? The current scenario version of this file will be deleted."))	
	if (tclvalue(returnVal) != "yes") return()
	.Tcl(paste('file delete -force "', dirs$curScenInputs(filename), '"',sep=''))
	
	setComment(input, "")
	refreshInputs(inputFilesFrame)
}

commentInput <- function (input) {
	if (isDemoBase()) return()
	comment <- getComment(input)
	returnVal <- modalInputDialog(title="Input", question="Please enter a comment describing scenario changes to this input: ", 
			entryInit = comment, entryWidth = 64)
	if (returnVal == "ID_CANCEL") return()
	
	setComment(input, returnVal)	
	refreshInputs(inputFilesFrame)
}

# checks to see if no inputs are missing from combined base and scenario inputs
noInputsAreMissing <- function () {
	requiredFiles <- getInputFiles()
	inputs <- unique(c(dir(dirs$baseInputs()), dir(dirs$curScenInputs())))
	if(max(is.na(match(unique(requiredFiles$filename), inputs))) == 1) {
		FALSE
	} else {
		TRUE
	}
}

# do some validation (not null, whitespace, usedby matches process name, input exists in demo base)
checkInputs <- function() {
	if(is.null(inputFiles)) {
		tkmessageBox(parent=tt, message = '"inputFiles" is null!', icon = "error")
		return(FALSE)
	}
	result <- checkDFForHiddenWhitespace(inputFiles)
	if(result != "") {
		tkmessageBox(parent=tt, message = paste('Inputs.csv: hidden whitespace found "', result, '"', sep=''), icon = "error")
	}
	for(process in unique(inputFiles$usedby)) {
		if(length(processes$name[processes$name==process]) == 0){
			tkmessageBox(parent=tt, message = paste('Inputs.csv: process "', process, '" not found.', sep=''), icon = "error")
		}
	}
	for(filename in unique(inputFiles$filename)) {
		if(!file.exists(paste(dirs$projects(demoProject), '/base/inputs/', filename, sep=''))) {
			tkmessageBox(parent=tt, message = paste('Inputs.csv: file "', filename, '" not found in demo project base inputs.', sep=''), icon = "error")
		}
	}
}

getInputFilesWithDates <- function() {
	inputFiles <- getInputFiles()
	inputDates <- file.info(dir(path=dirs$curScenInputs(),full.names=TRUE))
	inputDates$filename <- basename(rownames(inputDates))
	inputDates <- data.frame(inputDates$filename, inputDates$mtime)
	colnames(inputDates) <- c("filename", "scenarioModDate")
	inputFilesWithDates <- merge(inputFiles, inputDates, by.x = "filename", all.x=TRUE)
			
	if(isBase()) {		
		inputFilesWithDates$baseModDate <- inputFilesWithDates$scenarioModDate
		inputFilesWithDates$modDate <- inputFilesWithDates$baseModDate
	} else {	# merge in base input dates where scenario has no input file	
		baseDates <- file.info(dir(path=dirs$baseInputs(),full.names=TRUE))
		baseDates$filename = basename(rownames(baseDates))
		baseDates <- data.frame(baseDates$filename, baseDates$mtime)
		colnames(baseDates) <- c("filename", "baseModDate")
		
		inputFilesWithDates <- merge(inputFilesWithDates, baseDates, by.x = "filename", all.x=TRUE)
		#### ifelse() won't return a date type.   WTF!  We'll have to do it in a loop
		#inputFilesWithDates$modDate = ifelse(is.na(inputFilesWithDates$scenarioModDate), inputFilesWithDates$baseModDate, inputFilesWithDates$scenarioModDate)	
		inputFilesWithDates$modDate <- inputFilesWithDates$baseModDate	
		for(i in 1:nrow(inputFilesWithDates)){
			if(!is.na(inputFilesWithDates[i,]$scenarioModDate)) {
				inputFilesWithDates[i,]$modDate <- inputFilesWithDates[i,]$scenarioModDate
			}
		}
	}
	
	inputFilesWithDates$modDateStr = format(inputFilesWithDates$modDate, "%m/%d/%Y %I:%M %p")
	
	inputFilesWithDates
}

getMaxInputDate <- function() {
	inputFiles <- getInputFilesWithDates()
	if(length(na.omit(inputFiles$modDate) > 0)) {
		max(inputFiles$modDate, na.rm=TRUE)
	} else {
		-Inf
	}
}
