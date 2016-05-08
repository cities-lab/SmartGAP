# outputs.r - functions for displaying model outputs
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

outputFiles <- NULL
outputFileControls <- NULL

# container
outputsFrame <- NULL

getOutputFiles <- function() {
    if(is.null(outputFiles)){
      outputFiles <<- read.csv(dirs$gui("outputs.csv"), header=T, sep=",", as.is=T)
  }
  outputFiles
}

refreshOutputs <- function(container) {
	initOutputFilesFrame(container)
	setEnableExistingOutputFiles()
}

initOutputFilesFrame <- function(container) {
    outputFileControls <<- list()

    # make sure frame is clear
    clearWidgets(container)
	outputsFrame <<- tkframe(container, borderwidth=5) 
	tkgrid(outputsFrame)

    outputWidgets <- list()
	
    outputFiles <- getOutputFilesWithDates()
	
    outputFiles <- sort(outputFiles, by = ~ type + name)
	
	maxInputDate <- getMaxInputDate()
	
    lastOutputFile <- NULL
    for(i in 1:nrow(outputFiles)){
        outputFile <- outputFiles[i,]

        # ignore duplicate output files
        # TODO: replace with unique reduction, based on name, above
        if(!is.null(lastOutputFile) && outputFile$name == lastOutputFile$name){
            next
        }

        # see if we have to insert category label
        if(is.null(lastOutputFile) || outputFile$type != lastOutputFile$type){
            typeLabel <- tklabel(outputsFrame, text=outputFile$type, font='OutputCategoryLabelFont', width=80)
            tkgrid(typeLabel, pady=c(8,0), sticky='w', columnspan=4)
        }

        #outputFrame <- tkframe(outputsFrame)
        #tkgrid(outputFrame, sticky='w')

        # controls
        outputViewButton <- tkbutton(
            outputsFrame, 
            width=8,
            text="View",
            command = eval(parse(text=paste("function() viewOutputFile(", quotestr(outputFile$name), ")"))),
			cursor="hand2",
            font="ButtonHeightFont"
        )
        outputExportButton <- tkbutton(
            outputsFrame, 
            width=8,
            text="Export",
            command = eval(parse(text=paste("function() exportOutputFile(\"",outputFile$name, "\")", sep=""))),
			cursor="hand2",
            font="ButtonHeightFont"
        )
        outputLabel <- tklabel(outputsFrame, text=outputFile$name, anchor='w', width=48)
		outputDateLabel <- tklabel(outputsFrame, text=outputFile$modDateStr, width=17)
        outputFileControls[[outputFile$name]] <<- list(outputViewButton, outputExportButton, outputLabel, outputDateLabel)
		
		if(!is.na(maxInputDate) & !is.na(outputFile$modDate)) {
			if(outputFile$modDate < maxInputDate) {
				tkconfigure(outputDateLabel, foreground="red")
			}
		}	

        # arrange things
        tkgrid(outputViewButton, outputExportButton, outputLabel, outputDateLabel)
        tkgrid.configure(outputViewButton, sticky='w')
        tkgrid.configure(outputExportButton, sticky='w')
		tkgrid.configure(outputLabel, sticky='w')
		tkgrid.configure(outputDateLabel, sticky='e')
		
        # set up for next loop
        lastOutputFile <- outputFile
    }
}

setEnableExistingOutputFiles <- function() {
    #This function assumes initOutputFilesFrame() has been called to initialize outputFileControls[]
    outputFiles <- getOutputFiles()
    for(i in 1:nrow(outputFiles)) {
        outputFile <- outputFiles[i,]
        targetFile <- dirs$curScenOutputs(outputFile$filename)
        if (file.exists(targetFile)) {
            lapply(outputFileControls[[outputFile$name]], function(c) tkconfigure(c, state="normal", cursor="hand2"))
        } else {
            lapply(outputFileControls[[outputFile$name]], function(c) tkconfigure(c, state="disabled", cursor=""))
        }
    }
}

viewOutputFile <- function(name){
    view.df <- assignLoadForOutputFile(name)
    view.df <- cbind(Categories=names(view.df),Values=round(view.df,digits=2))
    win <- gwindow(paste("View", name))
    size(win) <- c(500,300)
    modalWindow(getToolkitWidget(win))
    view.gdf <- gdf(view.df, container=win)
}

exportOutputFileToTempDir <- function(name){
    tmpPath <- paste(dirs$curScenOutputs(),"viewoutput.csv",sep="/")
    outputRData <- assignLoadForOutputFile(name)
    write.csv(outputRData, file=tmpPath)
    tmpPath
}

assignLoadForOutputFile <- function(name) {
    outputFiles <- getOutputFiles()
    outputFile <- outputFiles[outputFiles$name == name,]
    outputPath <- paste(dirs$curScenOutputs(),outputFile$filename,sep="/")
    assignLoad(file=outputPath)
}

# taken from GreenSTEP.r
assignLoad <- function(filename){
    load(filename)
    get(ls()[ls() != "filename"])
}

# exports output to user-specified location
exportOutputFile <- function (name) {
	outPath <- tclvalue(
    tkgetSaveFile(
		parent=tt,
      initialdir=path.expand("~"), # home directory
      initialfile=paste(name,".csv",sep=""),
      defaultextension=".csv", 
      filetypes = "{{CSV Files} {.csv}}"
    ) 	
  )

  # proceed only if user selected a path
	if (nchar(outPath)) {
        # put up a modal dialog during this long-running set of operations
        dlg <- modalDialog(
            title="Exporting...", 
            msg=paste("Exporting", paste(name,".csv",sep=""))
        )
        tcl("update")
        tmpPath <- exportOutputFileToTempDir(name)
        tcl("file", "copy", "-force", tmpPath, outPath)
        closeModalDialog(dlg)
        #tcl("exec", "explorer", paste("/select",normalizePath(outPath),sep=",")) ###TODO works but throws error

    }
}


# do some validation (not null, whitespace, usedby matches process name)
checkOutputs <- function() {
	if(is.null(outputFiles)) {
		tkmessageBox(parent=tt, message = '"outputFiles" is null!', icon = "error")
		return(FALSE)
	}
	result <- checkDFForHiddenWhitespace(outputFiles)
	if(result != "") {
		tkmessageBox(parent=tt, message = paste('Outputs.csv: hidden whitespace found "', result, '"', sep=''), icon = "error")
	}
	for(process in unique(outputFiles$usedby)) {
		if(length(processes$name[processes$name==process]) == 0){
			tkmessageBox(parent=tt, message = paste('Outputs.csv: process "', process, '" not found.', sep=''), icon = "error")
		}
	}
}

deleteOutputFiles <- function(process=NULL) {
    # merge outputs with processes, so we have process sequence available
    outputsAndProcesses <- merge(
        getOutputFiles(),
        getProcesses(),
        by.x = "usedby",
        by.y = "name"
    )

    if(!is.null(process)){
        # reduce to outputs for the current process and all subsequent
        lastSequence <- getProcessSequence(process)
        outputsAndProcesses <- subset(outputsAndProcesses, sequence >= lastSequence)
    }
	if(nrow(outputsAndProcesses) > 0) {
		for(i in 1:nrow(outputsAndProcesses)){
			outputFile <- dirs$curScenOutputs(outputsAndProcesses[i,]$filename)
			Try(.Tcl(paste('file delete -force "', outputFile, '"', sep='')))
		}
	}
}

updateOutputsStep <- function (process) {
	if(!is.null(process)) {
		outputFiles <- subset(getOutputFilesWithDates(), usedby == process)
		if(nrow(outputFiles) > 0) {
			for(i in 1:nrow(outputFiles)){
				tkconfigure(outputFileControls[[outputFiles[i,]$name]][[4]], text=outputFiles[i,]$modDateStr)
			}				
			setEnableExistingOutputFiles()
		}
	}
}


getOutputFilesWithDates <- function() {
	outputFiles <- getOutputFiles()
	outputDates <- file.info(dir(path=dirs$curScenOutputs(),full.names=TRUE))
	outputDates$filename = basename(rownames(outputDates))
	outputDates <- data.frame(outputDates$filename, outputDates$mtime)
	colnames(outputDates) <- c("filename", "modDate")
	outputFilesWithDates <- merge(outputFiles, outputDates, by.x = "filename", all.x=TRUE)
	outputFilesWithDates$modDateStr= ifelse(is.na(outputFilesWithDates$modDate), "", format(outputFilesWithDates$modDate, "%m/%d/%Y %I:%M %p"))
	outputFilesWithDates
}

getMinOutputDate <- function() {
	outputDates <- getOutputFilesWithDates()
	if(length(na.omit(outputDates$modDate) > 0)) {
		min(outputDates$modDate, na.rm=TRUE)
	} else {
		Inf
	}
}

