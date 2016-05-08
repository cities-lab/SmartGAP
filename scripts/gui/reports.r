# reports.r - functions for generating model reports
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

scenarioCheckboxes <- list()
scenariosChecked <- list()
selectedScenarios <- NULL
scenarioOutputsExistance <- list() # for each scenario a logical vector of which outputs exist (some scenarios may not have been run to completion)
scenarioComparable <- list() # for each scenario : -1 if no outputs exist, 0 if some exist, 1 if all exist
comparableScenariosExist <- NULL
metricRadios <- list()
metricValue <- tclVar("Accidents")
aggregationRadios <- list()
aggregationValue <- tclVar("all")
measureRadios <- list()
measureValue <- tclVar("comma")
Metrics <- NULL	# object which defines the relationships between the metrics, the categorization used for those metrics, and their number formats
	
#build the "Metrics" object which defines the relationships between the metrics, the categorization used for those metrics, and their number formats
buildMetrics <- function() {
	Metrics <<- subset(getOutputFiles(), reportable=1)
	Metrics <<- sort(Metrics, by = ~ type + name)
}

refreshReportsFrame <- function (container) {
	if(initReportsFrame(container)) {
		setEnableReportsFrame()		
	}
}

initReportsFrame <- function(container) {
	
	clearWidgets(container)
	
	buildMetrics()
	buildScenarioOutputsExistance()
	
	topframe <- tkframe(container)
	tkgrid(topframe)
	
	# if no output exists at all then throw up message and return
	if(!comparableScenariosExist) {
		tkgrid(tklabel(topframe, text="No results found. At least one scenario needs to be run first.", font="Message", foreground="gray"), padx=50, pady=50)	
		return(FALSE)
	}
	
	# scenarios
	scenariosframe <- tkframe(topframe, borderwidth=1, relief="ridge")
	tkgrid(tklabel(scenariosframe,text="Scenarios", font="ReportControlsFont"))
	
	partialOutputExists <- FALSE
	for (scenario in scenarios) {
		comparable <- scenarioComparable[[scenario]]
		scenariosChecked[[scenario]] <<- tclVar("0")
		if(comparable == 0) {
			partialOutputExists <- TRUE
			cb <- tkcheckbutton(scenariosframe, text=paste(scenario, "*"), command=setEnableReportsFrame)
		} else {
			cb <- tkcheckbutton(scenariosframe, text=scenario, command=setEnableReportsFrame)		
		}
		tkconfigure(cb, variable=scenariosChecked[[scenario]])
		if(comparable == -1) {
			tkconfigure(cb, state="disabled")
		}
		tkgrid(cb, sticky='w')
		scenarioCheckboxes[[scenario]] <<- cb
	}
	if (partialOutputExists) {
		tkgrid(tklabel(scenariosframe, text="* missing outputs", font='CommentFont'))
	}

	# metrics
	metricsframe <- tkframe(topframe, borderwidth=1, relief="ridge")
	tkgrid(tklabel(metricsframe,text="Metric", font="ReportControlsFont"))
	
	lastType <- NULL
	for(i in 1:nrow(Metrics)){
		type <- as.character(Metrics[i,]$type)
		# see if we have to insert category label
		if(is.null(lastType) || type != lastType){
			typeLabel <- tklabel(metricsframe, text=type, font='CommentFont', foreground="gray")
			tkgrid(typeLabel)
		}
		metric <- as.character(Metrics[i,]$metric)
		rb <- tkradiobutton(metricsframe, text=metric, command=setEnableAggregationRadios)
		tkconfigure(rb,variable=metricValue, value=metric)
		tkgrid(rb, sticky="w")
		metricRadios[[metric]] <<- rb
		lastType <- type
	}

	# aggregation
	aggregationframe <- tkframe(topframe, borderwidth=1, relief="ridge")
	tkgrid(tklabel(aggregationframe,text="Aggregation", font="ReportControlsFont"))
	rbAllPlaceTypes <- tkradiobutton(aggregationframe, text = "All")
	tkconfigure(rbAllPlaceTypes, variable=aggregationValue, value="all")
	aggregationRadios[["all"]] <<- rbAllPlaceTypes
	rbPlaceTypes <- tkradiobutton(aggregationframe, text = "Place Types")
	tkconfigure(rbPlaceTypes, variable=aggregationValue, value="place")
	aggregationRadios[["place"]] <<- rbPlaceTypes
	rbAreaTypes <- tkradiobutton(aggregationframe, text = "Area Types")
	tkconfigure(rbAreaTypes, variable=aggregationValue, value="area")
	aggregationRadios[["area"]] <<- rbAreaTypes
	rbDevelopmentTypes <- tkradiobutton(aggregationframe, text = "Development Types")
	tkconfigure(rbDevelopmentTypes, variable=aggregationValue, value="development")
	aggregationRadios[["development"]] <<- rbDevelopmentTypes
	rbVehicleTypes <- tkradiobutton(aggregationframe, text = "Vehicle Type")
	tkconfigure(rbVehicleTypes, variable=aggregationValue, value="vehtype")
	aggregationRadios[["vehtype"]] <<- rbVehicleTypes
	rbAccidentSeverities <- tkradiobutton(aggregationframe, text = "Accident Severity")
	tkconfigure(rbAccidentSeverities, variable=aggregationValue, value="accsev")
	aggregationRadios[["accsev"]] <<- rbAccidentSeverities
	rbIncomeGroups <- tkradiobutton(aggregationframe, text = "Income Group")
	tkconfigure(rbIncomeGroups, variable=aggregationValue, value="incgrp")
	aggregationRadios[["incgrp"]] <<- rbIncomeGroups
	lapply(aggregationRadios, function(c) tkgrid(c, sticky="w"))
	
	# measure
	measureframe <- tkframe(topframe, borderwidth=1, relief="ridge")
	tkgrid(tklabel(measureframe,text="Measure", font="ReportControlsFont"))
	rbNumber <- tkradiobutton(measureframe, text = "Number")
	tkconfigure(rbNumber, variable=measureValue, value="comma")
	measureRadios[["comma"]] <<- rbNumber
	rbPercentage <- tkradiobutton(measureframe, text = "Percentage")
	tkconfigure(rbPercentage, variable=measureValue, value="percentage")
	measureRadios[["percentage"]] <<- rbPercentage
	rbIndex100 <- tkradiobutton(measureframe, text = "Index (100)")
	tkconfigure(rbIndex100, variable=measureValue, value="index100")
	measureRadios[["index100"]] <<- rbIndex100
	rbIndex0 <- tkradiobutton(measureframe, text = "Index (0)")
	tkconfigure(rbIndex0, variable=measureValue, value="index0")
	measureRadios[["index0"]] <<- rbIndex0
	lapply(measureRadios, function(c) tkgrid(c, sticky="w"))
	
	#  plot and export button
	btnframe <- tkframe(topframe)
	plotButton <- tkbutton(btnframe, text="Plot", command=plotButtonClicked, cursor="hand2")
	exportButton <- tkbutton(btnframe, text="Export ...", command=exportButtonClicked, cursor="hand2")
	tkgrid(plotButton, exportButton)
	tkgrid.configure(plotButton, sticky="nw", padx = 15, pady = 15)
	tkgrid.configure(exportButton, sticky="nw", padx = 15, pady = 15)
	
	#tkgrid(scenariosframe, metricsframe, aggregationframe, sticky='n', padx = 15, pady = 15)
	tkgrid(scenariosframe, row=1, column=1, rowspan=3, sticky='n', padx = 15, pady = 15)
	tkgrid(metricsframe, row=1, column=2, rowspan=3, sticky='n', padx = 15, pady = 15)
	tkgrid(aggregationframe, row=1, column=3, sticky='nw', padx = 15, pady = 15)
	tkgrid(measureframe, row=2, column=3, sticky='nw', padx = 15, pady = 15)
	tkgrid(btnframe, row=3, column=3, padx = 15, pady = 15)	
	
	return(TRUE)
}

setEnableReportsFrame <- function() {
	setEnableMetricRadios()
	setEnableAggregationRadios()
}

setEnableMetricRadios <- function() {
	updateSelectedScenarios()
	availableMetrics <- match(Metrics$filename, Metrics$filename)
	if (length(selectedScenarios) > 0) {
		for (scenario in selectedScenarios) {
			availableMetrics <- availableMetrics & scenarioOutputsExistance[[scenario]] 
		}
	}
	firstEnabled <- 0
	resetSelected <- FALSE
	for (i in 1:length(Metrics$filename)) {
		m <- as.character(Metrics$metric[i])
		if (is.na(availableMetrics[i])) {
			tkconfigure(metricRadios[[m]], state="disabled")
			if(tclvalue(metricValue) == 0 || tclvalue(metricValue) == m) resetSelected <- TRUE
		} else {
			tkconfigure(metricRadios[[m]], state="normal")
			if (firstEnabled == 0) firstEnabled <- m
		}
	}
	if(resetSelected) tclvalue(metricValue) <- firstEnabled	# if we disabled the current selection then change it to first enabled radio
	
}

setEnableAggregationRadios <- function() {
	metric <- tclvalue(metricValue)
	if (metric == 0) {
		tkmessageBox(parent=tt, message = "Unable to compare results. The selected scenarios do not share common outputs.", icon = "error")
		return
	}
	category <- Metrics$category[Metrics$metric == metric]
	if (category=="Pt") {
		tkconfigure(aggregationRadios[["all"]], state="normal")
		tkconfigure(aggregationRadios[["place"]], state="normal")
		tkconfigure(aggregationRadios[["area"]], state="normal")
		tkconfigure(aggregationRadios[["development"]], state="normal")
		tkconfigure(aggregationRadios[["vehtype"]], state="disabled")
		tkconfigure(aggregationRadios[["accsev"]], state="disabled")
		tkconfigure(aggregationRadios[["incgrp"]], state="disabled")
		if (tclvalue(aggregationValue) %in% c("vehtype","accsev","incgrp")){
			tclvalue(aggregationValue) <- "all"
		}
	} else if (category=="Ma") {
		tkconfigure(aggregationRadios[["all"]], state="normal")
		tkconfigure(aggregationRadios[["place"]], state="disabled")
		tkconfigure(aggregationRadios[["area"]], state="disabled")
		tkconfigure(aggregationRadios[["development"]], state="disabled")
		tkconfigure(aggregationRadios[["vehtype"]], state="disabled")
		tkconfigure(aggregationRadios[["accsev"]], state="disabled")
		tkconfigure(aggregationRadios[["incgrp"]], state="disabled")
		tclvalue(aggregationValue) <- "all"
	}else if (category=="MaTy") {
		tkconfigure(aggregationRadios[["all"]], state="normal")
		tkconfigure(aggregationRadios[["place"]], state="disabled")
		tkconfigure(aggregationRadios[["area"]], state="disabled")
		tkconfigure(aggregationRadios[["development"]], state="disabled")
		tkconfigure(aggregationRadios[["vehtype"]], state="normal")
		tkconfigure(aggregationRadios[["accsev"]], state="disabled")
		tkconfigure(aggregationRadios[["incgrp"]], state="disabled")
		if (tclvalue(aggregationValue) != "all" && tclvalue(aggregationValue) != "vehtype") {
			tclvalue(aggregationValue) <- "all"
		}
	}else if (category=="As") {
		tkconfigure(aggregationRadios[["all"]], state="normal")
		tkconfigure(aggregationRadios[["place"]], state="disabled")
		tkconfigure(aggregationRadios[["area"]], state="disabled")
		tkconfigure(aggregationRadios[["development"]], state="disabled")
		tkconfigure(aggregationRadios[["vehtype"]], state="disabled")
		tkconfigure(aggregationRadios[["accsev"]], state="normal")
		tkconfigure(aggregationRadios[["incgrp"]], state="disabled")
		if (tclvalue(aggregationValue) != "all" && tclvalue(aggregationValue) != "accsev") {
			tclvalue(aggregationValue) <- "all"
		}
	}else if (category=="Ig") {
		tkconfigure(aggregationRadios[["all"]], state="normal")
		tkconfigure(aggregationRadios[["place"]], state="disabled")
		tkconfigure(aggregationRadios[["area"]], state="disabled")
		tkconfigure(aggregationRadios[["development"]], state="disabled")
		tkconfigure(aggregationRadios[["vehtype"]], state="disable")
		tkconfigure(aggregationRadios[["accsev"]], state="disabled")
		tkconfigure(aggregationRadios[["incgrp"]], state="normal")
		if (tclvalue(aggregationValue) != "all" && tclvalue(aggregationValue) != "incgrp") {
			tclvalue(aggregationValue) <- "all"
		}	
	}
}

buildScenarioOutputsExistance <- function () {
	comparableScenariosExist <<- FALSE
	numReportableOutputs <- length(Metrics$filename)
	scenarioOutputsExistance <<- list()
	for (scenario in scenarios) {
		outputs <- dir(paste(dirs$curProject(scenario),"outputs", sep="/"))
		scenarioOutputsExistance[[scenario]] <<- match(Metrics$filename, outputs)
		numScenarioOuputs <- length(na.omit(scenarioOutputsExistance[[scenario]]))
		if(numScenarioOuputs == 0) {
			scenarioComparable[[scenario]] <<- -1
		} else if (numScenarioOuputs == numReportableOutputs) {
			scenarioComparable[[scenario]] <<- 1
			comparableScenariosExist <<- TRUE
		} else {
			scenarioComparable[[scenario]] <<- 0	
			comparableScenariosExist <<- TRUE		
		}
	}	
}


plotButtonClicked <- function() {
	if(validInputs())
		showPlot()
}

updateSelectedScenarios <- function() {
	selectedScenarios <<- NULL
	
	for (scenario in scenarios) {
		if (tclvalue(scenariosChecked[[scenario]]) == "1")
			selectedScenarios <<- c(selectedScenarios, scenario)
	}
}

validInputs <- function() {
	updateSelectedScenarios()
	
	if(is.null(selectedScenarios)) {
		tkmessageBox(parent=tt, message = "Please select at least one scenario.", icon = "error")
		return(FALSE)
	}
	metric <- tclvalue(metricValue)
	if (metric == 0) {
		tkmessageBox(parent=tt, message = "Unable to compare results. The selected scenarios do not share common outputs.", icon = "error")
		return(FALSE)
	}
	
	return(TRUE)
}

showPlot <- function() {
	title <- paste(curProject,", ", format(Sys.time(), "%m/%d/%Y %I:%M %p"))
	w <- tktoplevel()
	tkwm.title(w, title)
	winX <- as.integer(tkwinfo("rootx",tt)) + 5
	winY <- as.integer(tkwinfo("rooty",tt)) - 20
	winWidth <- 800
	winHeight <- 600
	tkwm.deiconify(w)
	tkwm.geometry(w, paste(winWidth, 'x', winHeight, '+', winX, '+', winY, sep=''))
	tkwm.resizable(w, FALSE, FALSE)	
	img <- tkrplot(w,hscale=2.0,vscale=1.5, fun=makeChart)
	tkgrid(img)
	tkfocus(w)
}


exportButtonClicked <- function() {
	if(validInputs())
		exportPlot()
}
# exports plot to user-specified location
exportPlot <- function (name) {
	outPath <- tclvalue(
			tkgetSaveFile(
					parent=tt,
					initialdir=path.expand("~"), # home directory
					initialfile=paste(curProject,".pdf",sep=""),
					defaultextension=".pdf", 
					filetypes = " {{jpeg Files} {.jpeg}}
							{{png Files} {.png}}
							{{bmp Files} {.bmp}}
							{{tiff Files} {.tiff}}
							{{svg Files} {.svg}}
							{{wmf Files} {.wmf}}
							{{ps Files} {.ps}}
							{{tex Files} {.tex}}
							{{pdf Files} {.pdf}}"
			) 	
	)
	
	# proceed only if user selected a path
	if (nchar(outPath)) {
		makeChart()
		ggsave(file=outPath)
		#tcl("exec", "explorer", paste("/select",normalizePath(outPath),sep=",")) ###TODO works but throws error
		
	}
}

#function to make the charts
#1. supports Pt, MaTy, and Ma outputs
#2. supports number, percentage, index measures
makeChart <- function(){
	metric <- tclvalue(metricValue)
	aggregation <- tclvalue(aggregationValue)
	measure <- tclvalue(measureValue)
	
	#helper functons
	#load the .Rdata files
	assignLoad <- function(filename){
		load(filename)
		get(ls()[ls() != "filename"])
	}
	
	#importing and formatting the data varies by the five output categories (Pt, MaTy, As, Ig, Ma )
	if(Metrics$category[Metrics$metric == metric] == "Pt"){
		#get the files specified by the user inputs
		#initialize an object to hold the data and make the correspondences between place, area and development
		chartdata <- data.frame(place = c("Rur","Sub R", "Sub E", "Sub M", "Sub T", "CIC R", "CIC E", "CIC M", "CIC T", "UC R", "UC E", "UC MU", "UC T"),
				area = c("Rural", rep("Suburban",4),rep("Close Com",4),rep("Urban Core",4)),
				development = c("Greenfield",rep(c("Resident","Employ","Mix Use","TOD"),3)))
		
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on place
		for(scenario in selectedScenarios){
			temp <- assignLoad(paste(paste(dirs$curProject(scenario),"outputs",metric,sep="/"),".Pt.RData",sep=""))
			temp <- data.frame(place = names(temp),temp)
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp,by="place") 
		}
	} 
	
	if(Metrics$category[Metrics$metric == metric] == "MaTy"){
		#get the files specified by the user inputs
		chartdata <- data.frame(vehtype = c("LtVeh","Truck","Bus"))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on vehtype
		for(scenario in selectedScenarios){
			temp <- assignLoad(paste(paste(dirs$curProject(scenario),"outputs",metric,sep="/"),".MaTy.RData",sep=""))
			temp <- data.frame(vehtype = colnames(temp),temp[1,])
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="vehtype") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "Ig"){
		#get the files specified by the user inputs
		chartdata <- data.frame(incgrp = c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" ))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on incgrp
		for(scenario in selectedScenarios){
			temp <- assignLoad(paste(paste(dirs$curProject(scenario),"outputs",metric,sep="/"),".Ig.RData",sep=""))
			temp <- data.frame(incgrp = names(temp),temp)
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="incgrp") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "As"){
		#get the files specified by the user inputs
		chartdata <- data.frame(accsev = c("Fatal","Injury","Property"))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on accsev
		for(scenario in selectedScenarios){
			temp <- assignLoad(paste(paste(dirs$curProject(scenario),"outputs",metric,sep="/"),".As.RData",sep=""))
			temp <- data.frame(accsev = rownames(temp),temp[,1])
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="accsev") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "Ma"){
		#get the files specified by the user inputs
		chartdata <- data.frame(variable = selectedScenarios, value = NA)
		#read in the scenario files one by one and add to chartdata
		for(scenario in selectedScenarios){
			temp <- assignLoad(paste(paste(dirs$curProject(scenario),"outputs",metric,sep="/"),".Ma.RData",sep=""))
			chartdata$value[chartdata$variable == scenario] <- temp 
		}
		
	}
	
	#default labels is to use the value from Metrics
	formatter <- paste(Metrics$formatter[Metrics$metric == metric])
	#default axis unit is to use the units from Metrics
	unitsname <- Metrics$units[Metrics$metric == metric]
	#if measure is percentages, convert values to percentages
	if(measure == "percentage"){
		if(Metrics$category[Metrics$metric == metric] != "Ma"){
			chartdata[,selectedScenarios] <- prop.table(as.matrix(chartdata[,selectedScenarios]),margin=2)
		}else{
			chartdata$value <- 1 #within category percentages don't make sense for the overall regional values but need to do this to avoid error
		}	
		formatter <- "percent"
		unitsname <- "(percentage)"
	}
	#if measure is index, convert values to index relative to first scenario
	#index100 is based off 100, index0 is then centered on 0 to show small changes more clearly
	if(measure %in% c("index0","index100")){
		formatter <- "comma"
		unitsname <- "(index, base=100)"
		normalize <- 0
		if(measure == "index0") {
					normalize <- 100
					unitsname <- "(percent change)"
				}
		if(Metrics$category[Metrics$metric == metric] != "Ma"){	
			if(aggregation != "all"){
				chartdata <- aggregate(chartdata[,selectedScenarios],by=list(chartdata[[aggregation]]),sum)
			}else{
				chartdata <- aggregate(chartdata[,selectedScenarios],by=list(rep("all",nrow(chartdata))),sum)
			}
			if(length(selectedScenarios == 1)){ names(chartdata)[2] <- selectedScenarios[1] }
			chartdata[,selectedScenarios] <- chartdata[,selectedScenarios]/chartdata[,selectedScenarios[1]]*100 - normalize
			names(chartdata)[1] <- aggregation
		}else{
			chartdata$value <- chartdata$value/chartdata$value[1]*100 - normalize
		}
	}
	
	#for data with categories (i.e. anything other than single regional values) need to reshape for charting
	if(Metrics$category[Metrics$metric == metric] != "Ma"){
		#Reshape the data so that it is ready for ggplot2
		chartdata <- melt(chartdata)
	}
	
	#build the metric name for the title and y axis name
  	metricname <- Metrics$metricname[Metrics$metric == metric]
	yname <- paste(metricname,unitsname,sep=" ")

	#chart type and labeling depends on aggregation
	if(aggregation=="all"){
		#make the chart with scenario as x axis
		chart <- qplot(variable,data=chartdata, geom="bar", weight = value, main=paste("Comparison of ",metricname," by Scenario",sep=""))
		chart <- chart + labs(x="Scenario",y=yname)
  	#remove the legend, uneccessary as the scenarios are labeled on the x axis
    chart <- chart + theme(legend.position = "none")
    #change the fill color to a more pleasant color that black
		chart <- chart + geom_bar(fill = "#9ECAE1")
	} else {
		if(aggregation=="place"){
			xname <- "Place Type"
			xorder <- c("Rur","Sub R", "Sub E", "Sub M", "Sub T", "CIC R", "CIC E", "CIC M", "CIC T", "UC R", "UC E", "UC MU", "UC T")
		}	
		if(aggregation=="area"){
			xname <- "Area Type"
			xorder <- c("Rural", "Suburban","Close Com","Urban Core")
		}
		if(aggregation=="development"){
			xname <- "Development Type"
			xorder <- c("Greenfield","Resident","Employ","Mix Use","TOD")
		}
		if(aggregation=="vehtype"){
			xname <- "Vehicle Type"
			xorder <- c("LtVeh","Truck", "Bus")
		}
		if(aggregation=="accsev"){
			xname <- "Accident Severity"
			xorder <- c("Fatal","Injury","Property")
		}
		if(aggregation=="incgrp"){
			xname <- "Income Group"
			xorder <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )
		}
		#make chart and format x axis
		chart <- qplot(get(`aggregation`),data=chartdata, geom="bar", weight = value, group = variable, fill = variable, position="dodge", main=paste("Comparison of ",metricname," by ", xname,sep=""))
		chart <- chart + labs(x=xname,y=yname,fill="Scenario")
		chart <- chart + xlim(xorder)
		#color palette for charts with multiple series
		chart <- chart + scale_fill_brewer()
	}
	#format y axis so it is numbers with commas separating 1000's
	if(formatter=="comma"){chart <- chart + scale_y_continuous(labels = comma)}
	if(formatter=="dollar"){chart <- chart + scale_y_continuous(labels = dollar)}
	if(formatter=="percent"){chart <- chart + scale_y_continuous(labels = percent)}
	#font size - make it bigger
  	chart <- chart + theme(axis.text.x = element_text(size = 14),
                        axis.text.y = element_text(size = 14),
                        axis.title.x = element_text(size = 16),
                        axis.title.y = element_text(size = 16, angle = 90),
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 16),
                        plot.title = element_text(size = 18))
  #color: white background, gray ticks, no grid lines
 	chart <- chart + theme(panel.background = element_rect(fill = NA, colour = "grey80"), 
                        axis.ticks = element_line(colour = "grey80"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
 		
	print(chart) # Need to call print for chart to show up in tkrplot graphics device	
}


