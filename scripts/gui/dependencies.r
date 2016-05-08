# dependencies.r - C16 dependencies: put all source(), library(), require(), tclRequire() calls here 
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

#Test whether a package is available and install without popping up the Cran mirror list	
loadPackage <- function (package, filename=NULL) {
	if(!package %in% .packages(all = TRUE, lib.loc="../library")) {
        if(is.null(filename)) {
            install.packages(package, "../library", repos = "http://cran.r-project.org")
        } else {
            for (i in 1:length(filename)) {
              install.packages(paste(getwd(),"..","dependencies",filename[i], sep="/"))
            }
        }
	}
	eval(parse(text=paste("library(", package, ",lib.loc='../library')", sep="")))
}

#Load packages required by the model script
#===================================================

#loadPackage("optparse", filename=c('getopt_1.19.0.zip','optparse_1.0.0.zip')) #option parsing requires optparse package
#loadPackage("reshape", filename=c('reshape_0.8.4.zip','plyr_1.8.zip')) #Business Synthsis (createBiz function) requires the reshape package
#loadPackage("data.table", filename=c('data.table_1.8.6.zip')) #pedictLogit fucntion requires tables in data.table format
loadPackage("getopt")
loadPackage("optparse") #option parsing requires optparse package
loadPackage("plyr")
loadPackage("reshape") #Business Synthsis (createBiz function) requires the reshape package
loadPackage("data.table") #pedictLogit fucntion requires tables in data.table format

# Load packages required by the GUI script
#===================================================

loadPackage("tcltk")
#loadPackage("tcltk2", filename=c('tcltk2_1.1-5.zip'))
loadPackage("tcltk2")
#loadPackage("gWidgetstcltk", filename=c('gWidgetstcltk_0.0-53.zip','gWidgets_0.0-52.zip','digest_0.6.0.zip'))
loadPackage("digest")
loadPackage("gWidgets")
loadPackage("gWidgetstcltk")
options(guiToolkit = "tcltk") #guiToolkit option required for gWidgets
#loadPackage("plyr") # flow_chart
#loadPackage("tkrplot", filename=c('tkrplot_0.0-23.zip')) #reports
loadPackage("tkrplot") #reports
#loadPackage("ggplot2",c('ggplot2_0.9.3.zip','colorspace_1.2-0.zip','stringr_0.6.2.zip','RColorBrewer_1.0-5.zip','dichromat_1.2-4.zip','munsell_0.4.zip','labeling_0.1.zip','gtable_0.1.2.zip','reshape2_1.2.2.zip','scales_0.2.3.zip','proto_0.3-10.zip')) #reports
loadPackage("ggplot2") #reports
loadPackage("scales") #reports
loadPackage("colorspace")
loadPackage("stringr")
loadPackage("RColorBrewer")
loadPackage("dichromat")
loadPackage("munsell")
loadPackage("labeling")
loadPackage("gtable")
loadPackage("reshape2")
loadPackage("proto")

# try to load a tcl package and throw an error meessage up if not found. (modified from http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/ExceptionHandling.html)
TclRequire <- function(tclPkg, errMessage)
{
	if ((data.class(result<-try(tclRequire(tclPkg),TRUE))=="try-error") || (is.logical(result) && result==FALSE))
	{
		tkmessageBox(title="An error has occured!",message=errMessage,icon="error",type="ok")	
		tkdestroy(tt)
		return(FALSE)
	}
	else
		return(TRUE)
}

# BWidget:  (tkwidget calls) ScrolledWindow, ScrolledFrame (ComboBox no longer used)  BWidget is included in standard R Windows install 2.13.1
TclRequire(tclPkg="BWidget",errMessage="The required tcl/tk package 'BWidget' could not be found. BWidget is included in the standard R Windows install, http://software.rc.fas.harvard.edu/mirrors/R/.")		

#http://sourceforge.net/projects/tcllib/files/BWidget/ or http://software.rc.fas.harvard.edu/mirrors/R/


#===================================================
# all source files

source("helpers.r")
source("styles.r")
source("processes.r")
source("comments.r")
source("inputs.r")
source("outputs.r")
source("flow_chart.r")
source("run.r")
source("parameters.r")
source("projects.r")
source("scenarios.r")
source("reports.r")
source("about.r")
source("prefs.r")
