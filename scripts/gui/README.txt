

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


h1. Project Layout
* *model*
    * static model inputs not to be modified (Colin Smith)
    
* *projects*
    * projects and scenarios are stored here
    
* *scripts*
    * model scripts (Colin Smith)
    * *SmartGAP.r*
        * main script for running the Regional Evaluation for Growth Strategies (REGS) model
    * *SmartGAP_Inputs.r*
        * loads all of the model objects and data needed to run the REGS model.
    * *SmartGAP_Sim.r*
        * this module performs all of the household microsimulation calculations
        
* *scripts/gui*
    * root of the GUI project (Jeremy Wallis, Chris Hoffman)
    * *main.r*
        * the main executing code block, parses options, builds the GUI
    * *dependencies.r*
        * all source(), library(), require(), tclRequire() calls
    * *helpers.r*
        * contains helper functions
    * *projects.r*
        * contains logic for handling controls at project level
    * *scenarios.r*
        * contains logic for handling controls at scenario level
    * *flow_chart.r*
        * contains logic for rendering the flow chart and wiring up events
    * *processes.r*
        * functions for getting model processes
    * *processes.csv*
        * list of model processes 
    * *parameters.r*
        * functions for handling model parameters (at the project level)
    * *parameters.csv*
        * list of model parameter files customizable at project level
    * *inputs.r*
        * functions for building and wiring up the model inputs
    * *inputs.csv*
        * list of required input files and the processes that depend on them
    * *comments.r*
        * functions for getting and setting scenario and inputs comments (at scenario level)
    * *comments.csv*
        * list of user comments on scenario and inputs (at scenario level)
    * *run.r*
        * functions running model in external R process while maintaining GUI responsiveness
    * *outputs.r*
        * functions for displaying model outputs
    * *outputs.csv*
        * list of model output files and the processes that create them
    * *reports.r*
        * functions for generating model reports
    * *styles.r*
        * contains font information for the gui
    * *about.r*
        * About window with version and copyright
    * *prefs.r*
        * functions for getting and setting preferences
    * *prefs.csv*
        * preference values (currently just Project and Scenario)


