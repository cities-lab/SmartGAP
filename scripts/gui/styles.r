# styles.r - sets up the font styles for the GUI
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

.Tcl("font create ProjectScenarioTitleFont -family Helvetica -size 16 -weight bold")
.Tcl("font create FlowChartTitleFont -family Helvetica -size 14")
.Tcl("font create FlowChartFont -family Helvetica -size 8 -weight bold")
.Tcl("font create InputTypeLabelFont -family Helvetica -size 10 -weight bold")
.Tcl("font create InputLabelFont  -size 9")
.Tcl("font create ButtonHeightFont  -size 8")
.Tcl("font create OutputCategoryLabelFont -family Helvetica -size 9 -weight bold")
.Tcl("font create CommentFont -family Helvetica -size 7 -slant italic")
.Tcl("font create LicenseFont -family Helvetica -size 7 -slant italic")
.Tcl("font create ReportControlsFont -family Helvetica -size 9 -weight bold")
.Tcl("font create Message -family Helvetica -size 9 -weight bold -slant italic")
.Tcl("font create AboutTitleFont -family Helvetica -size 16")
