# SmartGAP

SmartGAP is a scenario planning software tool that synthesizes households and firms in a region and determines their travel demand characteristics based on their built environment and transportation policies. It has been developed by RSG Inc for TRB's second Strategic Highway Research Program (SHRP 2) Capacity Project C16.

I am not affliated with the original SmartGAP software tool or SHRP 2 C16 project and the repository is created from SmartGAP's open source code for educational purpose. Please contact the original authors of the software and investigators of the project for questions. More information available at [TRB](http://www.trb.org/Main/Blurbs/168842.aspx). A new version of SmartGAP is being developed by RSG Inc at https://github.com/RSGInc/RPAT

## Instructions

1. Download and install R. Download the latest version of R from CRAN: http://cran.r-project.org/. R is available for Linux, Mac OS X, and Windows; the current version is R-3.3.0. It can be installed like common applications on each OS. If needed, installation instructions for Windows are at https://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-install-R-for-Windows_003f, while those for Mac OS X are at https://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html.

2. Clone the code `git clone https://github.com/cities-lab/SmartGAP.git` or download SmartGAP.zip from release. The current version fixes some issue with R package installation and should work on both Windows and Mac OS X. Unzip the downloaded zip file into a local folder.

3. If you use Windows, open a file explorer and find "RUN_SmartGAP.bat" in the unzipped SmartGAP folder, double click to run it.
If you use a Mac, find "RUN_SmartGAP.command" in the unzipped SmartGAP folder in Finder and double click to run it. The first time that RUN_SmartGAP runs, it will set up R for running SmartGAP.

4. Close the window opened in Step 3 when prompted (".libPaths set successfully; close this window and run RUN_SmartGAP again.") and double click the appropriate file to run SmartGAP again. SmartGAP should now install R packages and launch its Graphic User Interface. In some case, you may need to repeat step 4.

5. Follow the [SmartGAP User Guide (Chapter 4)](http://onlinepubs.trb.org/onlinepubs/shrp2/SHRP2prepubC16guide.pdf) for how to work with SmartGAP.


## References
1. [SHRP2 C16 report](http://onlinepubs.trb.org/onlinepubs/shrp2/SHRP2_S2-C16-RR-1.pdf)
2. [SmartGAP User Guide](http://onlinepubs.trb.org/onlinepubs/shrp2/SHRP2prepubC16guide.pdf)
