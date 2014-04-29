# Analyze Previously-Run Simulations
The ESME simulator logs exposures for every animat, every distinct mode, and every time step.  For complicated scenarios with many species and many platforms over long durations, it will be desirable to restrict different criteria and generate focused exposure histograms.

The Simulation Log Analysis tool is included in the program directory in which ESME is installed.  Launching it displays the following interface, independent of the Workbench:

![](http://esme.bu.edu/img/book_screenshots/logAnalyzerDialog.png)

By default , simulations are stored as a simulation.exposures log file in an automatically generated tree in the “My Documents” folder.   When a given exposures file is loaded, it can be re-analyzed by selectively choosing:
*	Which time period to examine
*	Which platforms to include or exclude from analysis
*	Which modes to include or exclude from analysis
*	Which species to include or exclude
*	The exposure histogram bin widths and frequency range.

For custom analysis, a [MATLAB](http://esme.bu.edu/download/MatlabTools.zip) or [Python](http://esme.bu.edu/download/PythonAPI.zip) API is available that provides direct access to individual simulation records.
