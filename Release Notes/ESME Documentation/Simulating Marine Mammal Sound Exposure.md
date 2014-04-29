# Simulating Marine Mammal Sound Exposure
With the calculated transmission losses of at least one acoustic source in at least one location and at least one species population of marine mammals, the effect of that source on that species can be modeled for a given period of time at a given time resolution.

With this information, preliminary summary statistics can be calculated and plotted and the data exposed for further analysis via MATLAB or Python.

Pressing the Run Simulation button on the ribbon control launches the Scenario Simulator dialog window.

![](http://esme.bu.edu/img/book_screenshots/runSimulationDialog.png)

The default time step length is equal to the longest active time of any acoustic source in the scenario, but can be changed as desired.

If Animate Display is checked, the map display will animate as the simulation runs. The currently active sound sources will display on the map and update their position and bearing at every time step.  In certain cases, this will result in the displayed modes flickering rapidly as they turn on and off according to their duty cycles.

If “Simulate animal movement” is checked, 3MB will, at each time step, update the position of each animat in the scenario according to its respective movement model.  Note: this will greatly increase the time each scenario takes to complete.

![](http://esme.bu.edu/img/book_screenshots/simulationRunning.png)

Once the simulation is complete, a directory will be created in the My Documents folder named ESME Simulations.  Inside this directory, one directory will be created for each Scenario and uniquely time-stamped simulation run.

![](http://esme.bu.edu/img/book_screenshots/showHistograms.png)

If the “Display exposure histograms” button is checked, after the simulation completes, summary graphics will be displayed.

Each histogram shows the relative effect of each distinct mode in the simulation on a given species, binned over the range of 100-200 dB SPL in 10 dB bins.
