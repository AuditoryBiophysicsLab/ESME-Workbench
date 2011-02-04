
This directory contains source code for the Surface Marine Gridded 
Climatology (SMGC) version 2.0 extraction software.

This code was written and compiled under Linux but should compile 
under windows with some minor modifications.  A linux binary is 
provided with the SMGC2.0 distribution.

To build the binary executable type "make" at the unix command prompt
and then copy the executable to a location on your path (e.g. /usr/local/bin).

Set the following environment variables in the system wide shell
environment (e.g. /etc/profile)

	export SMGC_DATA_NORTH=/smgc2/north/
	export SMGC_DATA_SOUTH=/smgc2/south/

This assumes all the datafiles for the northern hemisphere are in 
"/smgc2/north" and all the datafiles for the southern hemisphere 
are in "/smgc2/south".  Even if all the datafiles (north and south) are in
one location (e.g. /smgc/data) you must still set the environment
variables like so:

	export SMGC_DATA_NORTH=/smgc/data/
	export SMGC_DATA_SOUTH=/smgc/data/

DO NOT FORGET TO INCLUDE THE TRAILING BACKSLASH!







