MATLAB Builder NE (.NET Component)


1. Prerequisites for Deployment 

. Verify the MATLAB Compiler Runtime (MCR) is installed and ensure you    
  have installed version 7.15.   

. If the MCR is not installed, run MCRInstaller.exe, located in:

  <matlabroot>*\toolbox\compiler\deploy\win64\MCRInstaller.exe

For more information on the MCR Installer, see the MATLAB Compiler 
documentation.   
      
NOTE: You will need administrator right to run MCRInstaller.

2. Files to Deploy and Package

-BellhopNL.dll
   -contains the generated component using MWArray API. 
-BellhopNLNative.dll
   -contains the generated component using native API.
-This readme file

. If the target machine does not have version 7.15 of 
  the MCR installed, include MCRInstaller.exe.

. If you have generated a type-safe API for this component (using ntswrap.exe
or deploytool), see the Type Safe API section below for additional files to
deploy and package.


Auto-generated Documentation Templates:

MWArray.xml - This file contains the code comments for the MWArray data conversion 
              classes and their methods. This file can be found in either the component 
              distrib directory or in
              <mcr_root>*\toolbox\dotnetbuilder\bin\win64\v2.0

BellhopNL_overview.html - HTML overview documentation file for the generated component. 
                          It contains the requirements for accessing the component and 
                          for generating arguments using the MWArray class hierarchy.

BellhopNL.xml - This file contains the code comments for the BellhopNL component classes 
                          and methods. Using a third party documentation tool, this file 
                          can be combined with either or both of the previous files to 
                          generate online documentation for the BellhopNL component.

                 


3. Resources

To learn more about:               See:
================================================================================================
The MWArray classes                MATLAB product help or <mcr_root>*\
                                   help\toolbox\dotnetbuilder\MWArrayAPI\MWArrayAPI.chm
Examples of .NET Web Applications  MATLAB Application Deployment 
                                   Web Example Guide


4. Definitions

For a complete list of product terminology, go to 
http://www.mathworks.com/help and select MATLAB Builder NE.



* NOTE: <matlabroot> is the directory where MATLAB is installed on the target machine.
        <mcr_root> is the directory where MCR is installed on the target machine.
