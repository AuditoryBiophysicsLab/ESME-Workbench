using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss.REFMS
{
    public class REFMSInputFile
    {
#if false
COMMENT
For: LOC_259-Summer[2000 lb Bomb:1|2000 lb Bomb:1|Explosive:1] Vers: 1.20.0 working

UNITS       1 - Metric Units   
EXPLOSIVE   1    	Explosive composition (1=TNT)
YIELD       453.590 	kg
CLUSTER     1   	Charges
DEXPLOSION  1.00 	Depth of explosion in meters

RADIUS      0.   	Skip ship response
DURATION    5.0 	Duration in secs after bottom reflection
IMULT       0    	Second Order Reflection
IRB1        1.   	Compute bottom reflections
IRB2        1.   	Compute rays surf to gage
IRSC        0    	Compute rays in sound channel
IC          1.   	Disables cavitation.

FILTER     -2    	 

DWATER      24.9 	Depth of water in meters

BSSRATIO    2.000 	Soil Type: Sound Speed Ratio
BRHO        8.000 	Soil Type: Density
BSWSPEED    1394.923 	Soil Type: Shear Wave Velocity
BMAT        1.0 
SCALEI      no   
SVPFILE     LOC_259-Summer.svp   

SE          1   make 3rd-octave energy spectra
SPLCUTOFF   200.000  	 from 1 sources
SELCUTOFF   163  800  171  1100  183   
SOURCES     1   

DEPTHE
 0.30 25.00 16  
LOOPE
 50.00 20000.00 20 
LOOPL
 25000.00 100000.00 15  
STOP
#endif
    }
}
