using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace ESME.TransmissionLoss.REFMS
{
    public class REFMSInputFile
    {
        public double Yield { get; set; }
        public double Cluster { get; set; }
        public double ExplosionDepth { get; set; }
        public double WaterDepth { get; set; }
        public double BottomSoundSpeedRatio { get; set; }
        public double BottomShearWaveSpeed { get; set; }
        public double SPLCutoff { get; set; }
        public double SourceCount { get; set; }

        public void Write(string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("");
                writer.WriteLine("COMMENT");
                writer.WriteLine("For: [tbd]");
                writer.WriteLine("");
                writer.WriteLine("UNITS       1 - Metric Units");
                writer.WriteLine("EXPLOSIVE   1    	Explosive composition (1=TNT)");
                writer.WriteLine("YIELD       {0:0.000} 	kg", Yield);
                writer.WriteLine("CLUSTER     {0}   	Charges", Cluster);
                writer.WriteLine("DEXPLOSION  {0:0.00} 	Depth of explosion in meters", ExplosionDepth);
                writer.WriteLine("");
                writer.WriteLine("RADIUS      0.   	Skip ship response");
                writer.WriteLine("DURATION    5.0 	Duration in secs after bottom reflection");
                writer.WriteLine("IMULT       0    	Second Order Reflection");
                writer.WriteLine("IRB1        1.   	Compute bottom reflections");
                writer.WriteLine("IRB2        1.   	Compute rays surf to gage");
                writer.WriteLine("IRSC        0    	Compute rays in sound channel");
                writer.WriteLine("IC          1.   	Disables cavitation.");
                writer.WriteLine("");
                writer.WriteLine("FILTER     -2    	 ");
                writer.WriteLine("");
                writer.WriteLine("DWATER      26.9 	Depth of water in meters");
                writer.WriteLine("");
                writer.WriteLine("BSSRATIO    2.000 	Soil Type: Sound Speed Ratio");
                writer.WriteLine("BRHO        8.000 	Soil Type: Density");
                writer.WriteLine("BSWSPEED    1357.767 	Soil Type: Shear Wave Velocity");
                writer.WriteLine("BMAT        1.0 ");
                writer.WriteLine("SCALEI      no   ");
                writer.WriteLine("SVPFILE     loc.svp   ");
                writer.WriteLine("");
                writer.WriteLine("SE          1   make 3rd-octave energy spectra");
                writer.WriteLine("SPLCUTOFF   200.000  	 from 1 sources");
                writer.WriteLine("SELCUTOFF   164  800  171  1100  183   ");
                writer.WriteLine("SOURCES     1   ");
                writer.WriteLine("");
                writer.WriteLine("DEPTHE");
                writer.WriteLine(" 0.30 27.00 20  ");
                writer.WriteLine("LOOPE");
                writer.WriteLine(" 50.00 549.30 20 ");
                writer.WriteLine("LOOPL");
                writer.WriteLine(" 686.63 2746.51 15  ");
                writer.WriteLine("STOP");
            }
        }
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
