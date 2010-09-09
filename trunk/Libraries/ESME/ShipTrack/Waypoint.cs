using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.ShipTrack
{
    public class Waypoint
    {
        public TimeSpan TimeSinceStart {get;set;}
        public EarthCoordinate Location{get;set;}
        public float Heading_degrees {get;set;}
        public float HorizontalBeamBearing_degrees{get;set;}
        public float HorizontalBeamWidth_degrees{get;set;}

        public Waypoint(TimeSpan TimeSinceStart, EarthCoordinate Location, float Heading_degrees, float HorizontalBeamBearing_degrees,
                        float HorizontalBeamWidth_degrees,
                                   double[] frequencyVector, double[] intensityVector, double[] depthVector, double[] transectLengthVector,
                                   double[] verticalAngleVector, double[] verticalHalfAngleVector, double[] pingIntervalVector, double[] pingDurationVector,
                                   int[] transectCountVector, string FullFileName, DateTime ModificationTime)
        {
        }
    }
}
