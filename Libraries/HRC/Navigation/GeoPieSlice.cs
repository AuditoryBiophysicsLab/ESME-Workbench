using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HRC.Navigation
{
    public class GeoPieSlice
    {
        public GeoPieSlice(Geo location, double centerAzimuth, double horizontalWidth, double radius)
        {
            Location = location;
            Radius = radius;
            StartAzimuth = centerAzimuth - (horizontalWidth / 2);
            EndAzimuth = centerAzimuth + (horizontalWidth / 2);
            IsClockwise = true;
        }

        public Geo Location { get; private set; }
        /// <summary>
        /// In radians, clockwise from true north
        /// </summary>
        public double StartAzimuth { get; private set; }
        /// <summary>
        /// In radians, clockwise from true north
        /// </summary>
        public double EndAzimuth { get; private set; }
        /// <summary>
        /// In radians
        /// </summary>
        public double Radius { get; private set; }
        public bool IsClockwise { get; private set; }

        public bool Contains(Geo geo)
        {
            if (Location.DistanceRadians(geo) > Radius) return false;
            var azimuthToTarget = Location.Azimuth(geo);
            // todo: this is overly simplistic, deal with wrapping issues etc. here as well as clockwise and counterclockwise slices
            if (azimuthToTarget < StartAzimuth || azimuthToTarget > EndAzimuth) return false;
        }
    }
}
