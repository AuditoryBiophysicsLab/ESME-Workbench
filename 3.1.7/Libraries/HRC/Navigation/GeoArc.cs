using System;

namespace HRC.Navigation
{
    public class GeoArc
    {
        /// <summary>
        /// Describes a circular arc on the surface of a sphere
        /// </summary>
        /// <param name="location">Location of the center of the circle we're describing an arc of</param>
        /// <param name="centerAzimuth">Azimuth of the center of the slice, in radians from true north</param>
        /// <param name="horizontalWidth">Width of the slice in radians</param>
        /// <param name="radius">Radius of the circle in radians (relative to the center of the sphere)</param>
        public GeoArc(Geo location, double centerAzimuth, double horizontalWidth, double radius)
        {
            Location = location;
            Radius = radius;
            // Force the center azimuth to be between -2pi and 2pi
            centerAzimuth = Math.Sign(centerAzimuth) * (Math.Abs(centerAzimuth) % MoreMath.TwoPi);
            if (horizontalWidth < 0)
            {
                SliceStartAzimuth = centerAzimuth + (horizontalWidth / 2);
                SliceEndAzimuth = centerAzimuth - (horizontalWidth / 2);
            }
            else
            {
                SliceStartAzimuth = centerAzimuth - (horizontalWidth / 2);
                SliceEndAzimuth = centerAzimuth + (horizontalWidth / 2);
            }
            SliceWidth = Math.Min(Math.Abs(horizontalWidth), MoreMath.TwoPi);
        }

        public Geo Location { get; private set; }
        /// <summary>
        /// In radians, clockwise from true north
        /// </summary>
        public double SliceStartAzimuth { get; private set; }
        /// <summary>
        /// In radians, clockwise from true north
        /// </summary>
        public double SliceEndAzimuth { get; private set; }
        /// <summary>
        /// In radians, clockwise from true north
        /// </summary>
        public double SliceWidth { get; private set; }
        /// <summary>
        /// In radians
        /// </summary>
        public double Radius { get; private set; }

        public bool Contains(Geo geo)
        {
            var radius = Location.DistanceRadians(geo);
            var azimuth = Location.Azimuth(geo);
            return Contains(radius, azimuth);
        }

        public bool Contains(double radius, double azimuth)
        {
            if (radius > Radius) return false;

            if (SliceStartAzimuth <= azimuth && azimuth <= SliceEndAzimuth) return true;
            azimuth += (azimuth < 0) ? MoreMath.TwoPi : -MoreMath.TwoPi;
            return SliceStartAzimuth <= azimuth && azimuth <= SliceEndAzimuth;
        }
    }
}
