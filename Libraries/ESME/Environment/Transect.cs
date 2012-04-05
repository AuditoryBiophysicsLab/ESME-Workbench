using System;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class Transect : Geo
    {
        public Transect(string name, Geo startPoint, Geo endPoint)
            : base(startPoint)
        {
            Name = name;
            EndPoint = endPoint;
            Length = DistanceKilometers(EndPoint) * 1000;
            Bearing = RadiansToDegrees(Azimuth(EndPoint));
            MidPoint = Offset(KilometersToRadians(Length / 2000), DegreesToRadians(Bearing));
        }

        public Transect(string name, Geo startPoint, double bearing, double length) : base(startPoint)
        {
            Name = name;
            Length = length;
            EndPoint = Offset(KilometersToRadians(Length / 1000), DegreesToRadians(Bearing));
            Bearing = bearing;
            MidPoint = new Geo(startPoint);
            MidPoint = Offset(KilometersToRadians(Length / 2000), DegreesToRadians(Bearing));
        }

        /// <summary>
        /// Bearing from StartPoint to EndPoint, in degrees true
        /// </summary>
        public double Bearing { get; private set; }

        /// <summary>
        /// Distance in meters from StartPoint to EndPoint
        /// </summary>
        public new double Length { get; private set; }

        /// <summary>
        /// Name of the transect
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Start point of the transect
        /// </summary>
        public Geo StartPoint { get { return this; } }

        /// <summary>
        /// End point of the transect
        /// </summary>
        public Geo EndPoint { get; private set; }

        /// <summary>
        /// Mid point of the transect
        /// </summary>
        public new Geo MidPoint { get; private set; }
    }
}