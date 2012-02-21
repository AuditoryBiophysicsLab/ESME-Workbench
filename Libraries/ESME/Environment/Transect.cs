using System;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class Transect : Geo
    {
        readonly Geo _endPoint;
        readonly Geo _midPoint;

        public Transect(string name, Geo startPoint, Geo endPoint)
            : base(startPoint)
        {
            Name = name;
            _endPoint = endPoint;
            Length = DistanceMeters(_endPoint);
            Bearing = AzimuthDegrees(_endPoint);
            _midPoint = startPoint.Move(Bearing, Length / 2);
        }

        public Transect(string name, Geo startPoint, double bearing, double length) : base(startPoint)
        {
            Name = name;
            _endPoint = new Geo(this, bearing, length);
            Length = length;
            Bearing = bearing;
            _midPoint = new Geo(startPoint);
            _midPoint.Move(Bearing, Length / 2);
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
        public Geo StartPoint { get { return new Geo(this); } }

        /// <summary>
        /// End point of the transect
        /// </summary>
        public Geo EndPoint { get { return new Geo(_endPoint); } }

        /// <summary>
        /// Mid point of the transect
        /// </summary>
        public new Geo MidPoint { get { return new Geo(_midPoint); } }
    }
}