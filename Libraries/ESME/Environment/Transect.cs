using HRC.Navigation;

namespace ESME.Environment
{
    public class Transect : EarthCoordinate
    {
        readonly EarthCoordinate _endPoint;
        readonly EarthCoordinate _midPoint;

        public Transect(string name, EarthCoordinate startPoint, EarthCoordinate endPoint) : base(startPoint)
        {
            Name = name;
            _endPoint = endPoint;
            Length = DistanceTo(_endPoint);
            Bearing = BearingTo(_endPoint);
            _midPoint = new EarthCoordinate(startPoint);
            _midPoint.Move(Bearing, Length / 2);
        }

        public Transect(string name, EarthCoordinate startPoint, double bearing, double length) : base(startPoint)
        {
            Name = name;
            _endPoint = new EarthCoordinate(this, bearing, length);
            Length = length;
            Bearing = bearing;
            _midPoint = new EarthCoordinate(startPoint);
            _midPoint.Move(Bearing, Length / 2);
        }

        /// <summary>
        /// Bearing from StartPoint to EndPoint, in degrees true
        /// </summary>
        public double Bearing { get; private set; }

        /// <summary>
        /// Distance in meters from StartPoint to EndPoint
        /// </summary>
        public double Length { get; private set; }

        /// <summary>
        /// Name of the transect
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Start point of the transect
        /// </summary>
        public EarthCoordinate StartPoint { get { return new EarthCoordinate(this); } }

        /// <summary>
        /// End point of the transect
        /// </summary>
        public EarthCoordinate EndPoint { get { return new EarthCoordinate(_endPoint); } }

        /// <summary>
        /// Mid point of the transect
        /// </summary>
        public EarthCoordinate MidPoint { get { return new EarthCoordinate(_midPoint); } }
    }
}