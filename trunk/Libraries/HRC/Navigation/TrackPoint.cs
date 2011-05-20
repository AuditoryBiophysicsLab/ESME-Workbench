using System;

namespace HRC.Navigation
{
    public class TrackPoint
    {
        private readonly Geo _position = Geo.FromDegrees(0, 0);
        private double _eheight;
        private double _course;
        private double _speed;
        private long _timeMillis;
        // 5 millimeters in km. 
        private const double DeltaMm = 0.00005;

        public String Dump() { return String.Format("{0} {1} {2} {3} {4}", _position.Latitude, _position.Longitude, _eheight, _course, _speed); }

        public void SetLatLonHeight(double lat, double lon, double aHeight)
        {
            _position.Initialize(lat, lon);
            _eheight = aHeight;
        }

        public void SetTime(long millis)
        {
            _timeMillis = millis;
        }

        public long GetTime()
        {
            return _timeMillis;
        }

        public Geo GetGeoLlh()
        {
            return Geo.FromGeo(_position);
        }

        public void GetGeoLlh(Geo geo)
        {
            geo.Initialize(_position);
        }

        public double GetEllipsoidHeight()
        {
            return _eheight;
        }

        public double GetDepth()
        {
            return -_eheight;
        }

        public void SetCourse(double c)
        {
            _course = c;
        }

        public double GetCourse()
        {
            return _course;
        }

        public void SetSpeed(double s)
        {
            _speed = s;
        }

        public double GetSpeed()
        {
            return _speed;
        }

        public bool IsSamePosition(TrackPoint other)
        {
            return IsSamePosition(other, DeltaMm);
        }

        /**
         * If the position is the same lat/lon position with in delta kilometers, returns true.
         * @param other the second point
         * @param delta kilometers
         * @return if same position within delta kilometers.
         */
        public bool IsSamePosition(TrackPoint other, double delta)
        {
            double latCheck = (_position.Latitude - other.GetGeoLlh().Latitude);
            double lonCheck = (_position.Longitude - other.GetGeoLlh().Longitude);
            double deltaDeg = Geo.RadiansToDegrees(Geo.KilometersToRadians(delta));
            return (latCheck < deltaDeg) && (lonCheck < deltaDeg);
        }

        /**
         * Returns distance in meters
         * 
         * @param other
         * @return
         */
        public double Distance(TrackPoint other)
        {
            double dist = _position.DistanceRadians(other._position);
            double distkm = Geo.RadiansToKilometers(dist);
            return distkm * 1000.0;
        }

        public double SlantRange(TrackPoint other)
        {
            double dist = _position.DistanceRadians(other._position);
            double distM = Geo.RadiansToKilometers(dist) * 1000.0;
            double diffHeight = _eheight - other._eheight;

            return Math.Sqrt((distM * distM) + (diffHeight * diffHeight));
        }

        //TODO: make sure this is true bearing
        public double RelativeBearing(TrackPoint other)
        {
            return Geo.RadiansToDegrees(_position.Azimuth(other._position));
        }

        public new String ToString()
        {
            return String.Format("Moving {0:0.###} kts {1:0.###} degs @ {2} {3}T", _speed, _course, _position, _timeMillis);
        }

        public bool Equals(TrackPoint other)
        {
            return (IsSamePosition(other) &&
                (_eheight == other._eheight) &&
                (_course == other._course) &&
                (_speed == other._speed));
        }
    }
}
