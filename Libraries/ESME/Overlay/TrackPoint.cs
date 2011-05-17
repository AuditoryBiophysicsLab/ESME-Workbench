using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Overlay
{
    class TrackPoint
    {
        private Geo position = Geo.makeGeoDegrees(0, 0);
        private double eheight = 0;
        private double course = 0;
        private double speed = 0;
        private long timeMillis = 0L;
        // 5 millimeters in km. 
        private static double DELTA_MM = 0.00005;

        public TrackPoint()
        {

        }

        public String dump() { return String.Format("{0} {1} {2} {3} {4}", position.getLatitude(), position.getLongitude(), eheight, course, speed); }

        public void setLatLonHeight(double lat, double lon, double aHeight)
        {
            position.initialize(lat, lon);
            eheight = aHeight;
        }

        public void setTime(long millis)
        {
            timeMillis = millis;
        }

        public long getTime()
        {
            return timeMillis;
        }

        public Geo getGeoLLH()
        {
            return Geo.makeGeo(position);
        }

        public void getGeoLLH(Geo geo)
        {
            geo.initialize(position);
        }

        public double getEllipsoidHeight()
        {
            return eheight;
        }

        public double getDepth()
        {
            return -eheight;
        }

        public void setCourse(double c)
        {
            course = c;
        }

        public double getCourse()
        {
            return course;
        }

        public void setSpeed(double s)
        {
            speed = s;
        }

        public double getSpeed()
        {
            return speed;
        }

        public bool isSamePosition(TrackPoint other)
        {
            return isSamePosition(other, DELTA_MM);
        }

        /**
         * If the position is the same lat/lon position with in delta kilometers, returns true.
         * @param other the second point
         * @param delta kilometers
         * @return if same position within delta kilometers.
         */
        public bool isSamePosition(TrackPoint other, double delta)
        {
            double latCheck = (position.getLatitude() - other.getGeoLLH().getLatitude());
            double lonCheck = (position.getLongitude() - other.getGeoLLH().getLongitude());
            double deltaDeg = Geo.degrees(Geo.kmToAngle(delta));
            return (latCheck < deltaDeg) && (lonCheck < deltaDeg);
        }

        /**
         * Returns distance in meters
         * 
         * @param other
         * @return
         */
        public double distance(TrackPoint other)
        {
            double dist = position.distance(other.position);
            double distkm = Geo.km(dist);
            return distkm * 1000.0;
        }

        public double slantRange(TrackPoint other)
        {
            double dist = position.distance(other.position);
            double distM = Geo.km(dist) * 1000.0;
            double diffHeight = eheight - other.eheight;

            return Math.Sqrt((distM * distM) + (diffHeight * diffHeight));
        }

        //TODO: make sure this is true bearing
        public double relativeBearing(TrackPoint other)
        {
            return Geo.degrees(position.azimuth(other.position));
        }

        public new String ToString()
        {
            return String.Format("Moving {0:0.###} kts {1:0.###} degs @ {2} {3}T", speed, course, position, timeMillis);
        }

        public bool Equals(TrackPoint other)
        {
            return (isSamePosition(other) &&
                (this.eheight == other.eheight) &&
                (this.course == other.course) &&
                (this.speed == other.speed));
        }
    }
}
