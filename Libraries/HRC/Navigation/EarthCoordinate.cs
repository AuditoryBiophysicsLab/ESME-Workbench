using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Xml.Serialization;
using Point = System.Windows.Point;

namespace HRC.Navigation
{
    [TypeConverter(typeof (EarthCoordinateTypeConverter))]
    public class EarthCoordinate : IEquatable<EarthCoordinate>, IComparable<EarthCoordinate>
    {
        //Equation for finding Earth's radius given the latitude (code in Matlab).  The Earth is actually
        //slightly spheroidal, with the radius maximum at equator and minimum at poles. 
        //a           = 6378.137;             Major axis of Earth (km)
        //b           = 6356.752;             Minor axis of Earth (km)
        //http://en.wikipedia.org/wiki/Earth_radiusEarth
        //radiusEarth = sqrt(((a^2*cosd(source_lat))^2+(b^2*sind(source_lat))^2)/((a*cosd(source_lat))^2+(b*sind(source_lat))^2));   % (km)   

        [XmlIgnore] const double RadiusOfTheEarthInMeters = 6372795.477598;
        [XmlIgnore] const double RadiansToDegrees = (180.0 / Math.PI);
        [XmlIgnore] const UInt32 Magic = 0x21954ef6;

        /// <summary>
        ///   Ran into some NaNs when computing distances between two points that were very close to or identical with each other, so to work
        ///   around this problem we're limiting the precision of numbers when calling arc functions (asin, acos, atan) to this many digits
        ///   of precision.  8 digits yields an approximate precision of 0.1 meters.
        /// </summary>
        [XmlIgnore] const int MaximumDigitsOfPrecision = 8;

        /// <summary>
        ///   Construct a new EarthCoordinate
        /// </summary>
        /// <param name = "latitude">in degrees</param>
        /// <param name = "longitude">in degrees</param>
        public EarthCoordinate(double latitude, double longitude)
        {
            Latitude = latitude;
            Longitude = longitude;
        }

        public EarthCoordinate() { }

        public EarthCoordinate(EarthCoordinate startCoordinate, double bearing, double distanceMeters)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            var bearingRadians = ((Math.PI / 180f) * bearing + 2 * Math.PI) % (2 * Math.PI);
            var arcLengthTraveled = distanceMeters / RadiusOfTheEarthInMeters;
            var sine = (Math.Sin(startCoordinate.LatitudeRadians) * Math.Cos(arcLengthTraveled)) + (Math.Cos(startCoordinate.LatitudeRadians) * Math.Sin(arcLengthTraveled) * Math.Cos(bearingRadians));
            var endLatRadians = Math.Asin(Math.Round(sine, MaximumDigitsOfPrecision));
            var x = Math.Sin(bearingRadians) * Math.Sin(arcLengthTraveled) * Math.Cos(startCoordinate.LatitudeRadians);
            var y = Math.Cos(arcLengthTraveled) - (Math.Sin(startCoordinate.LatitudeRadians) * Math.Sin(endLatRadians));
            var endLonRadians = startCoordinate.LongitudeRadians + Math.Atan2(Math.Round(x, MaximumDigitsOfPrecision), Math.Round(y, MaximumDigitsOfPrecision));

            Latitude = endLatRadians * RadiansToDegrees;
            Longitude = endLonRadians * RadiansToDegrees;
        }

        public EarthCoordinate(EarthCoordinate source)
        {
            Latitude = source.Latitude;
            Longitude = source.Longitude;
        }

        public EarthCoordinate(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into an EarthCoordinate");
            Latitude = stream.ReadSingle();
            Longitude = stream.ReadSingle();
        }

        [XmlIgnore]
        public static int Size
        {
            get { return 16; }
        }

        /// <summary>
        /// Latitude, in degrees
        /// </summary>
        [XmlElement]
        public double Latitude { get; set; }

        /// <summary>
        /// Longitude, in degrees
        /// </summary>
        [XmlElement]
        public double Longitude { get; set; }

        /// <summary>
        /// Latitude, in radians
        /// </summary>
        [XmlIgnore]
        public double LatitudeRadians
        {
            get { return Latitude * (Math.PI / 180.0); }
            set { Latitude = value * (180.0 / Math.PI); }
        }

        /// <summary>
        /// Longitude, in radians
        /// </summary>
        [XmlIgnore]
        public double LongitudeRadians
        {
            get { return Longitude * (Math.PI / 180.0); }
            set { Longitude = value * (180.0 / Math.PI); }
        }

        // This routine compares one earth coordinate to another for purposes of sorting.
        // If the current point is to the north or west of the other point it is presumed to be less than the other point
        // If the current point is neither further north nor further west than the other point, then if the other point
        // is further north or further west than the current point, the current point is presumed to be greater than the other
        // point.
        // If neither is true, the points are held to be equal.

        #region IComparable<EarthCoordinate> Members

        public int CompareTo(EarthCoordinate other)
        {
            if (DistanceBetween(this, other) < 1.0) return 0;
            if ((Latitude < other.Latitude) || (Longitude < other.Longitude)) return -1;
            return 1;
        }

        #endregion

        #region IEquatable<EarthCoordinate> Members

        public bool Equals(EarthCoordinate other) { return DistanceBetween(this, other) < 1.0; }

        #endregion

        /// <summary>
        /// Distance between two EarthCoordinates, in meters
        /// </summary>
        /// <param name="a">Point A</param>
        /// <param name="b">Point B</param>
        /// <returns></returns>
        public static double DistanceBetween(EarthCoordinate a, EarthCoordinate b) { return a.DistanceTo(b); }

        /// <summary>
        /// The distance to another EarthCoordinate
        /// </summary>
        /// <param name="earthCoordinate"></param>
        /// <returns>Distance in meters</returns>
        public double DistanceTo(EarthCoordinate earthCoordinate)
        {
            // http://www.movable-type.co.uk/scripts/LatLong.html
            var cosine = Math.Sin(LatitudeRadians) * Math.Sin(earthCoordinate.LatitudeRadians) + Math.Cos(LatitudeRadians) * Math.Cos(earthCoordinate.LatitudeRadians) * Math.Cos(earthCoordinate.LongitudeRadians - LongitudeRadians);
            if (cosine > 1.0)
                return Math.Acos(Math.Round(cosine, MaximumDigitsOfPrecision)) * RadiusOfTheEarthInMeters;
            return Math.Acos(cosine) * RadiusOfTheEarthInMeters;
        }

        /// <summary>
        /// The bearing to another EarthCoordinate
        /// </summary>
        /// <param name="earthCoordinate"></param>
        /// <returns>Bearing in degrees from true north</returns>
        public double BearingTo(EarthCoordinate earthCoordinate) { return ((BearingToInRadians(earthCoordinate) * RadiansToDegrees) + 360.0) % 360.0; }

        /// <summary>
        /// The bearing to another EarthCoordinate
        /// </summary>
        /// <param name="earthCoordinate"></param>
        /// <returns>Bearing in radians from true north</returns>
        public double BearingToInRadians(EarthCoordinate earthCoordinate)
        {
            var deltaLong = earthCoordinate.LongitudeRadians - LongitudeRadians;

            var x = Math.Sin(deltaLong) * Math.Cos(earthCoordinate.LatitudeRadians);
            var y = Math.Cos(LatitudeRadians) * Math.Sin(earthCoordinate.LatitudeRadians) - Math.Sin(LatitudeRadians) * Math.Cos(earthCoordinate.LatitudeRadians) * Math.Cos(deltaLong);
            return Math.Atan2(Math.Round(x, MaximumDigitsOfPrecision), Math.Round(y, MaximumDigitsOfPrecision));
        }

        /// <summary>
        ///   Move the current point over a defined course and distance
        /// </summary>
        /// <param name = "course">Course object</param>
        /// <param name = "distance">in meters</param>
        public void Move(Course course, double distance) { Move(course.Degrees, distance); }

        /// <summary>
        ///   Move the current point over a defined course and distance
        /// </summary>
        /// <param name = "bearing">in degrees</param>
        /// <param name = "distance">in meters</param>
        public void Move(double bearing, double distance)
        {
            var dR = distance / RadiusOfTheEarthInMeters;
            var bearingRad = bearing * (Math.PI / 180.0);
            // moves the current EarthCoordinate by distance along bearing
            // lat2 = asin(sin(lat1)*cos(d/R) + cos(lat1)*sin(d/R)*cos(brng))
            // lon2 = lon1 + atan2(sin(brng)*sin(d/R)*cos(lat1), cos(d/R)−sin(lat1)*sin(lat2))
            var sine = Math.Sin(LatitudeRadians) * Math.Cos(dR) + Math.Cos(LatitudeRadians) * Math.Sin(dR) * Math.Cos(bearingRad);
            //LatitudeRadians = Math.Asin(Math.Round(sine, MaximumDigitsOfPrecision));
            LatitudeRadians = Math.Asin(sine);
            var x = Math.Sin(bearingRad) * Math.Sin(dR) * Math.Cos(LatitudeRadians);
            var y = Math.Cos(dR) - Math.Sin(LatitudeRadians) * Math.Sin(LatitudeRadians);
            //LongitudeRadians = LongitudeRadians + Math.Atan2(Math.Round(x, MaximumDigitsOfPrecision), Math.Round(y, MaximumDigitsOfPrecision));
            LongitudeRadians = LongitudeRadians + Math.Atan2(x, y);
        }

        /// <summary>
        /// The bearing between two EarthCoordinates
        /// </summary>
        /// <param name="startCoordinate"></param>
        /// <param name="endCoordinate"></param>
        /// <returns>The bearing from startCoordinate to endCoordinate, in degrees</returns>
        public static double BearingBetween(EarthCoordinate startCoordinate, EarthCoordinate endCoordinate)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            var lonDeltaRadians = endCoordinate.LongitudeRadians - startCoordinate.LongitudeRadians;

            var x = Math.Sin(lonDeltaRadians) * Math.Cos(endCoordinate.LatitudeRadians);
            var y = (Math.Cos(startCoordinate.LatitudeRadians) * Math.Sin(endCoordinate.LatitudeRadians)) - (Math.Sin(startCoordinate.LatitudeRadians) * Math.Cos(endCoordinate.LatitudeRadians) * Math.Cos(lonDeltaRadians));
            var bearingRadians = Math.Atan2(Math.Round(x, MaximumDigitsOfPrecision), Math.Round(y, MaximumDigitsOfPrecision));

            return (bearingRadians * (180 / Math.PI) + 360) % 360f;
        }

        /// <summary>
        ///   Returns a new EarthCoordinate that is a specified bearing and distance from a provided point
        /// </summary>
        /// <param name = "startPoint">The provided point</param>
        /// <param name = "bearing">in degrees</param>
        /// <param name = "distance">in meters</param>
        /// <returns></returns>
        public static EarthCoordinate BearingAndDistanceBetween(EarthCoordinate startPoint, double bearing, double distance)
        {
            var endPoint = new EarthCoordinate(startPoint);
            endPoint.Move(bearing, distance);
            return endPoint;
        }

        public override string ToString() { return string.Format("({0:0.0000},{1:0.0000})", Latitude, Longitude); }

        public static EarthCoordinate operator +(EarthCoordinate a, EarthCoordinate b) { return new EarthCoordinate(a.Latitude + b.Latitude, a.Longitude + b.Longitude); }

        public static EarthCoordinate operator -(EarthCoordinate a, EarthCoordinate b) { return new EarthCoordinate(a.Latitude - b.Latitude, a.Longitude - b.Longitude); }

        public static EarthCoordinate operator *(EarthCoordinate a, double scaleFactor) { return new EarthCoordinate(a.Latitude * scaleFactor, a.Longitude * scaleFactor); }

        public static EarthCoordinate operator /(EarthCoordinate a, double scaleFactor) { return new EarthCoordinate(a.Latitude / scaleFactor, a.Longitude / scaleFactor); }

        public virtual void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float) Latitude);
            stream.Write((float) Longitude);
        }

        public static explicit operator PointF(EarthCoordinate e) { return new PointF((float) e.Longitude, (float) e.Latitude); }
        public static implicit operator Point(EarthCoordinate e) { return new Point(e.Longitude, e.Latitude); }

        public void Compare(EarthCoordinate that) { Console.WriteLine("that coordinate's distance from this one is {0}m at a bearing of {1}deg", DistanceTo(that), BearingTo(that)); }
    }

    public class EarthCoordinate<T> : EarthCoordinate
    {
        public EarthCoordinate() { }
        public EarthCoordinate(double latitude, double longitude) : base(latitude, longitude) { }
        public EarthCoordinate(double latitude, double longitude, T data) : base(latitude, longitude) { Data = data; }

        public T Data { get; set; }
    }

    [TypeConverter(typeof (EarthCoordinate3DTypeConverter))]
    public class EarthCoordinate3D : EarthCoordinate, IEquatable<EarthCoordinate3D>
    {
        /// <summary>
        /// Elevation of the current point, in meters above sea level
        /// </summary>
        [XmlElement]
        public double Elevation { get; set; }

        [XmlIgnore] const UInt32 Magic = 0x0908cd2f;

        [XmlIgnore]
        public new static int Size
        {
            get { return 8 + EarthCoordinate.Size; }
        }

        /// <summary>
        ///   Construct a new EarthCoordinate3D
        /// </summary>
        /// <param name = "latitude">in degrees</param>
        /// <param name = "longitude">in degrees</param>
        /// <param name = "elevation">in meters</param>
        public EarthCoordinate3D(double latitude, double longitude, double elevation) : base(latitude, longitude) { Elevation = elevation; }

        public EarthCoordinate3D(EarthCoordinate earthCoordinate) : base(earthCoordinate.Latitude, earthCoordinate.Longitude) { Elevation = 0; }

        public EarthCoordinate3D(EarthCoordinate3D earthCoordinate) : base(earthCoordinate.Latitude, earthCoordinate.Longitude) { Elevation = earthCoordinate.Elevation; }

        public EarthCoordinate3D(BinaryReader stream) : base(stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into an EarthCoordinate3D");
            Elevation = stream.ReadSingle();
        }

        public EarthCoordinate3D() { }

        public static EarthCoordinate3D operator +(EarthCoordinate3D a, EarthCoordinate3D b) { return new EarthCoordinate3D(a.Latitude + b.Latitude, a.Longitude + b.Longitude, a.Elevation + b.Elevation); }

        public static EarthCoordinate3D operator +(EarthCoordinate3D a, EarthCoordinate b) { return new EarthCoordinate3D(a.Latitude + b.Latitude, a.Longitude + b.Longitude, a.Elevation); }

        public static EarthCoordinate3D operator -(EarthCoordinate3D a, EarthCoordinate3D b) { return new EarthCoordinate3D(a.Latitude - b.Latitude, a.Longitude - b.Longitude, a.Elevation - b.Elevation); }

        public static EarthCoordinate3D operator *(EarthCoordinate3D a, double scaleFactor) { return new EarthCoordinate3D(a.Latitude * scaleFactor, a.Longitude * scaleFactor, a.Elevation + scaleFactor); }

        public static EarthCoordinate3D operator /(EarthCoordinate3D a, double scaleFactor) { return new EarthCoordinate3D(a.Latitude / scaleFactor, a.Longitude / scaleFactor, a.Elevation / scaleFactor); }

        public bool Equals(EarthCoordinate3D that) { return ((Latitude == that.Latitude) && (Longitude == that.Longitude) && (Elevation == that.Elevation)); }

        public override void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float) Elevation);
            base.Write(stream);
        }
    }
}