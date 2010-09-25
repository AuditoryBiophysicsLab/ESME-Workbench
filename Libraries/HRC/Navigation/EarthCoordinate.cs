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

        [XmlIgnore] const double EarthRadius_Meters = 6372795.477598;
        [XmlIgnore] const double RadiansToDegrees = (180.0/Math.PI);
        [XmlIgnore] const UInt32 Magic = 0x21954ef6;

        /// <summary>
        /// Construct a new EarthCoordinate
        /// </summary>
        /// <param name="latitude">in degrees</param>
        /// <param name="longitude">in degrees</param>
        public EarthCoordinate(double latitude, double longitude)
        {
            Latitude_degrees = latitude;
            Longitude_degrees = longitude;
        }

        public EarthCoordinate() { }

        public EarthCoordinate(EarthCoordinate startCoordinate, double bearing, double distanceMeters)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            var bearingRadians = ((Math.PI/180f)*bearing + 2*Math.PI)%(2*Math.PI);
            var arcLengthTraveled = distanceMeters/EarthRadius_Meters;

            var endLatRadians = Math.Asin((Math.Sin(startCoordinate.Latitude_radians)*Math.Cos(arcLengthTraveled)) + (Math.Cos(startCoordinate.Latitude_radians)*Math.Sin(arcLengthTraveled)*Math.Cos(bearingRadians)));
            var endLonRadians = startCoordinate.Longitude_radians + Math.Atan2(Math.Sin(bearingRadians)*Math.Sin(arcLengthTraveled)*Math.Cos(startCoordinate.Latitude_radians), Math.Cos(arcLengthTraveled) - (Math.Sin(startCoordinate.Latitude_radians)*Math.Sin(endLatRadians)));

            Latitude_degrees = endLatRadians*RadiansToDegrees;
            Longitude_degrees = endLonRadians*RadiansToDegrees;
        }

        public EarthCoordinate(EarthCoordinate source)
        {
            Latitude_degrees = source.Latitude_degrees;
            Longitude_degrees = source.Longitude_degrees;
        }

        public EarthCoordinate(EarthCoordinate3D source)
        {
            Latitude_degrees = source.Latitude_degrees;
            Longitude_degrees = source.Longitude_degrees;
        }

        public EarthCoordinate(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into an EarthCoordinate");
            Latitude_degrees = stream.ReadSingle();
            Longitude_degrees = stream.ReadSingle();
        }

        [XmlIgnore]
        public static int Size
        {
            get { return 16; }
        }

        [XmlElement]
        public double Latitude_degrees { get; set; }

        [XmlElement]
        public double Longitude_degrees { get; set; }

        [XmlIgnore]
        public double Latitude_radians
        {
            get { return Latitude_degrees*(Math.PI/180f); }
            set { Latitude_degrees = value*(180.0/Math.PI); }
        }

        [XmlIgnore]
        public double Longitude_radians
        {
            get { return Longitude_degrees*(Math.PI/180f); }
            set { Longitude_degrees = value*(180.0/Math.PI); }
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
            if ((Latitude_degrees < other.Latitude_degrees) || (Longitude_degrees < other.Longitude_degrees)) return -1;
            if ((Latitude_degrees > other.Latitude_degrees) || (Longitude_degrees > other.Longitude_degrees)) return 1;
            return 0;
        }

        #endregion

        #region IEquatable<EarthCoordinate> Members

        public bool Equals(EarthCoordinate other)
        {
            if ((Latitude_degrees == other.Latitude_degrees) && (Longitude_degrees == other.Longitude_degrees)) return true;
            return false;
        }

        #endregion

        public static double DistanceBetween_Meters(EarthCoordinate a, EarthCoordinate b) { return a.GetDistanceTo_Meters(b); }

        public double GetDistanceTo_Meters(EarthCoordinate earthCoordinate)
        {
            // http://www.movable-type.co.uk/scripts/LatLong.html

            return Math.Acos(Math.Sin(Latitude_radians)*Math.Sin(earthCoordinate.Latitude_radians) + Math.Cos(Latitude_radians)*Math.Cos(earthCoordinate.Latitude_radians)*Math.Cos(earthCoordinate.Longitude_radians - Longitude_radians))*EarthRadius_Meters;
        }

        public double GetBearingTo_Degrees(EarthCoordinate earthCoordinate) { return ((GetBearingTo_Radians(earthCoordinate)*RadiansToDegrees) + 360.0)%360.0; }

        public double GetBearingTo_Radians(EarthCoordinate earthCoordinate)
        {
            var deltaLong = earthCoordinate.Longitude_radians - Longitude_radians;

            var y = Math.Sin(deltaLong)*Math.Cos(earthCoordinate.Latitude_radians);
            var x = Math.Cos(Latitude_radians) * Math.Sin(earthCoordinate.Latitude_radians) - Math.Sin(Latitude_radians) * Math.Cos(earthCoordinate.Latitude_radians) * Math.Cos(deltaLong);
            return Math.Atan2(y, x);
        }

        /// <summary>
        /// Move the current point over a defined course and distance
        /// </summary>
        /// <param name="course">Course object</param>
        /// <param name="distance">in meters</param>
        public void Move(Course course, double distance) { Move(course.Degrees, distance); }

        /// <summary>
        /// Move the current point over a defined course and distance
        /// </summary>
        /// <param name="bearing">in degrees</param>
        /// <param name="distance">in meters</param>
        public void Move(double bearing, double distance)
        {
            double dR = distance/EarthRadius_Meters;
            double bearingRad = bearing*(Math.PI/180.0);
            // moves the current EarthCoordinate by distance along bearing
            // lat2 = asin(sin(lat1)*cos(d/R) + cos(lat1)*sin(d/R)*cos(brng))
            // lon2 = lon1 + atan2(sin(brng)*sin(d/R)*cos(lat1), cos(d/R)−sin(lat1)*sin(lat2))
            Latitude_radians = Math.Asin(Math.Sin(Latitude_radians) * Math.Cos(dR) + Math.Cos(Latitude_radians) * Math.Sin(dR) * Math.Cos(bearingRad));
            Longitude_radians = Longitude_radians + Math.Atan2(Math.Sin(bearingRad) * Math.Sin(dR) * Math.Cos(Latitude_radians), Math.Cos(dR) - Math.Sin(Latitude_radians) * Math.Sin(Latitude_radians));
        }

        public static double Bearing_Degrees(EarthCoordinate startCoordinate, EarthCoordinate endCoordinate)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            double lonDeltaRadians = endCoordinate.Longitude_radians - startCoordinate.Longitude_radians;

            double bearingRadians = Math.Atan2(Math.Sin(lonDeltaRadians)*Math.Cos(endCoordinate.Latitude_radians), (Math.Cos(startCoordinate.Latitude_radians)*Math.Sin(endCoordinate.Latitude_radians)) - (Math.Sin(startCoordinate.Latitude_radians)*Math.Cos(endCoordinate.Latitude_radians)*Math.Cos(lonDeltaRadians)));

            return (bearingRadians*(180/Math.PI) + 360)%360f;
        }

        /// <summary>
        /// Returns a new EarthCoordinate that is a specified bearing and distance from a provided point
        /// </summary>
        /// <param name="startPoint">The provided point</param>
        /// <param name="bearing">in degrees</param>
        /// <param name="distance">in meters</param>
        /// <returns></returns>
        public static EarthCoordinate BearingAndDistanceFrom(EarthCoordinate startPoint, double bearing, double distance)
        {
            var endPoint = new EarthCoordinate(startPoint);
            endPoint.Move(bearing, distance);
            return endPoint;
        }

        public override string ToString() { return "(" + Latitude_degrees + ", " + Longitude_degrees + ")"; }

        public static EarthCoordinate operator +(EarthCoordinate a, EarthCoordinate b) { return new EarthCoordinate(a.Latitude_degrees + b.Latitude_degrees, a.Longitude_degrees + b.Longitude_degrees); }

        public static EarthCoordinate operator -(EarthCoordinate a, EarthCoordinate b) { return new EarthCoordinate(a.Latitude_degrees - b.Latitude_degrees, a.Longitude_degrees - b.Longitude_degrees); }

        public static EarthCoordinate operator *(EarthCoordinate a, double scaleFactor) { return new EarthCoordinate(a.Latitude_degrees*scaleFactor, a.Longitude_degrees*scaleFactor); }

        public static EarthCoordinate operator /(EarthCoordinate a, double scaleFactor) { return new EarthCoordinate(a.Latitude_degrees/scaleFactor, a.Longitude_degrees/scaleFactor); }

        public virtual void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float) Latitude_degrees);
            stream.Write((float) Longitude_degrees);
        }

        public static explicit operator PointF(EarthCoordinate e) { return new PointF((float)e.Longitude_degrees, (float)e.Latitude_degrees); }
        public static explicit operator Point(EarthCoordinate e) { return new Point(e.Longitude_degrees, e.Latitude_degrees); }

        public void Compare(EarthCoordinate that) { Console.WriteLine("that coordinate's distance from this one is {0}m at a bearing of {1}deg", GetDistanceTo_Meters(that), GetBearingTo_Degrees(that)); }
    }

    [TypeConverter(typeof (EarthCoordinate3DTypeConverter))]
    public class EarthCoordinate3D : EarthCoordinate, IEquatable<EarthCoordinate3D>
    {
#if false
        public XmlSchema GetSchema() { return null; }
        public void ReadXml(XmlReader reader)
        {
            while (reader.Read())
            {
                if (reader.NodeType == XmlNodeType.Element)
                {
                    switch (reader.Name)
                    {
                        case "Latitude_degrees":
                            Latitude_degrees = double.Parse(reader.Value);
                            break;
                        case "Longitude_degrees":
                            Longitude_degrees = double.Parse(reader.Value);
                            break;
                        case "Elevation_meters":
                            Elevation_meters = double.Parse(reader.Value);
                            break;
                    }
                }
            }
        }
        public void WriteXml(XmlWriter writer)
        {
            writer.WriteElementString("Latitude_degrees", Latitude_degrees.ToString());
            writer.WriteElementString("Longitude_degrees", Longitude_degrees.ToString());
            writer.WriteElementString("Elevation_meters", Elevation_meters.ToString());
        }
#endif

        [XmlElement]
        public double Elevation_meters { get; set; }

        [XmlIgnore] const UInt32 Magic = 0x0908cd2f;

        [XmlIgnore]
        public new static int Size
        {
            get { return 8 + EarthCoordinate.Size; }
        }

        /// <summary>
        /// Construct a new EarthCoordinate3D
        /// </summary>
        /// <param name="latitude">in degrees</param>
        /// <param name="longitude">in degrees</param>
        /// <param name="elevation">in meters</param>
        public EarthCoordinate3D(double latitude, double longitude, double elevation) : base(latitude, longitude) { Elevation_meters = elevation; }

        public EarthCoordinate3D(EarthCoordinate earthCoordinate) : base(earthCoordinate.Latitude_degrees, earthCoordinate.Longitude_degrees) { Elevation_meters = 0; }

        public EarthCoordinate3D(EarthCoordinate3D earthCoordinate) : base(earthCoordinate.Latitude_degrees, earthCoordinate.Longitude_degrees) { Elevation_meters = earthCoordinate.Elevation_meters; }

        public EarthCoordinate3D(BinaryReader stream) : base(stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into an EarthCoordinate3D");
            Elevation_meters = stream.ReadSingle();
        }

        public EarthCoordinate3D() { }

        public static EarthCoordinate3D operator +(EarthCoordinate3D a, EarthCoordinate3D b) { return new EarthCoordinate3D(a.Latitude_degrees + b.Latitude_degrees, a.Longitude_degrees + b.Longitude_degrees, a.Elevation_meters + b.Elevation_meters); }

        public static EarthCoordinate3D operator +(EarthCoordinate3D a, EarthCoordinate b) { return new EarthCoordinate3D(a.Latitude_degrees + b.Latitude_degrees, a.Longitude_degrees + b.Longitude_degrees, a.Elevation_meters); }

        public static EarthCoordinate3D operator -(EarthCoordinate3D a, EarthCoordinate3D b) { return new EarthCoordinate3D(a.Latitude_degrees - b.Latitude_degrees, a.Longitude_degrees - b.Longitude_degrees, a.Elevation_meters - b.Elevation_meters); }

        public static EarthCoordinate3D operator *(EarthCoordinate3D a, double scaleFactor) { return new EarthCoordinate3D(a.Latitude_degrees*scaleFactor, a.Longitude_degrees*scaleFactor, a.Elevation_meters + scaleFactor); }

        public static EarthCoordinate3D operator /(EarthCoordinate3D a, double scaleFactor) { return new EarthCoordinate3D(a.Latitude_degrees/scaleFactor, a.Longitude_degrees/scaleFactor, a.Elevation_meters/scaleFactor); }

        public bool Equals(EarthCoordinate3D that) { return ((Latitude_degrees == that.Latitude_degrees) && (Longitude_degrees == that.Longitude_degrees) && (Elevation_meters == that.Elevation_meters)); }

        public override void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float) Elevation_meters);
            base.Write(stream);
        }
    }
}