using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Drawing;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace HRC.Navigation
{
    [System.ComponentModel.TypeConverter(typeof(EarthCoordinateTypeConverter))]
    public class EarthCoordinate : 
        IEquatable<EarthCoordinate>, 
        IComparable<EarthCoordinate>
    {
        // % Equation for finding Earth's radius given the latitude (code in Matlab).  The Earth is actually
        //% slightly spheroidal, with the radius maximum at equator and minimum at poles. 
        //a           = 6378.137;             % Major axis of Earth (km)
        //b           = 6356.752;             % Minor axis of Earth (km)
        //http://en.wikipedia.org/wiki/Earth_radiusEarth
        //radiusEarth      =
        //sqrt(((a^2*cosd(source_lat))^2+(b^2*sind(source_lat))^2)/((a*cosd(source_lat))^2+(b*sind(source_lat))^2));   % (km)   

        [XmlIgnore]
        private const double EarthRadius_Meters = 6372795.477598;
        [XmlIgnore]
        private const double RadiansToDegrees = (180.0 / Math.PI);
        [XmlIgnore]
        private const double DegreesToRadians = (Math.PI / 180.0);
        [XmlIgnore]
        private const UInt32 Magic = 0x21954ef6;
        [XmlIgnore]
        private static EarthCoordinate earthOrigin = new EarthCoordinate(0.0, 0.0);
        [XmlIgnore]
        private const double KnotsToMetersPerSecond = 0.514444444444444;
        [XmlIgnore]
        public static int Size { get { return 16; } }

        public EarthCoordinate(double Latitude_degrees, double Longitude_degrees)
        {
            this.Latitude_degrees = Latitude_degrees;
            this.Longitude_degrees = Longitude_degrees;
        }

        public EarthCoordinate()
        {
        }

        public EarthCoordinate(EarthCoordinate startCoordinate, double bearingDegrees, double distanceMeters)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            double bearingRadians = ((Math.PI / 180f) * bearingDegrees + 2 * Math.PI) % (2 * Math.PI);
            double arcLengthTraveled = distanceMeters / EarthRadius_Meters;

            double endLatRadians = Math.Asin((Math.Sin(startCoordinate.Latitude_radians) * Math.Cos(arcLengthTraveled)) +
                (Math.Cos(startCoordinate.Latitude_radians) * Math.Sin(arcLengthTraveled) * Math.Cos(bearingRadians)));
            double endLonRadians = startCoordinate.Longitude_radians + Math.Atan2(Math.Sin(bearingRadians) * Math.Sin(arcLengthTraveled) * Math.Cos(startCoordinate.Latitude_radians),
                Math.Cos(arcLengthTraveled) - (Math.Sin(startCoordinate.Latitude_radians) * Math.Sin(endLatRadians)));

            Latitude_degrees = endLatRadians * RadiansToDegrees;
            this.Longitude_degrees = endLonRadians * RadiansToDegrees;
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
            if (stream.ReadUInt32() != Magic)
                throw new FormatException("Attempted to read invalid data into an EarthCoordinate");
            Latitude_degrees = stream.ReadSingle();
            Longitude_degrees = stream.ReadSingle();
        }

        // This routine compares one earth coordinate to another for purposes of sorting.
        // If the current point is to the north or west of the other point it is presumed to be less than the other point
        // If the current point is neither further north nor further west than the other point, then if the other point
        // is further north or further west than the current point, the current point is presumed to be greater than the other
        // point.
        // If neither is true, the points are held to be equal.
        public int CompareTo(EarthCoordinate other)
        {
            if ((this.Latitude_degrees < other.Latitude_degrees) || (this.Longitude_degrees < other.Longitude_degrees))
                return -1;
            if ((this.Latitude_degrees > other.Latitude_degrees) || (this.Longitude_degrees > other.Longitude_degrees))
                return 1;
            return 0;
        }

        public bool Equals(EarthCoordinate other)
        {
            if ((Latitude_degrees == other.Latitude_degrees) && (Longitude_degrees == other.Longitude_degrees))
                return true;
            return false;
        }

        public static double DistanceBetween_Meters(EarthCoordinate A, EarthCoordinate B)
        {
            return A.GetDistanceTo_Meters(B);
        }

        public double GetDistanceTo_Meters(EarthCoordinate earthCoordinate)
        {
            // http://www.movable-type.co.uk/scripts/LatLong.html

            double distance = Math.Acos(Math.Sin(this.Latitude_radians) * Math.Sin(earthCoordinate.Latitude_radians) +
                Math.Cos(this.Latitude_radians) * Math.Cos(earthCoordinate.Latitude_radians) *
                Math.Cos(earthCoordinate.Longitude_radians - this.Longitude_radians)) * EarthRadius_Meters;

            return distance;
        }

        public double GetBearingTo_Degrees(EarthCoordinate earthCoordinate)
        {
            return ((GetBearingTo_Radians(earthCoordinate) * RadiansToDegrees) + 360.0) % 360.0;
        }

        public double GetBearingTo_Radians(EarthCoordinate earthCoordinate)
        {
            double deltaLong;
            double LatRad = this.Latitude_radians;
            double LongRad = this.Longitude_radians;
            double x, y;

            deltaLong = earthCoordinate.Longitude_radians - LongRad;

            y = Math.Sin(deltaLong) * Math.Cos(earthCoordinate.Latitude_radians);
            x = Math.Cos(LatRad) * Math.Sin(earthCoordinate.Latitude_radians) -
                Math.Sin(LatRad) * Math.Cos(earthCoordinate.Latitude_radians) * Math.Cos(deltaLong);
            return Math.Atan2(y, x);
        }

        public void Move(Course Course, double Distance_Meters)
        {
            Move(Course.Degrees, Distance_Meters);
        }

        public void Move(double Bearing_Degrees, double Distance_Meters)
        {
            double dR = Distance_Meters / EarthRadius_Meters;
            double bearingRad = Bearing_Degrees * (Math.PI / 180.0);
            double LatRad = this.Latitude_radians;
            double LongRad = this.Longitude_radians;
            // moves the current EarthCoordinate by Distance_Meters along Bearing_Degrees
            // lat2 = asin(sin(lat1)*cos(d/R) + cos(lat1)*sin(d/R)*cos(brng))
            // lon2 = lon1 + atan2(sin(brng)*sin(d/R)*cos(lat1), cos(d/R)−sin(lat1)*sin(lat2))
            this.Latitude_radians = Math.Asin(Math.Sin(LatRad) * Math.Cos(dR) + Math.Cos(LatRad) * Math.Sin(dR) * Math.Cos(bearingRad));
            this.Longitude_radians = LongRad + Math.Atan2(Math.Sin(bearingRad) * Math.Sin(dR) * Math.Cos(LatRad), Math.Cos(dR) - Math.Sin(LatRad) * Math.Sin(LatRad));
        }

        public static double Bearing_Degrees(EarthCoordinate startCoordinate, EarthCoordinate endCoordinate)
        {
            // See: http://www.movable-type.co.uk/scripts/LatLong.html

            double lonDeltaRadians = endCoordinate.Longitude_radians - startCoordinate.Longitude_radians;

            double bearingRadians = Math.Atan2(Math.Sin(lonDeltaRadians) * Math.Cos(endCoordinate.Latitude_radians),
                (Math.Cos(startCoordinate.Latitude_radians) * Math.Sin(endCoordinate.Latitude_radians)) -
                (Math.Sin(startCoordinate.Latitude_radians) * Math.Cos(endCoordinate.Latitude_radians) * Math.Cos(lonDeltaRadians)));

            return (bearingRadians * (180 / Math.PI) + 360) % 360f;
        }


        public static EarthCoordinate BearingAndDistanceFrom(EarthCoordinate StartPoint, double Bearing_Degrees, double Distance_Meters)
        {
            EarthCoordinate EndPoint = new EarthCoordinate(StartPoint);
            EndPoint.Move(Bearing_Degrees, Distance_Meters);
            return EndPoint;
        }

        [XmlElement]
        public double Latitude_degrees { get; set; }
        [XmlElement]
        public double Longitude_degrees { get; set; }

        [XmlIgnore]
        public double Latitude_radians { get { return Latitude_degrees * (Math.PI / 180f); } set { Latitude_degrees = value * (180.0 / Math.PI); } }
        [XmlIgnore]
        public double Longitude_radians { get { return Longitude_degrees * (Math.PI / 180f); } set { Longitude_degrees = value * (180.0 / Math.PI); } }
        public override string ToString() { return "(" + Latitude_degrees + ", " + Longitude_degrees + ")"; }

        public static EarthCoordinate operator +(EarthCoordinate A, EarthCoordinate B)
        {
            return new EarthCoordinate(A.Latitude_degrees + B.Latitude_degrees, A.Longitude_degrees + B.Longitude_degrees);
        }

        public static EarthCoordinate operator -(EarthCoordinate A, EarthCoordinate B)
        {
            return new EarthCoordinate(A.Latitude_degrees - B.Latitude_degrees, A.Longitude_degrees - B.Longitude_degrees);
        }

        public static EarthCoordinate operator *(EarthCoordinate A, double ScaleFactor)
        {
            return new EarthCoordinate(A.Latitude_degrees * ScaleFactor, A.Longitude_degrees * ScaleFactor);
        }

        public static EarthCoordinate operator /(EarthCoordinate A, double ScaleFactor)
        {
            return new EarthCoordinate(A.Latitude_degrees / ScaleFactor, A.Longitude_degrees / ScaleFactor);
        }

        public virtual void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float)Latitude_degrees);
            stream.Write((float)Longitude_degrees);
        }

        public static explicit operator PointF(EarthCoordinate e)
        {
            return new System.Drawing.PointF((float)e.Longitude_degrees, (float)e.Latitude_degrees);
        }

        public void Compare(EarthCoordinate that)
        {
            Console.WriteLine("that coordinate's distance from this one is {0}m at a bearing of {1}deg", this.GetDistanceTo_Meters(that), this.GetBearingTo_Degrees(that));
        }
    }

    [System.ComponentModel.TypeConverter(typeof(EarthCoordinate3DTypeConverter))]
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
        [XmlIgnore]
        private const UInt32 Magic = 0x0908cd2f;
        [XmlIgnore]
        public static new int Size { get { return 8 + EarthCoordinate.Size; } }

        public EarthCoordinate3D(double latitudeDegrees, double longitudeDegrees, double Elevation_Meters)
            : base(latitudeDegrees, longitudeDegrees)
        {
            this.Elevation_meters = Elevation_Meters;
        }

        public EarthCoordinate3D(EarthCoordinate TwoDCoord)
            : base(TwoDCoord.Latitude_degrees, TwoDCoord.Longitude_degrees)
        {
            this.Elevation_meters = 0;
        }

        public EarthCoordinate3D(EarthCoordinate3D ThreeDCoord)
            : base(ThreeDCoord.Latitude_degrees, ThreeDCoord.Longitude_degrees)
        {
            this.Elevation_meters = ThreeDCoord.Elevation_meters;
        }

        public EarthCoordinate3D(BinaryReader stream)
            : base(stream)
        {
            if (stream.ReadUInt32() != Magic)
                throw new FormatException("Attempted to read invalid data into an EarthCoordinate3D");
            Elevation_meters = stream.ReadSingle();
        }

        public EarthCoordinate3D() { }

        public static EarthCoordinate3D operator +(EarthCoordinate3D A, EarthCoordinate3D B)
        {
            return new EarthCoordinate3D(A.Latitude_degrees + B.Latitude_degrees, A.Longitude_degrees + B.Longitude_degrees, A.Elevation_meters + B.Elevation_meters);
        }

        public static EarthCoordinate3D operator +(EarthCoordinate3D A, EarthCoordinate B)
        {
            return new EarthCoordinate3D(A.Latitude_degrees + B.Latitude_degrees, A.Longitude_degrees + B.Longitude_degrees, A.Elevation_meters);
        }

        public static EarthCoordinate3D operator -(EarthCoordinate3D A, EarthCoordinate3D B)
        {
            return new EarthCoordinate3D(A.Latitude_degrees - B.Latitude_degrees, A.Longitude_degrees - B.Longitude_degrees, A.Elevation_meters - B.Elevation_meters);
        }

        public static EarthCoordinate3D operator *(EarthCoordinate3D A, double ScaleFactor)
        {
            return new EarthCoordinate3D(A.Latitude_degrees * ScaleFactor, A.Longitude_degrees * ScaleFactor, A.Elevation_meters + ScaleFactor);
        }

        public static EarthCoordinate3D operator /(EarthCoordinate3D A, double ScaleFactor)
        {
            return new EarthCoordinate3D(A.Latitude_degrees / ScaleFactor, A.Longitude_degrees / ScaleFactor, A.Elevation_meters / ScaleFactor);
        }

        public bool Equals(EarthCoordinate3D that)
        {
            return ((this.Latitude_degrees == that.Latitude_degrees) && (this.Longitude_degrees == that.Longitude_degrees) && (this.Elevation_meters == that.Elevation_meters));
        }
        
        public override void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float)Elevation_meters);
            base.Write(stream);
        }

    }

}
