//#define KML_Support
using System;
using System.Collections.Generic;
using System.IO;

#if KML_Support
using KMLib.Feature;
using KMLib.Geometry;
#endif

namespace HRC.Navigation
{
    /// <summary>
    /// A class that represents a point on the Earth as a three dimensional unit length vector, rather than latitude and longitude
    /// </summary>
    public class Geo : IEquatable<object>, IEquatable<Geo>, IGeoExtent, IComparer<Geo>
    {
        #region Constants for the shape of the Earth
        /***************************************************************************
         * Constants for the shape of the earth. see
         * http://www.gfy.ku.dk/%7Eiag/HB2000/part4/groten.htm
         **************************************************************************/
        public static double flattening = 1.0/298.257223563;
        public static double FLATTENING_C = (1.0 - flattening)*(1.0 - flattening);

        public static double METERS_PER_NM = 1852;
        private static readonly double NPD_LTERM1 = 111412.84/METERS_PER_NM;
        private static readonly double NPD_LTERM2 = -93.5/METERS_PER_NM;
        private static readonly double NPD_LTERM3 = 0.118/METERS_PER_NM;
        #endregion

        #region Constructors
        public Geo() : this(0, 0, 0) { }

        /// <summary>
        /// Construct a Geo from a latitude and longitude (in degrees)
        /// </summary>
        /// <param name="lat"></param>
        /// <param name="lon"></param>
        public Geo(double lat, double lon) : this(lat, lon, true) { }

        /// <summary>
        /// Construct a Geo from a latitude and longitude, in radians or degrees
        /// </summary>
        /// <param name="lat"></param>
        /// <param name="lon"></param>
        /// <param name="isDegrees">true if the latitude and longitude are in degrees, false if they are in radians</param>
        public Geo(double lat, double lon, bool isDegrees)
        {
            if (isDegrees)
            {
                lat = MoreMath.DegreesToRadians(lat);
                lon = MoreMath.DegreesToRadians(lon);
            }
            var rlat = GeocentricLatitude(lat);
            var c = Math.Cos(rlat);
            X = c * Math.Cos(lon);
            Y = c * Math.Sin(lon);
            Z = Math.Sin(rlat);
        }

        /// <summary>
        /// Construct a Geo from it's X, Y, Z values
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name="z"></param>
        public Geo(double x, double y, double z)
        {
            X = x;
            Y = y;
            Z = z;
        }

        /// <summary>
        /// Construct a Geo from another Geo
        /// </summary>
        /// <param name="geo"></param>
        public Geo(Geo geo) : this(geo.X, geo.Y, geo.Z){}        
        #endregion

        #region Static utility functions
        /**
         * Compute nautical miles per degree at a specified latitude (in degrees).
         * Calculation from NIMA: http://pollux.nss.nima.mil/calc/degree.html
         */
        public static double NauticalMilesPerDegreeAtLatitude(double latdeg)
        {
            var lat = MoreMath.DegreesToRadians(latdeg);
            return (NPD_LTERM1*Math.Cos(lat) + NPD_LTERM2*Math.Cos(3*lat) + NPD_LTERM3*Math.Cos(5*lat));
        }

        /** Convert from geographic to geocentric latitude (radians) */

        public static double GeocentricLatitude(double geographicLatitude)
        {
            return Math.Atan((Math.Tan(geographicLatitude)*FLATTENING_C));
        }

        /** Convert from geocentric to geographic latitude (radians) */

        public static double GeographicLatitude(double geocentricLatitude)
        {
            return Math.Atan(Math.Tan(geocentricLatitude)/FLATTENING_C);
        }

        /** Convert from degrees to radians. */

        public static double DegreesToRadians(double degrees)
        {
            return Navigation.Length.Degrees.ToRadians(degrees);
        }

        /** Convert from radians to degrees. */

        public static double RadiansToDegrees(double radians)
        {
            return Navigation.Length.Degrees.FromRadians(radians);
        }

        /** Convert radians to kilometers. * */

        public static double RadiansToKilometers(double radians)
        {
            return Navigation.Length.Kilometers.FromRadians(radians);
        }

        /** Convert kilometers to radians. * */

        public static double KilometersToRadians(double km)
        {
            return Navigation.Length.Kilometers.ToRadians(km);
        }

        /** Convert radians to nauticalMiles. * */

        public static double RadiansToNauticalMiles(double radians)
        {
            return Navigation.Length.NauticalMiles.FromRadians(radians);
        }

        /** Convert nautical miles to radians. * */

        public static double NauticalMilesToRadians(double nm)
        {
            return Navigation.Length.NauticalMiles.ToRadians(nm);
        }

        public static Geo Read(BinaryReader reader)
        {
            return new Geo(reader.ReadDouble(), reader.ReadDouble(), reader.ReadDouble());
        }
        #endregion

        #region Properties
        /// <summary>
        /// North pole
        /// </summary>
        public static Geo North { get { return new Geo(0, 0, 1); } }

        /// <summary>
        /// Empty geo
        /// </summary>
        public static Geo Empty { get { return new Geo(0, 0, 0); } }

        /// <summary>
        /// Latitude, in degrees
        /// </summary>
        public double Latitude
        {
            get { return !double.IsNaN(_latitude) ? _latitude : (_latitude = RadiansToDegrees(LatitudeRadians)); }
        }
        double _latitude = double.NaN;

        /// <summary>
        /// Longitude, in degrees
        /// </summary>
        public double Longitude
        {
            get { return !double.IsNaN(_longitude) ? _longitude : (_longitude = RadiansToDegrees(LongitudeRadians)); }
        }
        double _longitude = double.NaN;

        /// <summary>
        /// Latitude, in radians
        /// </summary>
        public double LatitudeRadians
        {
            get { return !double.IsNaN(_latitudeRadians) ? _latitudeRadians : (_latitudeRadians = GeographicLatitude(Math.Atan2(Z, Math.Sqrt(X*X + Y*Y)))); }
        }
        double _latitudeRadians = double.NaN;

        /// <summary>
        /// Longitude, in radians
        /// </summary>
        public double LongitudeRadians
        {
            get { return !double.IsNaN(_longitudeRadians) ? _longitudeRadians : (_longitudeRadians = Math.Atan2(Y, X)); }
        }
        double _longitudeRadians = double.NaN;

        /// <summary>
        /// X component of the Geo vector
        /// </summary>
        public double X { get; private set; }

        /// <summary>
        /// Y component of the Geo vector
        /// </summary>
        public double Y { get; private set; }
        
        /// <summary>
        /// Z component of the Geo vector
        /// </summary>
        public double Z { get; private set; }

        /// <summary>
        /// The Bounding Circle of the current geo, with a radius of zero
        /// </summary>
        public BoundingCircle BoundingCircle { get { return new BoundingCircle(this, 0.0); } }

        /// <summary>
        /// Euclidean length
        /// </summary>
        public double Length { get { return Math.Sqrt(Dot(this)); } }

        /// <summary>
        /// Returns the current Geo as unit-Geo
        /// </summary>
        public Geo Normalized { get { return Scale(1.0 / Length); } }
        
        /// <summary>
        /// The Geo opposite this Geo on the earth
        /// </summary>
        public Geo Antipode { get { return Scale(-1.0); } }

        #endregion

        #region Operators
        public static Geo operator +(Geo a, Geo b) { return new Geo(a.X + b.X, a.Y + b.Y, a.Z + b.Z); }
        public static Geo operator -(Geo a, Geo b) { return new Geo(a.X - b.X, a.Y - b.Y, a.Z - b.Z); }
        public static bool operator !=(Geo a, Geo b) { return !(a == b); }
        // ReSharper disable PossibleNullReferenceException
        public static bool operator ==(Geo a, Geo b)
        {
            if (((object)a == null) && (object)b == null) return true;
            if (((object)a != null) && (object)b == null) return false;
            if ((object)a == null) return false;
            return MoreMath.IsApproximatelyEqual(a.X, b.X) &&
                   MoreMath.IsApproximatelyEqual(a.Y, b.Y) &&
                   MoreMath.IsApproximatelyEqual(a.Z, b.Z);
        }
        // ReSharper restore PossibleNullReferenceException
        #endregion

        #region Helper functions
        /// <summary>
        /// Find the midpoint Geo between this one and another on a Great Circle line between the two. 
        /// The result is undefined of the two points are antipodes.
        /// </summary>
        /// <param name="g2"></param>
        /// <returns></returns>
        public Geo MidPoint(Geo g2)
        {
            if (g2 == Antipode) throw new InvalidOperationException("Cannot find the midpoint of two points that are antipodes of one another");
            return (this + g2).Normalized;
        }

        /// <summary>
        /// Dot product
        /// </summary>
        /// <param name="b"></param>
        /// <returns></returns>
        public double Dot(Geo b) { return (X*b.X + Y*b.Y + Z*b.Z); }

        /// <summary>
        /// Return a Geo scaled to the desired length
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        public Geo Scale(double s) { return new Geo(X * s, Y * s, Z * s); }

        /// <summary>
        /// Cross product
        /// </summary>
        /// <param name="b"></param>
        /// <returns></returns>
        public Geo Cross(Geo b) { return new Geo(Y * b.Z - Z * b.Y, Z * b.X - X * b.Z, X * b.Y - Y * b.X); }

        /// <summary>
        /// Normalized cross product
        /// </summary>
        /// <param name="b"></param>
        /// <returns></returns>
        public Geo CrossNormalize(Geo b)
        {
            var x = Y*b.Z - Z*b.Y;
            var y = Z*b.X - X*b.Z;
            var z = X*b.Y - Y*b.X;
            var length = Math.Sqrt(x * x + y * y + z * z);
            return new Geo(x / length, y / length, z / length);
        }

        /// <summary>
        /// Equality test
        /// </summary>
        /// <param name="other"></param>
        /// <returns>true if this is equivalent to other, false otherwise</returns>
        public bool Equals(Geo other) { return this == other; }

        /// <summary>
        /// Angular distance to v2, radians
        /// </summary>
        /// <param name="v2"></param>
        /// <returns></returns>
        public double DistanceRadians(Geo v2) { return Math.Atan2(v2.Cross(this).Length, v2.Dot(this)); }

        /// <summary>
        /// Distance to v2, in kilometers
        /// </summary>
        /// <param name="v2"></param>
        /// <returns></returns>
        public double DistanceKilometers(Geo v2) { return RadiansToKilometers(DistanceRadians(v2)); }

        /// <summary>
        /// Distance to v2, in nautical miles
        /// </summary>
        /// <param name="v2"></param>
        /// <returns></returns>
        public double DistanceNauticalMiles(Geo v2) { return RadiansToNauticalMiles(DistanceRadians(v2)); }

        /// <summary>
        /// Azimuth in radians from this to v2.
        /// </summary>
        /// <param name="v2"></param>
        /// <returns>angle, in radians, between this and v2, or NaN if the two geos are identical.</returns>
        public double StrictAzimuth(Geo v2)
        {
            var n1 = North.Cross(this).Normalized;
            var n2 = v2.Cross(this).Normalized;
            var az = Math.Atan2(-North.Dot(n2), n1.Dot(n2));
            return (az >= 0.0) ? az : 2.0*Math.PI + az;
        }

        /// <summary>
        /// Azimuth in radians from this to v2.
        /// </summary>
        /// <param name="v2"></param>
        /// <returns>angle, in radians, between this and v2, and zero instead of NaN if the two geos are identical.</returns>
        public double Azimuth(Geo v2)
        {
            var ret = StrictAzimuth(v2);
            return double.IsNaN(ret) ? 0 : ret;
        }

        /// <summary>
        /// Gives the angle, in radians, between p1 and p2 using the current point as the vertex
        /// </summary>
        /// <param name="p1"></param>
        /// <param name="p2"></param>
        /// <returns></returns>
        public double Angle(Geo p1, Geo p2) { return Math.PI - Cross(p1).DistanceRadians(p1.Cross(p2)); }

        /// <summary>
        /// Returns a Geo that is distance (radians), and azimuth (radians) away from the current geo. 
        /// This is undefined at the north pole, at which point "azimuth" is undefined.
        /// </summary>
        /// <param name="distance">distance of this to the target point in radians.</param>
        /// <param name="azimuth">Direction of target point from this, in radians, clockwise from north.</param>
        /// <returns></returns>
        public Geo Offset(double distance, double azimuth)
        {
            // m is normal the the meridian through this.
            var m = Cross(North).Normalized;
            // p is a point on the meridian distance <tt>distance</tt> from this.
            // Geo p = (new Rotation(m, distance)).rotate(this);
            var p = Rotation.Rotate(m, this, distance, false);
            // Rotate p around this so it has the right azimuth.
            return Rotation.Rotate(this, p, 2.0 * Math.PI - azimuth, false);
        }

        public void Write(BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
            writer.Write(Z);
        }
        #endregion

        #region Object overrides
        public override int GetHashCode()
        {
            var result = 17;
            var lx = BitConverter.DoubleToInt64Bits(X);
            var ly = BitConverter.DoubleToInt64Bits(Y);
            var lz = BitConverter.DoubleToInt64Bits(Z);
            result = 31 * result + (int)(lx ^ (lx >> 32));
            result = 31 * result + (int)(ly ^ (ly >> 32));
            result = 31 * result + (int)(lz ^ (lz >> 32));
            return result;
        }
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(obj, this)) return true;
            if (!(obj is Geo)) return false;
            return this == obj as Geo;
        }

        public int Compare(Geo x, Geo y)
        {
            var compare = x.Latitude.CompareTo(y.Latitude);
            return compare != 0 ? compare : x.Longitude.CompareTo(y.Longitude);
        }
        public override string ToString() { return string.Format("Geo[{0}, {1}]", Math.Round(Latitude, 10), Math.Round(Longitude, 10)); }
        #endregion

#if KML_Support
        #region KML support
        public Placemark Placemark { get { return _placemark ?? (_placemark = new Placemark {Point = new KmlPoint(Longitude, Latitude, 0)}); } }
        Placemark _placemark;
        #endregion
#endif
        public static Geo Deserialize(BinaryReader reader)
        {
            var x = reader.ReadDouble();
            var y = reader.ReadDouble();
            var z = reader.ReadDouble();
            return new Geo(x, y, z);
        }

        public void Serialize(BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
            writer.Write(Z);
        }
    }

    public class Geo<T> : Geo, IComparer<Geo<T>>
    {
        public Geo() { }
        public Geo(double lat, double lon) : base(lat, lon) { }
        public Geo(double lat, double lon, bool isDegrees) : base(lat, lon, isDegrees) { }
        public Geo(double lat, double lon, T data) : base(lat, lon) { Data = data; }
        public Geo(double x, double y, double z) : base(x, y, z) { }
        public Geo(Geo geo) : base(geo) { }
        public Geo(Geo geo, T data) : base(geo) { Data = data; }

        public T Data { get; set; }
        public int Compare(Geo<T> x, Geo<T> y)
        {
            var compare = x.Latitude.CompareTo(y.Latitude);
            return compare != 0 ? compare : x.Longitude.CompareTo(y.Longitude);
        }
    }
}
