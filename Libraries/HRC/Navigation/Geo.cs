using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using Point = System.Windows.Point;

namespace HRC.Navigation
{
    [Serializable]
    public class Geo : EqualityComparer<Geo>, IEquatable<Geo>, IComparable<Geo>, IComparer<Geo>, INotifyPropertyChanged, IDeserializationCallback
    {
        const double Flattening = 1.0 / 298.257223563;
        const double FlatteningC = (1.0 - Flattening) * (1.0 - Flattening);

        const double MetersPerNm = 1852;
        const double NpdLterm1 = 111412.84 / MetersPerNm;
        const double NpdLterm2 = -93.5 / MetersPerNm;
        const double NpdLterm3 = 0.118 / MetersPerNm;

        /// <summary>
        ///   Nautical miles per degree at a given latitude
        /// </summary>
        /// <param name = "latdeg"></param>
        /// <returns></returns>
        public static double NauticalMilesPerDegreeAtLatitude(double latdeg)
        {
            var lat = (latdeg * Math.PI) / 180.0;
            return (NpdLterm1 * Math.Cos(lat) + NpdLterm2 * Math.Cos(3 * lat) + NpdLterm3 * Math.Cos(5 * lat));
        }

        /// <summary>
        ///   Convert from geographic to geocentric latitude (radians)
        /// </summary>
        /// <param name = "geographicLatitude"></param>
        /// <returns></returns>
        public static double GeocentricLatitude(double geographicLatitude) { return Math.Atan((Math.Tan(geographicLatitude) * FlatteningC)); }

        /// <summary>
        ///   Convert from geocentric to geographic latitude (radians)
        /// </summary>
        /// <param name = "geocentricLatitude"></param>
        /// <returns></returns>
        public static double GeographicLatitude(double geocentricLatitude) { return Math.Atan(Math.Tan(geocentricLatitude) / FlatteningC); }

        /** Convert from degrees to radians. */
        public static double DegreesToRadians(double degrees) { return Navigation.Length.DECIMAL_DEGREE.toRadians(degrees); }

        /** Convert from radians to degrees. */
        public static double RadiansToDegrees(double radians) { return Navigation.Length.DECIMAL_DEGREE.fromRadians(radians); }

        /** Convert radians to kilometers. * */
        public static double RadiansToKilometers(double radians) { return Navigation.Length.KM.fromRadians(radians); }

        /** Convert kilometers to radians. * */
        public static double KilometersToRadians(double km) { return Navigation.Length.KM.toRadians(km); }

        /** Convert radians to nauticalMiles. * */
        public static double RadiansToNauticalMiles(double radians) { return Navigation.Length.NM.fromRadians(radians); }

        /** Convert nautical miles to radians. * */
        public static double NauticalMilesToRadians(double nm) { return Navigation.Length.NM.toRadians(nm); }

        public Geo() { }

        /**
     * Construct a Geo from its latitude and longitude.
     * 
     * @param lat latitude in decimal degrees.
     * @param lon longitude in decimal degrees.
     */
        public Geo(double lat, double lon) { Initialize(lat, lon); }

        /**
     * Construct a Geo from its latitude and longitude.
     * 
     * @param lat latitude.
     * @param lon longitude.
     * @param isDegrees should be true if the lat/lon are specified in decimal
     *        degrees, false if they are radians.
     */

        public Geo(double lat, double lon, bool isDegrees)
        {
            if (isDegrees) Initialize(lat, lon);
            else InitializeRadians(lat, lon);
        }

        /** Construct a Geo from its parts. */

        public Geo(double x, double y, double z)
        {
            X = x;
            Y = y;
            Z = z;
            UpdateLatitudeLongitude();
        }

        /** Construct a Geo from another Geo. */
        public Geo(Geo geo) : this(geo.X, geo.Y, geo.Z) { }

        public Geo(Geo geo, double azimuthDegrees, double distanceMeters)
        {
            var result = geo.Offset(KilometersToRadians(distanceMeters / 1000), DegreesToRadians(azimuthDegrees));
            X = result.X;
            Y = result.Y;
            Z = result.Z;
            UpdateLatitudeLongitude();
        }

        public static Geo FromRadians(double latr, double lonr)
        {
            var rlat = GeocentricLatitude(latr);
            var c = Math.Cos(rlat);
            return new Geo(c * Math.Cos(lonr), c * Math.Sin(lonr), Math.Sin(rlat));
        }

        public static Geo FromDegrees(double latd, double lond) { return FromRadians(DegreesToRadians(latd), DegreesToRadians(lond)); }

        public static Geo FromXYZ(double x, double y, double z) { return new Geo(x, y, z); }

        public static Geo FromGeo(Geo p) { return new Geo(p.X, p.Y, p.Z); }

        public static Geo Move(Geo geo, double azimuthDegrees, double distanceMeters)
        {
            return geo.Offset(KilometersToRadians(distanceMeters / 1000), DegreesToRadians(azimuthDegrees));
        }

        public Geo Move(double azimuthDegrees, double distanceMeters)
        {
            return Offset(KilometersToRadians(distanceMeters / 1000), DegreesToRadians(azimuthDegrees));
        }

        //public static implicit operator Geo(EarthCoordinate earthCoordinate) { return FromDegrees(earthCoordinate.Latitude, earthCoordinate.Longitude); }
        //public static explicit operator Geo(EarthCoordinate earthCoordinate) { return FromDegrees(earthCoordinate.Latitude, earthCoordinate.Longitude); }

        /**
     * Initialize this Geo to match another.
     * 
     * @param g
     */

        public void Initialize(Geo g)
        {
            X = g.X;
            Y = g.Y;
            Z = g.Z;
            UpdateLatitudeLongitude();
        }

        /**
     * Initialize this Geo with new parameters.
     * 
     * @param x
     * @param y
     * @param z
     */

        public void Initialize(double x, double y, double z)
        {
            X = x;
            Y = y;
            Z = z;
            UpdateLatitudeLongitude();
        }

        /**
     * Initialize this Geo with to represent coordinates.
     * 
     * @param lat latitude in decimal degrees.
     * @param lon longitude in decimal degrees.
     */
        public void Initialize(double lat, double lon) { InitializeRadians(DegreesToRadians(lat), DegreesToRadians(lon)); }

        /**
     * Initialize this Geo with to represent coordinates.
     * 
     * @param lat latitude in radians.
     * @param lon longitude in radians.
     */

        public void InitializeRadians(double lat, double lon)
        {
            var rlat = GeocentricLatitude(lat);
            var c = Math.Cos(rlat);
            X = c * Math.Cos(lon);
            Y = c * Math.Sin(lon);
            Z = Math.Sin(rlat);
            UpdateLatitudeLongitude();
        }

        protected void UpdateLatitudeLongitude()
        {
            _latitudeRadians = GeographicLatitude(Math.Atan2(Z, Math.Sqrt(X * X + Y * Y)));
            if (double.IsNaN(_latitudeRadians)) throw new InvalidOperationException("NaN is an invalid value for LatitudeRadians");
            _latitudeDegrees = RadiansToDegrees(_latitudeRadians);
            if (double.IsNaN(_latitudeDegrees)) throw new InvalidOperationException("NaN is an invalid value for Latitude");
            _longitudeRadians = Math.Atan2(Y, X);
            if (double.IsNaN(_longitudeRadians)) throw new InvalidOperationException("NaN is an invalid value for LongitudeRadians");
            _longitudeDegrees = RadiansToDegrees(_longitudeRadians);
            if (double.IsNaN(_longitudeDegrees)) throw new InvalidOperationException("NaN is an invalid value for Longitude");

            NotifyPropertyChanged(LatitudeChangedEventArgs);
            NotifyPropertyChanged(LongitudeChangedEventArgs);
            NotifyPropertyChanged(LatitudeRadiansChangedEventArgs);
            NotifyPropertyChanged(LongitudeRadiansChangedEventArgs);
        }
        [NonSerialized, XmlIgnore]
        double _latitudeDegrees, _latitudeRadians, _longitudeDegrees, _longitudeRadians;
        static readonly PropertyChangedEventArgs LatitudeChangedEventArgs = ObservableHelper.CreateArgs<Geo>(x => x.Latitude);
        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<Geo>(x => x.Longitude);
        static readonly PropertyChangedEventArgs LatitudeRadiansChangedEventArgs = ObservableHelper.CreateArgs<Geo>(x => x.LatitudeRadians);
        static readonly PropertyChangedEventArgs LongitudeRadiansChangedEventArgs = ObservableHelper.CreateArgs<Geo>(x => x.LongitudeRadians);

        /**
     * Find the midpoint Geo between this one and another on a Great Circle line
     * between the two. The result is undefined of the two points are antipodes.
     * 
     * @param g2
     * @return midpoint Geo.
     */
        public Geo MidPoint(Geo g2) { return Add(g2).Normalize(); }

        /**
     * Find the midpoint Geo between this one and another on a Great Circle line
     * between the two. The result is undefined of the two points are antipodes.
     * 
     * @param g2
     * @param ret a Geo value to set returned values in. Do not pass in a null
     *        value.
     * @return midpoint Geo.
     */
        public Geo MidPoint(Geo g2, Geo ret) { return Add(g2).Normalize(); }

        public Geo Interpolate(Geo g2, double x) { return Scale(x).Add(g2.Scale(1 - x)).Normalize(); }

        public new String ToString() { return "Geo[" + Latitude + "," + Longitude + "]"; }
        public void OnDeserialization(object sender) { UpdateLatitudeLongitude(); }

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

        /// <summary>
        ///   Latitude, in degrees
        /// </summary>
        public double Latitude
        {
            get { return _latitudeDegrees; }
            set { InitializeRadians(DegreesToRadians(value), LongitudeRadians); }
        }

        /// <summary>
        ///   Latitude, in radians
        /// </summary>
        [XmlIgnore]
        public double LatitudeRadians
        {
            get { return _latitudeRadians; }
            set { InitializeRadians(value, LongitudeRadians); }
        }

        /// <summary>
        ///   Longitude, in degrees
        /// </summary>
        public double Longitude
        {
            get { return _longitudeDegrees; }
            set { InitializeRadians(LatitudeRadians, DegreesToRadians(value)); }
        }

        /// <summary>
        ///   Longitude, in radians
        /// </summary>
        [XmlIgnore]
        public double LongitudeRadians
        {
            get { return _longitudeRadians; }
            set { InitializeRadians(LatitudeRadians, value); }
        }

        /// <summary>
        ///   X, in internal axis representation (positive to the right side of screen).
        /// </summary>
        [XmlIgnore]
        public double X { get; protected set; }

        /// <summary>
        ///   Y, in internal axis representation (positive into screen).
        /// </summary>
        [XmlIgnore]
        public double Y { get; protected set; }

        /// <summary>
        ///   X, in internal axis representation (positive to top of screen).
        /// </summary>
        [XmlIgnore]
        public double Z { get; protected set; }

        /// <summary>
        ///   North pole
        /// </summary>
        public static Geo North = new Geo(0.0, 0.0, 1.0);

        /// <summary>
        ///   Dot product
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public double Dot(Geo b) { return ((X * b.X) + (Y * b.Y) + (Z * b.Z)); }

        /// <summary>
        ///   Dot product
        /// </summary>
        /// <param name = "a"></param>
        /// <param name = "b"></param>
        /// <returns></returns>
        public static double Dot(Geo a, Geo b) { return ((a.X * b.X) + (a.Y * b.Y) + (a.Z * b.Z)); }

        /// <summary>
        ///   Euclidean length
        /// </summary>
        [XmlIgnore]
        public double Length
        {
            get { return Math.Sqrt(Dot(this)); }
            set
            {
                // It's tempting to call LatitudeRadians here, but it changes the
                // angle. I think we want to keep the angles the same, and just extend
                // x, y, z, and then let the latitudes get refigured out for the
                // ellipsoid when they are asked for.
                var rlat = Math.Atan2(Z, Math.Sqrt(X * X + Y * Y));
                var rlon = LongitudeRadians;

                var c = value * Math.Cos(rlat);
                X = c * Math.Cos(rlon);
                Y = c * Math.Sin(rlon);
                Z = value * Math.Sin(rlat);
            }
        }

        /// <summary>
        ///   Returns a new Geo, constructed from the current one, scaled along all three axes by Z
        /// </summary>
        /// <param name = "s"></param>
        /// <returns></returns>
        public Geo Scale(double s) { return new Geo(X * s, Y * s, Z * s); }

        /// <summary>
        ///   Returns a unit length vector parallel to this.
        /// </summary>
        /// <returns></returns>
        public Geo Normalize() { return Scale(1.0 / Length); }

        /// <summary>
        ///   Vector cross product
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public Geo Cross(Geo b) { return new Geo(Y * b.Z - Z * b.Y, Z * b.X - X * b.Z, X * b.Y - Y * b.X); }

        /// <summary>
        ///   Equivalent to Cross(b).Length
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public double CrossLength(Geo b) { return Cross(b).Length; }

        /// <summary>
        ///   Equivalent to Cross(b).Normalize();
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public Geo CrossNormalize(Geo b) { return Cross(b).Normalize(); }

        /// <summary>
        ///   Equivalent to a.Cross(b).Normalize()
        /// </summary>
        /// <param name = "a"></param>
        /// <param name = "b"></param>
        /// <returns></returns>
        public static Geo CrossNormalize(Geo a, Geo b) { return a.CrossNormalize(b); }

        /// <summary>
        ///   Vector sum of this + b
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public Geo Add(Geo b) { return new Geo(X + b.X, Y + b.Y, Z + b.Z); }

        /// <summary>
        ///   Vector difference of this - b
        /// </summary>
        /// <param name = "b"></param>
        /// <returns></returns>
        public Geo Subtract(Geo b) { return new Geo(X - b.X, Y - b.Y, Z - b.Z); }

        /// <summary>
        ///   True if this is equal to v2, false otherwise
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public bool Equals(Geo v2) { return X == v2.X && Y == v2.Y && Z == v2.Z; }

        /// <summary>
        ///   Angular distance, in radians between this and v2
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public double DistanceRadians(Geo v2) { return Math.Atan2(v2.CrossLength(this), v2.Dot(this)); }

        /// <summary>
        ///   Angular distance, in radians between v1 and v2
        /// </summary>
        /// <param name = "v1"></param>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public static double DistanceRadians(Geo v1, Geo v2) { return v1.DistanceRadians(v2); }

        /// <summary>
        ///   Angular distance, in radians between the two lat lon points
        /// </summary>
        /// <param name = "lat1"></param>
        /// <param name = "lon1"></param>
        /// <param name = "lat2"></param>
        /// <param name = "lon2"></param>
        /// <returns></returns>
        public static double DistanceRadians(double lat1, double lon1, double lat2, double lon2) { return DistanceRadians(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /// <summary>
        ///   Distance in kilometers from this to v2
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public double DistanceKilometers(Geo v2) { return RadiansToKilometers(DistanceRadians(v2)); }

        /// <summary>
        ///   Distance in kilometers from v1 to v2
        /// </summary>
        /// <param name = "v1"></param>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public static double DistanceKilometers(Geo v1, Geo v2) { return v1.DistanceKilometers(v2); }

        /// <summary>
        ///   Distance in kilometers between the two lat lon points
        /// </summary>
        /// <param name = "lat1"></param>
        /// <param name = "lon1"></param>
        /// <param name = "lat2"></param>
        /// <param name = "lon2"></param>
        /// <returns></returns>
        public static double DistanceKilometers(double lat1, double lon1, double lat2, double lon2) { return DistanceKilometers(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /// <summary>
        ///   Distance in meters from this to v2
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public double DistanceMeters(Geo v2) { return RadiansToKilometers(DistanceRadians(v2)) * 1000; }

        /// <summary>
        ///   Distance in meters from v1 to v2
        /// </summary>
        /// <param name = "v1"></param>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public static double DistanceMeters(Geo v1, Geo v2) { return v1.DistanceKilometers(v2) * 1000; }

        /// <summary>
        ///   Distance in meters between the two lat lon points
        /// </summary>
        /// <param name = "lat1"></param>
        /// <param name = "lon1"></param>
        /// <param name = "lat2"></param>
        /// <param name = "lon2"></param>
        /// <returns></returns>
        public static double DistanceMeters(double lat1, double lon1, double lat2, double lon2) { return DistanceKilometers(new Geo(lat1, lon1), new Geo(lat2, lon2)) * 1000; }

        /// <summary>
        ///   Distance in nautical miles from this to v2
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public double DistanceNauticalMiles(Geo v2) { return RadiansToNauticalMiles(DistanceRadians(v2)); }

        /// <summary>
        ///   Distance in nautical miles from v1 to v2
        /// </summary>
        /// <param name = "v1"></param>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public static double DistanceNauticalMiles(Geo v1, Geo v2) { return v1.DistanceNauticalMiles(v2); }

        /// <summary>
        ///   Distance in nautical miles between the two lat lon points
        /// </summary>
        /// <param name = "lat1"></param>
        /// <param name = "lon1"></param>
        /// <param name = "lat2"></param>
        /// <param name = "lon2"></param>
        /// <returns></returns>
        public static double DistanceNauticalMiles(double lat1, double lon1, double lat2, double lon2) { return DistanceNauticalMiles(new Geo(lat1, lon1), new Geo(lat2, lon2)); }

        /// <summary>
        ///   Azimuth in radians from this to v2
        /// </summary>
        /// <param name = "v2"></param>
        /// <returns></returns>
        public double Azimuth(Geo v2)
        {
            // n1 is the great circle representing the meridian of this. n2 is the
            // great circle between this and v2. The azimuth is the angle between
            // them but we specialized the cross product.
            // CrossNormalize is needed for geos of different length.
            var n1 = North.CrossNormalize(this);
            var n2 = v2.CrossNormalize(this);
            var az = Math.Atan2(-North.Dot(n2), n1.Dot(n2));
            return (az > 0.0) ? az : 2.0 * Math.PI + az;
        }

        public double AzimuthDegrees(Geo v2) { return Azimuth(v2) * (180.0 / Math.PI); }

        /// <summary>
        ///   Given 3 points on a sphere, p0, p1, p2, return the angle between them in radians.
        /// </summary>
        /// <param name = "p0"></param>
        /// <param name = "p1"></param>
        /// <param name = "p2"></param>
        /// <returns></returns>
        public static double AngleRadians(Geo p0, Geo p1, Geo p2) { return Math.PI - p0.Cross(p1).DistanceRadians(p1.Cross(p2)); }

        /// <summary>
        ///   Is the point, p, within radius radians of the great circle segment between this and v2?
        /// </summary>
        /// <param name = "v2"></param>
        /// <param name = "radius"></param>
        /// <param name = "p"></param>
        /// <returns></returns>
        public bool IsInside(Geo v2, double radius, Geo p)
        {
            // Allocate a Geo to be reused for all of these calculations, instead of
            // creating 3 of them that are just thrown away. There's one more we
            // still need to allocate, for dp below.
            var tmp = new Geo();

            // gc is a unit vector perpendicular to the plane defined by v1 and v2
            var gc = CrossNormalize(v2, tmp);

            // |gc . p| is the size of the projection of p onto gc (the normal of
            // (v1,v2) cos(pi/2-r) is effectively the size of the projection of a
            // vector along gc of the radius length. If the former is larger than
            // the latter, than p is further than radius from arc, so must not be
            // isInside
            if (Math.Abs(gc.Dot(p)) > Math.Cos((Math.PI / 2.0) - radius)) return false;

            // If p is within radius of either endpoint, then we know it isInside
            if (DistanceRadians(p) <= radius || v2.DistanceRadians(p) <= radius) return true;

            // d is the vector from the v2 to v1
            var d = v2.Subtract(this);

            // dp is the vector from p to v1
            var dp = p.Subtract(this);

            // size is the size of the projection of dp onto n
            var size = d.Normalize().Dot(dp);

            // p is inside iff size>=0 and size <= L 
            return (0 <= size && size <= d.Length);
        }

        /// <summary>
        ///   Do the segments v1-v2 and p1-p2 come within radius (radians) of each other?
        /// </summary>
        /// <param name = "v1"></param>
        /// <param name = "v2"></param>
        /// <param name = "radius"></param>
        /// <param name = "p1"></param>
        /// <param name = "p2"></param>
        /// <returns></returns>
        public static bool IsInside(Geo v1, Geo v2, double radius, Geo p1, Geo p2) { return v1.IsInside(v2, radius, p1) || v1.IsInside(v2, radius, p2) || p1.IsInside(p2, radius, v1) || p1.IsInside(p2, radius, v2); }

        /// <summary>
        ///   Static version of isInside uses conventional (decimal degree) coordinates.
        /// </summary>
        /// <param name = "lat1"></param>
        /// <param name = "lon1"></param>
        /// <param name = "lat2"></param>
        /// <param name = "lon2"></param>
        /// <param name = "radius"></param>
        /// <param name = "lat3"></param>
        /// <param name = "lon3"></param>
        /// <returns></returns>
        public static bool IsInside(double lat1, double lon1, double lat2, double lon2, double radius, double lat3, double lon3) { return (new Geo(lat1, lon1)).IsInside(new Geo(lat2, lon2), radius, new Geo(lat3, lon3)); }

        /**
     * Is Geo p inside the time bubble along the great circle segment from this
     * to v2 looking forward forwardRadius and backward backwardRadius.
     */
        public bool InBubble(Geo v2, double forwardRadius, double backRadius, Geo p) { return DistanceRadians(p) <= ((v2.Subtract(this).Normalize().Dot(p.Subtract(this)) > 0.0) ? forwardRadius : backRadius); }

        /** Returns the point opposite this point on the earth. */
        public Geo Antipode() { return Scale(-1.0); }

        /**
     * Find the intersection of the great circle between this and q and the
     * great circle normal to r.
     * <p>
     * 
     * That is, find the point, y, lying between this and q such that
     * 
     * <pre>
     *                              
     *  y = [x*this + (1-x)*q]*c
     *  where c = 1/y.dot(y) is a factor for normalizing y.
     *  y.dot(r) = 0
     *  substituting:
     *  [x*this + (1-x)*q]*c.dot(r) = 0 or
     *  [x*this + (1-x)*q].dot(r) = 0
     *  x*this.dot(r) + (1-x)*q.dot(r) = 0
     *  x*a + (1-x)*b = 0
     *  x = -b/(a - b)
     *                                        
     * </pre>
     * 
     * We assume that this and q are less than 180 degrees appart. When this and
     * q are 180 degrees appart, the point -y is also a valid intersection.
     * <p>
     * Alternatively the intersection point, y, satisfies y.dot(r) = 0
     * y.dot(this.crossNormalize(q)) = 0 which is satisfied by y =
     * r.crossNormalize(this.crossNormalize(q));
     * 
     */

        public Geo Intersect(Geo q, Geo r)
        {
            var a = Dot(r);
            var b = q.Dot(r);
            var x = -b / (a - b);
            // This still results in one Geo being allocated and lost, in the
            // q.scale call.
            return Scale(x).Add(q.Scale(1.0 - x)).Normalize();
        }

        /// <summary>
        ///   Compute a polygonal approximation of an arc centered at pc, beginning at p0 and ending at p1, going clockwise and including the two end points.
        /// </summary>
        /// <param name = "pc">center point</param>
        /// <param name = "p0">starting point</param>
        /// <param name = "p1">ending point</param>
        /// <param name = "err">The maximum angle between approximates allowed, in radians. Smaller values will look better but will result in more returned points.</param>
        /// <returns></returns>
        public static Geo[] ApproximateArc(Geo pc, Geo p0, Geo p1, double err)
        {
            var theta = AngleRadians(p0, pc, p1);
            // if the rest of the code is undefined in this situation, just skip it.
            if (Double.IsNaN(theta))
                return new[]
                {
                    p0, p1
                };

            var n = (int)(2.0 + Math.Abs(theta / err)); // number of points
            // (counting the end
            // points)
            var result = new Geo[n];
            result[0] = p0;
            var dtheta = theta / (n - 1);

            var rho = 0.0; // angle starts at 0 (directly at p0)

            for (var i = 1; i < n - 1; i++)
            {
                rho += dtheta;
                // Rotate p0 around this so it has the right azimuth.
                result[i] = Rotation.Rotate(pc, 2.0 * Math.PI - rho, p0);
            }
            result[n - 1] = p1;

            return result;
        }

        public Geo[] ApproximateArc(Geo p0, Geo p1, double err) { return ApproximateArc(this, p0, p1, err); }

        /// <summary>
        ///   Returns a Geo that is distance (radians), and azimuth (radians) away from this.
        ///   Undefined at the north pole, at which point "azimuth" is undefined.
        /// </summary>
        /// <param name = "distance">Distance of this to the target point in radians.</param>
        /// <param name = "azimuth">Direction of target point from this, in radians, clockwise from north.</param>
        /// <returns></returns>
        public Geo Offset(double distance, double azimuth)
        {
            // m is normal the the meridian through this.
            var m = CrossNormalize(North);
            // p is a point on the meridian distance <tt>distance</tt> from this.
            // Geo p = (new Rotation(m, distance)).rotate(this);
            var p = Rotation.Rotate(m, distance, this);
            // Rotate p around this so it has the right azimuth.
            return Rotation.Rotate(this, 2.0 * Math.PI - azimuth, p);
        }

        /// <summary>
        ///   Returns a Geo that is distance (radians), and azimuth (radians) away from origin.
        ///   Undefined at the north pole, at which point "azimuth" is undefined.
        /// </summary>
        /// <param name = "origin">Starting point</param>
        /// <param name = "distance">Distance of origin to the target point in radians.</param>
        /// <param name = "azimuth">Direction of target point from origin, in radians, clockwise from north.</param>
        /// <returns></returns>
        public static Geo Offset(Geo origin, double distance, double azimuth) { return origin.Offset(distance, azimuth); }

        public static explicit operator Point(Geo e) { return new Point(e.Longitude, e.Latitude); }
        public static explicit operator PointF(Geo e) { return new PointF((float)e.Longitude, (float)e.Latitude); }

#if false
    /**
     * Computes the area of a polygon on the surface of a unit sphere given an
     * enumeration of its point.. For a non unit sphere, multiply this by the
     * radius of sphere squared.
     */

        public static double area(Enumeration vs)
        {
            var count = 0;
            double area = 0;
            var v0 = (Geo) vs.nextElement();
            var v1 = (Geo) vs.nextElement();
            var p0 = v0;
            var p1 = v1;
            Geo p2 = null;
            while (vs.hasMoreElements())
            {
                count = count + 1;
                p2 = (Geo) vs.nextElement();
                area = area + angle(p0, p1, p2);
                p0 = p1;
                p1 = p2;
            }

            count = count + 1;
            p2 = v0;
            area = area + angle(p0, p1, p2);
            p0 = p1;
            p1 = p2;

            count = count + 1;
            p2 = v1;
            area = area + angle(p0, p1, p2);

            return area - (count - 2) * Math.PI;
        }
    /** alias for computeCorridor(path, radius, radians(10), true) * */
        public static Geo[] computeCorridor(Geo[] path, double radius) { return computeCorridor(path, radius, radians(10.0), true); }

        /**
     * Wrap a fixed-distance corridor around an (open) path, as specified by an
     * array of Geo.
     * 
     * @param path Open path, must not have repeated points or consecutive
     *        antipodes.
     * @param radius Distance from path to widen corridor, in angular radians.
     * @param err maximum angle of rounded edges, in radians. If 0, will
     *        directly cut outside bends.
     * @param capp iff true, will round end caps
     * @return a closed polygon representing the specified corridor around the
     *         path.
     * 
     */

        public static Geo[] computeCorridor(Geo[] path, double radius, double err, bool capp)
        {
            if (path == null || radius <= 0.0)
            {
                return new Geo[]
                       {};
            }
            // assert path!=null;
            // assert radius > 0.0;

            int pl = path.length;
            if (pl < 2) return null;

            // polygon will be right[0],...,right[n],left[m],...,left[0]
            var right = new ArrayList((int) (pl * 1.5));
            var left = new ArrayList((int) (pl * 1.5));

            Geo g0 = null; // previous point
            Geo n0 = null; // previous normal vector
            Geo l0 = null;
            Geo r0 = null;

            var g1 = path[0]; // current point

            for (var i = 1; i < pl; i++)
            {
                var g2 = path[i]; // next point
                var n1 = g1.crossNormalize(g2); // n is perpendicular to the vector
                // from g1 to g2
                n1 = n1.scale(radius); // normalize to radius
                // these are the offsets on the g2 side at g1
                var r1b = g1.add(n1);
                var l1b = g1.subtract(n1);

                if (n0 == null)
                {
                    if (capp && err > 0)
                    {
                        // start cap
                        var arc = approximateArc(g1, l1b, r1b, err);
                        for (var j = arc.length - 1; j >= 0; j--)
                        {
                            right.add(arc[j]);
                        }
                    }
                    else
                    {
                        // no previous point - we'll just be square
                        right.add(l1b);
                        left.add(r1b);
                    }
                    // advance normals
                    l0 = l1b;
                    r0 = r1b;
                }
                else
                {
                    // otherwise, compute a more complex shape

                    // these are the right and left on the g0 side of g1
                    var r1a = g1.add(n0);
                    var l1a = g1.subtract(n0);

                    var handed = g0.cross(g1).dot(g2); // right or left handed
                    // divergence
                    if (handed > 0)
                    {
                        // left needs two points, right needs 1
                        if (err > 0)
                        {
                            var arc = approximateArc(g1, l1b, l1a, err);
                            for (var j = arc.length - 1; j >= 0; j--)
                            {
                                right.add(arc[j]);
                            }
                        }
                        else
                        {
                            right.add(l1a);
                            right.add(l1b);
                        }
                        l0 = l1b;

                        Geo ip = Intersection.segmentsIntersect(r0, r1a, r1b, g2.add(n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            left.add(ip);
                        }
                        else
                        {
                            left.add(r1a);
                            left.add(r1b);
                        }
                        r0 = ip;
                    }
                    else
                    {
                        Geo ip = Intersection.segmentsIntersect(l0, l1a, l1b, g2.subtract(n1));
                        // if they intersect, take the intersection, else use the
                        // points and punt
                        if (ip != null)
                        {
                            right.add(ip);
                        }
                        else
                        {
                            right.add(l1a);
                            right.add(l1b);
                        }
                        l0 = ip;
                        if (err > 0)
                        {
                            var arc = approximateArc(g1, r1a, r1b, err);
                            for (var j = 0; j < arc.length; j++)
                            {
                                left.add(arc[j]);
                            }
                        }
                        else
                        {
                            left.add(r1a);
                            left.add(r1b);
                        }
                        r0 = r1b;
                    }
                }

                // advance points
                g0 = g1;
                n0 = n1;
                g1 = g2;
            }

            // finish it off
            var rn = g1.subtract(n0);
            var ln = g1.add(n0);
            if (capp && err > 0)
            {
                // end cap
                var arc = approximateArc(g1, ln, rn, err);
                for (var j = arc.length - 1; j >= 0; j--)
                {
                    right.add(arc[j]);
                }
            }
            else
            {
                right.add(rn);
                left.add(ln);
            }

            int ll = right.size();
            int rl = left.size();
            var result = new Geo[ll + rl];
            for (var i = 0; i < ll; i++)
            {
                result[i] = (Geo) right.get(i);
            }
            var j = ll;
            for (var i = rl - 1; i >= 0; i--)
            {
                result[j++] = (Geo) left.get(i);
            }
            return result;
        }
        //
        // Follows are a series of Geo array operations as useful utilities
        //
        /**
     * convert a String containing space-separated pairs of comma-separated
     * decimal lat-lon pairs into a Geo array.
     */
        public static Geo[] PosToGa(String coords) { return PosToGa(coords.Split(' ')); }

        /**
     * Convert an array of strings with comma-separated decimal lat,lon pairs
     * into a Geo array
     */

        public static Geo[] PosToGa(String[] coords)
        {
            // convert to floating lat/lon degrees
            var ga = new Geo[coords.Length];
            for (var i = 0; i < coords.Length; i++)
            {
                var ll = coords[i].Split(',');
                ga[i] = FromDegrees(Double.Parse(ll[0]), Double.Parse(ll[1]));
            }
            return ga;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values).
     * 
     * @return the ll array provided, or a new array of lla is null.
     */

        public static double[] GaToLLa(Geo[] ga, double[] lla)
        {
            if (lla == null)
            {
                lla = new double[2 * ga.Length];
            }

            for (var i = 0; i < ga.Length; i++)
            {
                var g = ga[i];
                lla[i * 2] = g.Latitude;
                lla[i * 2 + 1] = g.Longitude;
            }
            return lla;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values).
     * 
     * @return the ll array provided, or a new array of lla is null.
     */

        public static float[] GaToLLa(Geo[] ga, float[] lla)
        {
            if (lla == null)
            {
                lla = new float[2 * ga.Length];
            }

            for (var i = 0; i < ga.Length; i++)
            {
                var g = ga[i];
                lla[i * 2] = (float) g.Latitude;
                lla[i * 2 + 1] = (float) g.Longitude;
            }
            return lla;
        }

        /**
     * Convert a Geo array into a floating point lat lon array (alternating lat
     * and lon values)
     */
        public static float[] GaToLLa(Geo[] ga) { return GaToLLa(ga, new float[2 * ga.Length]); }

        /**
     * Return a Geo array with the duplicates removed. May arbitrarily mutate
     * the input array.
     */

        public static Geo[] RemoveDups(Geo[] ga)
        {
            var r = new Geo[ga.Length];
            var p = 0;
            foreach (var t in ga) 
            {
                if (p != 0 && (r[p - 1].Equals(t))) continue;
                r[p] = t;
                p++;
            }
            if (p != ga.Length)
            {
                var x = new Geo[p];
                Array.Copy(r, 0, x, 0, p);
                return x;
            }
            return ga;
        }

        /**
     * Convert a float array of alternating lat and lon pairs into a Geo array.
     */
        public static Geo[] LLaToGa(float[] lla) { return LLaToGa(lla, true); }

        /**
     * Convert a float array of alternating lat and lon pairs into a Geo array.
     */

        public static Geo[] LLaToGa(float[] lla, bool isDegrees)
        {
            var r = new Geo[lla.Length / 2];
            for (var i = 0; i < lla.Length / 2; i++)
            {
                if (isDegrees)
                {
                    r[i] = FromDegrees(lla[i * 2], lla[i * 2 + 1]);
                }
                else
                {
                    r[i] = FromRadians(lla[i * 2], lla[i * 2 + 1]);
                }
            }
            return r;
        }

        /**
     * Convert a double array of alternating lat and lon pairs into a Geo array.
     */
        public static Geo[] LLaToGa(double[] lla) { return LLaToGa(lla, true); }

        /**
     * Convert a double array of alternating lat and lon pairs into a Geo array.
     */

        public static Geo[] LLaToGa(double[] lla, bool isDegrees)
        {
            var r = new Geo[lla.Length / 2];
            for (var i = 0; i < lla.Length / 2; i++)
            {
                if (isDegrees)
                {
                    r[i] = FromDegrees(lla[i * 2], lla[i * 2 + 1]);
                }
                else
                {
                    r[i] = FromRadians(lla[i * 2], lla[i * 2 + 1]);
                }
            }
            return r;
        }

        /*
        /// <summary>
        ///   return a float array of alternating lat lon pairs where the first and
        ///   last pair are the same, thus closing the path, by adding a point if
        ///   needed. Does not mutate the input.
        /// </summary>
        */
        public static float[] CloseLLa(float[] lla)
        {
            var l = lla.Length;
            var s = (l / 2) - 1;
            if (lla[0] == lla[s * 2] && lla[1] == lla[s * 2 + 1]) return lla;
            var llx = new float[l + 2];
            for (var i = 0; i < l; i++)
            {
                llx[i] = lla[i];
            }
            llx[l] = lla[0];
            llx[l + 1] = lla[1];
            return llx;
        }

        /*
        /// <summary>
        ///   return a Geo array where the first and last elements are the same, thus
        ///   closing the path, by adding a point if needed. Does not mutate the input.
        /// </summary>
        /// <param name = "ga"></param>
        /// <returns></returns>
        */
        public static Geo[] CloseGa(Geo[] ga)
        {
            var l = ga.Length;
            if (ga[0].Equals(ga[l - 1])) return ga;
            var x = new Geo[l + 1];
            Array.Copy(ga, 0, x, 0, l);
            x[l] = ga[0];
            return x;
        }
#endif

        #region Overrides of EqualityComparer<Geo>
        public override bool Equals(Geo x, Geo y) { return (x.DistanceKilometers(y) < 0.01); }

        public override int GetHashCode(Geo obj)
        {
            if (obj == null) return 0;
            return (int)Math.Round(obj.X * 1e6, 0) ^ (int)Math.Round(obj.Y * 1e6, 0) ^ (int)Math.Round(obj.Z * 1e6, 0);
        }
        #endregion

        #region INotifyPropertyChanged Members
        [NonSerialized, XmlIgnore] PropertyChangedEventHandler _propertyChanged;

        public event PropertyChangedEventHandler PropertyChanged
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            add { _propertyChanged = (PropertyChangedEventHandler)Delegate.Combine(_propertyChanged, value); }
            [MethodImpl(MethodImplOptions.Synchronized)]
            remove { _propertyChanged = (PropertyChangedEventHandler)Delegate.Remove(_propertyChanged, value); }
        }

        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = _propertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
                if (handler.Target is DispatcherObject) ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                else handler(this, e);
        }
        #endregion

        public int CompareTo(Geo other) { return Compare(this, other); }
        int IComparer<Geo>.Compare(Geo e1, Geo e2) { return e1.Compare(e2); }
        public static int Compare(Geo e1, Geo e2) { return e1.Compare(e2); }
        public int Compare(Geo e2)
        {
            var latCompare = LatKey.CompareTo(e2.LatKey);
            return latCompare != 0 ? latCompare : LonKey.CompareTo(e2.LonKey);
        }
        [XmlIgnore]
        int _latKey = int.MinValue;
        [XmlIgnore]
        int LatKey
        {
            get
            {
                if (_latKey != int.MinValue) return _latKey;
                _latKey = (int)Math.Round(Latitude * 10000);
                return _latKey;
            }
        }
        [XmlIgnore]
        int _lonKey = int.MinValue;
        [XmlIgnore]
        int LonKey
        {
            get
            {
                if (_lonKey != int.MinValue) return _lonKey;
                _lonKey = (int)Math.Round(Longitude * 10000);
                return _lonKey;
            }
        }
    }

    public class Geo<T> : Geo
    {
        public Geo() {}
        public Geo(double lat, double lon) : base(lat, lon) { }
        public Geo(double lat, double lon, bool isDegrees) : base(lat, lon, isDegrees) { }
        public Geo(double lat, double lon, T data) : base(lat, lon) { Data = data; }
        public Geo(double x, double y, double z) : base(x, y, z) { }
        public Geo(Geo geo) : base(geo) { }
        public Geo(Geo geo, T data) : base(geo) { Data = data; }

        public T Data { get; set; }
    }
}