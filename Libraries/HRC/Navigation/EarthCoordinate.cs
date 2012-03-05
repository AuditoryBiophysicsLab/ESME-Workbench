using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Xml.Serialization;
using Point = System.Windows.Point;

namespace HRC.Navigation
{
#if false
    [Serializable]
    public class EarthCoordinate : Geo, IEqualityComparer<EarthCoordinate>, IEquatable<EarthCoordinate>, IComparable<EarthCoordinate>, IComparer<EarthCoordinate>
    {
        [XmlIgnore]
        const UInt32 Magic = 0x21954ef6;

        public EarthCoordinate(double latitude, double longitude) : base(latitude, longitude, true) { }

        public EarthCoordinate() { }

        public EarthCoordinate(Geo source) : this(source.Latitude, source.Longitude){ }

        public EarthCoordinate(Geo startCoordinate, double bearing, double distanceMeters)
        {
            var result = FromDegrees(startCoordinate.Latitude, startCoordinate.Longitude).Offset(KilometersToRadians(distanceMeters / 1000), DegreesToRadians(bearing));
            X = result.X;
            Y = result.Y;
            Z = result.Z;
            UpdateLatitudeLongitude();
        }

        public EarthCoordinate(BinaryReader stream)
        {
            if (stream.ReadUInt32() != Magic) throw new FormatException("Attempted to read invalid data into an EarthCoordinate");
            Initialize(stream.ReadSingle(), stream.ReadSingle());
        }

        [XmlIgnore]
        public static int Size
        {
            get { return 16; }
        }

        #region IEquatable<EarthCoordinate> Members

        public bool Equals(EarthCoordinate other) { return Compare(other) == 0; }

        #endregion

        public virtual void Write(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write((float)Latitude);
            stream.Write((float)Longitude);
        }
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
        /// <param name="geo"></param>
        /// <returns>Distance in meters</returns>
        public double DistanceTo(EarthCoordinate earthCoordinate)
        {
            return DistanceKilometers(earthCoordinate) * 1000;
        }

        /// <summary>
        /// The bearing to another EarthCoordinate
        /// </summary>
        /// <param name="geo"></param>
        /// <returns>Bearing in degrees from true north</returns>
        public double BearingTo(EarthCoordinate earthCoordinate)
        {
            return RadiansToDegrees(Azimuth(earthCoordinate));
        }

        /// <summary>
        /// The bearing to another EarthCoordinate
        /// </summary>
        /// <param name="geo"></param>
        /// <returns>Bearing in radians from true north</returns>
        public double BearingToInRadians(EarthCoordinate earthCoordinate) { return Azimuth(earthCoordinate); }

        /// <summary>
        /// Move the current point over a defined course and distance
        /// </summary>
        /// <param name = "course">Course object</param>
        /// <param name = "distance">in meters</param>
        public void Move(Course course, double distance) { Move(course.Degrees, distance); }

        /// <summary>
        /// Move the current point over a defined course and distance
        /// </summary>
        /// <param name = "bearing">in degrees</param>
        /// <param name = "distance">in meters</param>
        public void Move(double bearing, double distance)
        {
            var result = FromDegrees(Latitude, Longitude).Offset(KilometersToRadians(distance / 1000), DegreesToRadians(bearing));
            Initialize(result.Latitude, result.Longitude);
        }

        public static EarthCoordinate Move(EarthCoordinate start, double bearing, double distance)
        {
            return new EarthCoordinate(start.Offset(KilometersToRadians(distance / 1000), DegreesToRadians(bearing)));
        }

        /// <summary>
        /// The bearing between two EarthCoordinates
        /// </summary>
        /// <param name="startCoordinate"></param>
        /// <param name="endCoordinate"></param>
        /// <returns>The bearing from startCoordinate to endCoordinate, in degrees</returns>
        public static double BearingBetween(EarthCoordinate startCoordinate, EarthCoordinate endCoordinate)
        {
            return RadiansToDegrees(startCoordinate.Azimuth(endCoordinate));
        }

        public static explicit operator PointF(EarthCoordinate e) { return new PointF((float)e.Longitude, (float)e.Latitude); }
        public static implicit operator Point(EarthCoordinate e) { return new Point(e.Longitude, e.Latitude); }
        public int CompareTo(EarthCoordinate other) { return Compare(this, other); }

        /// <summary>
        /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="e1"/> and <paramref name="e2"/>, 
        /// as shown in the following table.
        /// Value Meaning 
        /// Return value of less than zero means that <paramref name="e1"/> is less than <paramref name="e2"/>.
        /// Return value of zero means that <paramref name="e1"/> equals <paramref name="e2"/>.
        /// Return value of greater than zero means that <paramref name="e1"/> is greater than <paramref name="e2"/>.
        /// </returns>
        /// <param name="e1">The first object to compare.</param>
        /// <param name="e2">The second object to compare.</param>
        int IComparer<EarthCoordinate>.Compare(EarthCoordinate e1, EarthCoordinate e2) { return e1.Compare(e2); }

        public static int Compare(EarthCoordinate e1, EarthCoordinate e2) { return e1.Compare(e2); }

        public int Compare(EarthCoordinate e2)
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

        public new string ToString() { return string.Format("({0:0.0000},{1:0.0000})", Latitude, Longitude); }

        #region Implementation of IEqualityComparer<in EarthCoordinate>
        public bool Equals(EarthCoordinate x, EarthCoordinate y)
        {
            return (x.LatKey == y.LatKey) && (x.LonKey == y.LonKey);
        }

        public int GetHashCode(EarthCoordinate obj)
        {
            if (_hashCode != int.MinValue) return _hashCode;
            _hashCode = (LonKey << 10) ^ LatKey;
            return _hashCode;
        }
        [XmlIgnore]
        int _hashCode = int.MinValue;
        #endregion
    }

    [Serializable]
    public class EarthCoordinate<T> : EarthCoordinate
    {
        public EarthCoordinate() { }
        public EarthCoordinate(Geo source) : base(source) { }
        public EarthCoordinate(Geo source, T data) : base(source) { Data = data; }
        public EarthCoordinate(double latitude, double longitude) : base(latitude, longitude) { }
        public EarthCoordinate(double latitude, double longitude, T data) : base(latitude, longitude) { Data = data; }

        public T Data { get; set; }
    }

    [TypeConverter(typeof (EarthCoordinate3DTypeConverter))]
    [Serializable]
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

    public class EarthCoordinateComparer : IComparer<EarthCoordinate>
    {
        public int Compare(EarthCoordinate x, EarthCoordinate y) { return x.Compare(y); }
    }
#endif
}