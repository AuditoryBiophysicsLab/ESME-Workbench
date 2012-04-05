//#define KML_Support
using System;
using HRC.Navigation;
#if KML_Support
using KMLib;
using KMLib.Feature;
using KMLib.Geometry;
using Core.Geometry;
#endif
namespace HRC.Navigation
{
    public class GeoSegment : IGeoExtent
    {
        public GeoSegment(Geo start, Geo end) { Segment = new GeoArray(start, end); Initialize(); }
        public GeoSegment(GeoSegment geoSegment) { Segment = new GeoArray(geoSegment[0], geoSegment[1]); Initialize(); }
        public GeoSegment(double startLat, double startLon, double endLat, double endLon) { Segment = new GeoArray(new Geo(startLat, startLon), new Geo(endLat, endLon)); Initialize(); }
        static readonly double _oneMeterInRadians = Geo.KilometersToRadians(0.001);
        public GeoSegment(Geo start, double length, double azimuth)
        {
            var oneMeterSegment = new GeoSegment(start, start.Offset(_oneMeterInRadians, Geo.DegreesToRadians(azimuth)));
            var segment = oneMeterSegment.Scale(length / _oneMeterInRadians);
            Segment = new GeoArray(segment[0], segment[1]); 
            Initialize();
        }

        public GeoArray Segment { get; private set; }
        public Geo this[int index]
        {
            get
            {
                if (index < 0 || index > 1)
                    throw new IndexOutOfRangeException("The only valid indices for a GeoSegment are 0 and 1");
                return Segment[index];
            }
        }

        #region Initialize() and properties set by it
        void Initialize()
        {
            Normal = this[0].CrossNormalize(this[1]);
            Center = this[0].MidPoint(this[1]);
            BoundingCircle = Segment.BoundingCircle;
            LengthRadians = Segment[0].DistanceRadians(Segment[1]);
        }
        public double LengthRadians { get; private set; }
        public BoundingCircle BoundingCircle { get; private set; }
        public Geo Normal { get; private set; }
        public Geo Center { get; private set; }
        #endregion
#if KML_Support
        #region KML support
        public Placemark Placemark
        {
            get
            {
                return _placemark ?? (_placemark = new Placemark
                {
                    LineString = new LineString
                    {
                        Tessellate = true,
                        coordinates = new Coordinates
                        {
                            new Point3D(Segment[0].Longitude, Segment[0].Latitude, 0),
                            new Point3D(Segment[1].Longitude, Segment[1].Latitude, 0)
                        }
                    }
                });

            }
        }
        Placemark _placemark;
        #endregion
#endif
    }

    public class SurfaceVector
    {
        public SurfaceVector() {}

        /// <summary>
        /// Construct a 2-D vector from an X,Y pair
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public SurfaceVector(double x, double y)
        {
            X = x; Y = y;
            _endGeo = new Geo(Y, X);
        }
        /// <summary>
        /// Construct a normalized (unit-length) 2-D vector from a GeoSegment
        /// </summary>
        /// <param name="segment"></param>
        public SurfaceVector(GeoSegment segment)
        {
            X = segment[1].Longitude - segment[0].Longitude;
            Y = segment[1].Latitude - segment[0].Latitude;
            Normalize();
            _endGeo = new Geo(Y, X);
        }
        void Normalize()
        {
            var length = Math.Sqrt(X * X + Y * Y);
            X = X / length;
            Y = Y / length;
        }

        public double X { get; private set; }
        public double Y { get; private set; }
        public double Dot(SurfaceVector b) { return (X * b.X + Y * b.Y); }
        public SurfaceVector Scale(double s) { return new SurfaceVector(X * s, Y * s); }
        public double Length { get { return Math.Sqrt(Dot(this)); } }
        public SurfaceVector Normalized { get { return Scale(1.0 / Length); } }
        public SurfaceVector Rotate(double angleRadians)
        {
            var cosTheta = Math.Cos(angleRadians);
            var sinTheta = Math.Sin(angleRadians);
            return new SurfaceVector(X * cosTheta - Y * sinTheta, X * sinTheta + Y * cosTheta);
        }

        public double Azimuth { get { return Math.Atan2(Y, X); } }
        public double GeoAzimuth { get { return Math.Atan2(X, Y); } }
        public double Angle(SurfaceVector other) { return Math.Atan2(other.Y, other.X) - Math.Atan2(Y, X); }

        #region Operators
        public static SurfaceVector operator +(SurfaceVector a, SurfaceVector b) { return new SurfaceVector(a.X + b.X, a.Y + b.Y); }
        public static SurfaceVector operator -(SurfaceVector a, SurfaceVector b) { return new SurfaceVector(a.X - b.X, a.Y - b.Y); }
        public static bool operator !=(SurfaceVector a, SurfaceVector b) { return !(a == b); }
        // ReSharper disable PossibleNullReferenceException
        public static bool operator ==(SurfaceVector a, SurfaceVector b)
        {
            return MoreMath.IsApproximatelyEqual(a.X, b.X) &&
                   MoreMath.IsApproximatelyEqual(a.Y, b.Y);
        }
        // ReSharper restore PossibleNullReferenceException
        #endregion
        #region Object overrides
        public override int GetHashCode()
        {
            var result = 17;
            var lx = BitConverter.DoubleToInt64Bits(X);
            var ly = BitConverter.DoubleToInt64Bits(Y);
            result = 31 * result + (int)(lx ^ (lx >> 32));
            result = 31 * result + (int)(ly ^ (ly >> 32));
            return result;
        }
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(obj, this)) return true;
            if (!(obj is SurfaceVector)) return false;
            return this == obj as SurfaceVector;
        }
        public override string ToString() { return string.Format("SurfaceVector[X = {0}, Y = {1}]", Math.Round(X, 10), Math.Round(Y, 10)); }
        #endregion
        readonly Geo _startGeo = new Geo(0, 0);
        readonly Geo _endGeo = new Geo(0, 0);
        public Geo this[int index]
        {
            get
            {
                if (index != 0 && index != 1) throw new IndexOutOfRangeException("Bad!");
                if (index == 0) return _startGeo;
                return _endGeo;
            }
        }

#if KML_Support
        #region KML support
        public Placemark Placemark
        {
            get
            {
                return _placemark ?? (_placemark = new Placemark
                {
                    LineString = new LineString
                    {
                        Tessellate = true,
                        coordinates = new Coordinates
                        {
                            new Point3D(0, 0, 0),
                            new Point3D(X, Y, 0)
                        }
                    }
                });

            }
        }
        Placemark _placemark;
        #endregion
#endif
    }
}