﻿//#define KML_Support
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using HRC.Navigation;

#if KML_Support
using KMLib;
using KMLib.Feature;
using KMLib.Geometry;
using Core.Geometry;
#endif

namespace HRC.Navigation
{
    public interface IGeoArray : IGeoPath
    {
        Geo this[int index] { get; }
        int Length { get; }
        double Area { get; }
        Geo Center { get; }
        bool IsClosed { get; }
        double[] ToLatLonArray(bool isDegrees);

        // If the current array is closed, returns it, otherwise returns a new array that is closed
        IGeoArray Closed { get; }
        IGeoArray WithoutDuplicates { get; }
    }

    public interface IMutableGeoArray : IGeoArray
    {
        new Geo this[int index] { set; }
        void Close();
        void RemoveDuplicates();
    }

    public class GeoArray : IGeoArray, IEnumerable<Geo>
    {
        protected Geo[] MyGeoArray;

        #region Constructors
        protected GeoArray() {}

        public GeoArray(params Geo[] geos) : this(geos.ToList()) { }
        public GeoArray(IEnumerable<Geo> geos)
        {
            MyGeoArray = geos.Select(geo => new Geo(geo)).ToArray();
            Initialize();
        }

        public GeoArray(IGeoArray array)
        {
            MyGeoArray = new Geo[array.Length];
            for (var i = 0; i < array.Length; i++)
                MyGeoArray[i] = new Geo(array[i]);
            Initialize();
        }

        public GeoArray(IList<double> latLonArray, bool isDegrees)
        {
            if (latLonArray.Count % 2 != 0) throw new ArgumentException("Cannot construct a GeoArray from a source array with an odd number of entries", "latLonArray");
            MyGeoArray = new Geo[latLonArray.Count/2];
            for (var i = 0; i < MyGeoArray.Length; i++)
                MyGeoArray[i] = new Geo(latLonArray[i * 2], latLonArray[(i * 2) + 1], isDegrees);
            Initialize();
        }
        #endregion
        #region Initialize() and properties set by it
        void Initialize()
        {
            BoundingBox = new GeoRect(MyGeoArray);
            var start = Geo.Empty;
            Center = Geos.Aggregate(start, (current, geo) => current + geo).Normalized;
            IsClosed = MyGeoArray.First().Equals(MyGeoArray.Last());
            BoundingCircle = new BoundingCircle(this);
        }

        public GeoRect BoundingBox { get; private set; }
        public Geo Center { get; private set; }
        public bool IsClosed { get; private set; }
        public BoundingCircle BoundingCircle { get; private set; }
        #endregion
        #region Read-only properties computed on the fly
        public Geo this[int index] { get { return MyGeoArray[index]; } }
        public int Length { get { return MyGeoArray.Length; } }

        /// <summary>
        /// Returns a closed version of the current GeoArray.  
        /// If the current GeoArray is already closed, it is returned.
        /// If the current GeoArray is NOT closed, a copy is made and returned.
        /// </summary>
        public IGeoArray Closed
        {
            get
            {
                if (IsClosed) return this;
                var newGeos = MyGeoArray.Select(geo => new Geo(geo)).ToList();
                newGeos.Add(new Geo(MyGeoArray.First()));
                return new GeoArray(newGeos);
            }
        }

        /// <summary>
        /// Returns a copy of the current GeoArray with duplicates (if any) removed.
        /// Note that this property ALWAYS copies the current GeoArray.
        /// </summary>
        // ReSharper disable AccessToModifiedClosure
        public IGeoArray WithoutDuplicates
        {
            get
            {
                var duplicatesRemoved = new Geo[MyGeoArray.Length];
                var p = 0;
                foreach (var geo in MyGeoArray.Where(geo => p == 0 || (!duplicatesRemoved[p - 1].Equals(geo))))
                    duplicatesRemoved[p++] = geo;
                return new GeoArray(duplicatesRemoved);
            }
        }
        // ReSharper restore AccessToModifiedClosure
        /// <summary>
        /// Computes the area of a polygon on the surface of a unit sphere. For a
        /// non unit sphere, multiply this by the radius of sphere squared. This 
        /// method will treat both open and closed polygons as closed and return
        /// the area enclosed by the points
        /// For more information see: http://math.rice.edu/~pcmi/sphere/
        /// </summary>
        public double Area
        {
            get
            {
                if (MyGeoArray.Length < 3) return 0;

                var count = 0;
                double area = 0;
                var v0 = new Geo(MyGeoArray[0]);
                var v1 = new Geo(MyGeoArray[1]);
                var p0 = new Geo(MyGeoArray[0]);
                var p1 = new Geo(MyGeoArray[1]);
                Geo p2;
                // Having the first and last points the same messes up the
                // algorithm, so skip the last point if it equals the first.
                var size = MyGeoArray.Length - (IsClosed ? 1 : 0);
                for (var i = 2; i < size; i++)
                {
                    count++;
                    p2 = new Geo(MyGeoArray[i]);
                    area += p0.Angle(p1, p2);
                    p0 = new Geo(p1);
                    p1 = new Geo(p2);
                }

                count++;
                p2 = new Geo(v0);
                area += p0.Angle(p1, p2);
                p0 = new Geo(p1);
                p1 = new Geo(p2);

                count++;
                p2 = new Geo(v1);
                area += p0.Angle(p1, p2);

                return Math.Abs(area - ((count - 2)*Math.PI));
            }
        }
        #endregion
        #region Helper functions
        public double[] ToLatLonArray(bool isDegrees)
        {
            var result = new double[MyGeoArray.Length*2];
            for (var i = 0; i < MyGeoArray.Length; i++)
            {
                if (isDegrees)
                {
                    result[i*2] = MyGeoArray[i].Latitude;
                    result[(i*2) + 1] = MyGeoArray[i].Longitude;
                }
                else
                {
                    result[i * 2] = MyGeoArray[i].LatitudeRadians;
                    result[(i * 2) + 1] = MyGeoArray[i].LongitudeRadians;
                }
            }
            return result;
        }

        public bool IsNearSegment(GeoSegment segment, double epsilon)
        {
            return segment.IsNear(this, epsilon);
        }
        #endregion
        #region Enumerators
        public IEnumerable<Geo> Geos { get { return MyGeoArray.Select(geo => new Geo(geo)); } }

        public IEnumerable<GeoSegment> Segments
        {
            get
            {
                for (var i = 0; i < MyGeoArray.Length - 1; i++) yield return new GeoSegment(MyGeoArray[i], MyGeoArray[i + 1]);
                if (IsClosed) yield return new GeoSegment(MyGeoArray.Last(), MyGeoArray.First());
            }
        }
        #endregion
        #region Explicit cast operators
        public static explicit operator Geo[](GeoArray geoArray)
        {
            var result = new Geo[geoArray.Length];
            for (var i = 0; i < geoArray.Length; i++)
                result[i] = new Geo(geoArray[i]);
            return result;
        }

        public static explicit operator GeoArray(Geo[] geos) { return new GeoArray(geos); }
        #endregion
        #region IEnumerable<Geo> support
        public IEnumerator<Geo> GetEnumerator()
        {
            return MyGeoArray.Select(geo => new Geo(geo)).GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
        #endregion
#if KML_Support
        #region KML support
        public Placemark Placemark
        {
            get
            {
                if (_placemark != null) return _placemark;
                _placemark = new Placemark { LineString = new LineString { coordinates = new Coordinates()} };
                foreach (var geo in Geos) _placemark.LineString.coordinates.Add(new Point3D(geo.Longitude, geo.Latitude, 0));
                return _placemark;
            }
        }
        Placemark _placemark;
        #endregion
#endif
    }

    public class MutableGeoArray: GeoArray, IMutableGeoArray
    {
        public new Geo this[int index]
        {
            get { return base[index]; }
            set { MyGeoArray[index] = value; }
        }

        public void Close()
        {
            if (IsClosed) return;
            MyGeoArray = (Geo[])(GeoArray)Closed;
        }

        public void RemoveDuplicates()
        {
            MyGeoArray = (Geo[])(GeoArray)WithoutDuplicates;
        }

    }
}