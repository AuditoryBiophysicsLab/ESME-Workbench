﻿using System;
using System.Collections.Generic;
using System.Linq;

namespace HRC.Navigation
{
    public abstract class GeoReferencedField
    {
        protected GeoReferencedField()
        {
            Latitudes = new List<double>();
            Longitudes = new List<double>();
        }

        /// <summary>
        ///   List of distinct latitudes (in degrees) represented in the field
        /// </summary>
        public List<double> Latitudes { get; protected set; }

        /// <summary>
        ///   List of distinct longitudes (in degrees) represented in the field
        /// </summary>
        public List<double> Longitudes { get; protected set; }

        /// <summary>
        /// The GeoRect that contains the field data
        /// </summary>
        public virtual GeoRect GeoRect {get{return new GeoRect(Latitudes.Last(), Latitudes.First(), Longitudes.Last(), Longitudes.First());}}
        
        /// <summary>
        /// Indicates whether the bounds of the field contains the specified EarthCoordinate.
        /// </summary>
        /// <param name="coordinate"></param>
        /// <returns></returns>
        public bool Contains(EarthCoordinate coordinate) { return GeoRect.Contains(coordinate); }
    }

    public class GenericGeoField<TEarthCoordinate> : GeoReferencedField
        where TEarthCoordinate : EarthCoordinate
    {
        protected GenericGeoField(IEnumerable<TEarthCoordinate> data)
        {
            var tmpLat = new List<double>();
            var tmpLon = new List<double>();
            foreach (var datum in data)
            {
                tmpLat.Add(datum.Latitude);
                tmpLon.Add(datum.Longitude);
            }
            Latitudes = tmpLat.Distinct().ToList();
            Longitudes = tmpLon.Distinct().ToList();
            Latitudes.Sort();
            Longitudes.Sort();
            FieldData = new TEarthCoordinate[Longitudes.Count, Latitudes.Count];
            var progress = 0;
            foreach (var datum in data)
            {
                var lonIndex = Longitudes.IndexOf(datum.Longitude);
                var latIndex = Latitudes.IndexOf(datum.Latitude);
                FieldData[lonIndex, latIndex] = datum;
                progress++;
            }
        }

        protected GenericGeoField(TEarthCoordinate[,] data)
        {
            Latitudes = new List<double>();
            Longitudes = new List<double>();
            for (var lon = 0; lon < data.GetLength(0); lon++)
                Longitudes.Add(data[lon, 0].Longitude);
            for (var lat = 0; lat < data.GetLength(1); lat++)
                Latitudes.Add(data[0, lat].Latitude);
            FieldData = data;
        }

        /// <summary>
        /// Array of EarthCoordinate-derived field values
        /// </summary>
        public TEarthCoordinate[,] FieldData { get; protected set; }

        /// <summary>
        /// Gets the longitudinal resolution of the current dataset, in degrees
        /// </summary>
        public double LongitudinalResolution { get { return Math.Abs(Longitudes[1] - Longitudes[0]); } }

        /// <summary>
        /// Gets the latitudinal resolution of the current dataset, in degrees
        /// </summary>
        public double LatitudinalResolution { get { return Math.Abs(Latitudes[1] - Latitudes[0]); } }

        /// <summary>
        /// Gets the nearest actual data point to the requested location, even if the location is outside the bounds of the data set.
        /// </summary>
        /// <param name="location">The EarthCoordinate (or EarthCoordinate-derived object) that provides the location of the data point being sought</param>
        /// <returns></returns>
        public TEarthCoordinate this[EarthCoordinate location]
        {
            get
            {
                int latStartIndex;
                var latEndIndex = latStartIndex = Latitudes.IndexOf(location.Latitude);
                var latitude = location.Latitude;
                var longitude = location.Longitude;

                if (latStartIndex == -1)
                {
                    var southLats = Latitudes.FindAll(y => y <= latitude);
                    if (southLats.Count() > 0) latStartIndex = Latitudes.IndexOf(southLats.Last());
                }
                if (latEndIndex == -1)
                {
                    var northLats = Latitudes.FindAll(y => y >= latitude);
                    if (northLats.Count() > 0) latEndIndex = Latitudes.IndexOf(northLats.First());
                }
                if (latStartIndex == -1) latStartIndex = latEndIndex;
                if (latEndIndex == -1) latEndIndex = latStartIndex;

                int lonStartIndex;
                var lonEndIndex = lonStartIndex = Longitudes.IndexOf(longitude);
                if (lonStartIndex == -1)
                {
                    var westLons = Longitudes.FindAll(x => x <= location.Longitude);
                    if (westLons.Count() > 0) lonStartIndex = Longitudes.IndexOf(westLons.Last());
                }
                if (lonEndIndex == -1)
                {
                    var eastLons = Longitudes.FindAll(x => x >= longitude);
                    if (eastLons.Count() > 0) lonEndIndex = Longitudes.IndexOf(eastLons.First());
                }
                if (lonStartIndex == -1) lonStartIndex = lonEndIndex;
                if (lonEndIndex == -1) lonEndIndex = lonStartIndex;

                var searchList = new List<TEarthCoordinate>();
                for (var latIndex = latStartIndex; latIndex <= latEndIndex; latIndex++)
                    for (var lonIndex = lonStartIndex; lonIndex <= lonEndIndex; lonIndex++)
                        if (FieldData[lonIndex, latIndex] != null) searchList.Add(FieldData[lonIndex, latIndex]);
                var closestSample = searchList.First();
                var closestDistance = location.DistanceTo(closestSample);
                foreach (var curSample in searchList)
                {
                    var curDistance = location.DistanceTo(curSample);
                    if (curDistance >= closestDistance) continue;
                    closestDistance = curDistance;
                    closestSample = curSample;
                }
                return closestSample;
            }
        }
    }

    public class GeoScalarField<TEarthCoordinate, TDataType> : GenericGeoField<TEarthCoordinate>
        where TEarthCoordinate : EarthCoordinate<TDataType>
        where TDataType : IComparable<TDataType>
    {
        public GeoScalarField(IEnumerable<TEarthCoordinate> data) : base(data) { }
        public GeoScalarField(TEarthCoordinate[,] data) : base((IEnumerable<TEarthCoordinate>)data.GetEnumerator()) { }

        public TEarthCoordinate Minimum
        {
            get
            {
                TEarthCoordinate curMinPoint = null;
                foreach (var datum in FieldData)
                {
                    if (curMinPoint == null) curMinPoint = datum;
                    if ((curMinPoint != datum) && (curMinPoint.Data.CompareTo(datum.Data) < 0)) curMinPoint = datum;
                }
                return curMinPoint;
            }
        }

        public TEarthCoordinate Maximum
        {
            get
            {
                TEarthCoordinate curMaxPoint = null;
                foreach (var datum in FieldData)
                {
                    if (curMaxPoint == null) curMaxPoint = datum;
                    if ((curMaxPoint != datum) && (curMaxPoint.Data.CompareTo(datum.Data) > 0)) curMaxPoint = datum;
                }
                return curMaxPoint;
            }
        }
    }
}