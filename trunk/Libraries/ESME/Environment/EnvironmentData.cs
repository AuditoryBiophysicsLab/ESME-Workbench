using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using HRC.LinqStatistics;
using HRC.Navigation;
using System.Windows;
using HRC.Utility;

namespace ESME.Environment
{
    public class EnvironmentData<T> : List<T> where T : EarthCoordinate
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
                typeof (EarthCoordinate),
                typeof (Geo),
                typeof (Point),
                typeof (T),
        };

        public T this[EarthCoordinate location]
        {
            get
            {
                var minDistance = double.MaxValue;
                T closestSample = null;
                foreach (var item in this)
                {
                    var curDistance = item.DistanceKilometers(location);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    closestSample = item;
                }
                return closestSample;
            }
        }

        public T this[uint lonIndex, uint latIndex]
        {
            get
            {
                if ((lonIndex < Longitudes.Count) || (latIndex < Latitudes.Count)) throw new IndexOutOfRangeException(string.Format("EnvironmentData: Attempted to access [{0}, {1}] when max indices are [{2}, {3}]", lonIndex, latIndex, Longitudes.Count, Latitudes.Count));
                var desiredLon = Longitudes[(int)lonIndex];
                var desiredLat = Latitudes[(int)latIndex];
                return this.Where(item => (Math.Round(item.Longitude, 4) == desiredLon) && (Math.Round(item.Latitude, 4) == desiredLat)).FirstOrDefault();
            }
        }

        public static EnvironmentData<T> Decimate(EnvironmentData<T> source, int outputWidth, int outputHeight)
        {
            if (outputWidth > source.Longitudes.Count || outputHeight > source.Latitudes.Count) throw new DataMisalignedException("Cannot decimate to a larger area.");
            var result = new EnvironmentData<T>();
            var sourceWidth = source.Longitudes.Count;
            var sourceHeight = source.Latitudes.Count;
            var widthStep = (double)sourceWidth / outputWidth;
            var heightStep = (double)sourceHeight / outputHeight;

            for (var widthIndex = 0; widthIndex < outputWidth; widthIndex++)
            {
                var sourceWidthIndex = (uint)((widthIndex * widthStep) + (widthStep / 2));
                for (var heightIndex = 0; heightIndex < outputHeight; heightIndex++)
                {
                    var sourceHeightIndex = (uint)((heightIndex * heightStep) + (heightStep / 2));
                    result.Add(source[sourceWidthIndex, sourceHeightIndex]);
                }
            }

            return result;
        }


        public void TrimToNearestPoints(GeoRect geoRect)
        {
            var southWest = this[geoRect.SouthWest];
            var northEast = this[geoRect.NorthEast];
            var trimRect = GeoRect.InflateWithGeo(new GeoRect(northEast.Latitude, southWest.Latitude, northEast.Longitude, southWest.Longitude), 0.01);
            var pointsToKeep = this.Where(trimRect.Contains).ToList();
            Clear();
            AddRange(pointsToKeep);
        }

        #region List<T> overrides
        public new void Add(T item)
        {
            base.Add(item);
            _latitudes = _longitudes = null;
        }

        public new void AddRange(IEnumerable<T> collection)
        {
            base.AddRange(collection);
            _latitudes = _longitudes = null;
        }

        public new void Clear()
        {
            base.Clear();
            _latitudes = _longitudes = null;
        }

        public new bool Remove(T item)
        {
            var result = base.Remove(item);
            if (result) _latitudes = _longitudes = null;
            return result;
        }

        public new int RemoveAll(Predicate<T> match)
        {
            var result = base.RemoveAll(match);
            if (result > 0) _latitudes = _longitudes = null;
            return result;
        }

        public new void RemoveAt(int index)
        {
            base.RemoveAt(index);
            _latitudes = _longitudes = null;
        }

        public new void RemoveRange(int index, int count)
        {
            base.RemoveRange(index, count);
            _latitudes = _longitudes = null;
        }
        #endregion

        public void RemoveDuplicates(BackgroundTask backgroundTask = null)
        {
            var uniqueList = new List<T>();
            foreach (var curEntry in from curEntry in this
                                     let foundMatch = uniqueList.Any(curEntry.Equals)
                                     where !foundMatch
                                     select curEntry) {uniqueList.Add(curEntry);}
            Clear();
            AddRange(uniqueList);
        }

        private static List<double> SortedList(IEnumerable<double> rawEnumerable)
        {
            var rawList = rawEnumerable.ToList();
            rawList.Sort();
            return rawList;
            //var result = new List<double>();
            //for (var index = 0; index < rawList.Count - 1; index++)
            //{
            //    result.Add(Math.Round(rawList[index], 4));
            //    int duplicateCount;
            //    for (duplicateCount = 0; duplicateCount < (rawList.Count - 1 - index); duplicateCount++)
            //        if (Math.Round(rawList[index + duplicateCount], 4) != Math.Round(rawList[index + duplicateCount + 1], 4)) break;
            //    index += duplicateCount;
            //}
            //return result;
        }

        static double CalculateResolution(IList<double> axis)
        {
            var differences = new List<double>();
            for (var index = 0; index < axis.Count - 1; index++)
                differences.Add(axis[index + 1] - axis[index]);
            return differences.StandardDeviation() < 0.001 ? Math.Round(differences[0] * 60, 2) : double.NaN;
        }

        public double Resolution
        {
            get
            {
                var lonRes = CalculateResolution(Longitudes);
                var latRes = CalculateResolution(Latitudes);
                return lonRes == latRes ? lonRes : double.NaN;
            }
        }

        [XmlIgnore]
        public List<double> Longitudes
        {
            get { return _longitudes ?? (_longitudes = SortedList(this.Select(point => Math.Round(point.Longitude, 4)).Distinct())); }
        }

        [XmlIgnore]
        List<double> _longitudes;

        [XmlIgnore]
        public List<double> Latitudes
        {
            get { return _latitudes ?? (_latitudes = SortedList(this.Select(point => Math.Round(point.Latitude, 4)).Distinct())); }
        }

        [XmlIgnore]
        List<double> _latitudes;
        
        /// <summary>
        /// The GeoRect that contains the field data
        /// </summary>
        public virtual GeoRect GeoRect
        {
            get
            {
                var latitudes = Latitudes;
                var longitudes = Longitudes;
                return new GeoRect(latitudes.Last(), latitudes.First(), longitudes.Last(), longitudes.First());
            }
        }

    }
    
    public class TimePeriodEnvironmentData<T> where T : EarthCoordinate
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<EarthCoordinate<T>>.ReferencedTypes);
        public TimePeriodEnvironmentData() { EnvironmentData = new EnvironmentData<T>(); }

        public NAVOTimePeriod TimePeriod { get; set; }
        public EnvironmentData<T> EnvironmentData { get; set; }
    }
}
