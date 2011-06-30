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
            const int sectionCount = 100;
            var uniqueList = new List<T>();
            var originalCount = this.Count;
            var duplicateCount = 0;
#if false
            var startLon = Longitudes.First();
            var endLon = Longitudes.Last();
            var lonSpan = endLon - startLon;
            var lonStep = lonSpan / sectionCount;
            var subLists = new List<List<T>>();
            for (var curSection = 0; curSection < sectionCount; curSection++)
            {
                if (backgroundTask != null) backgroundTask.RunState = "creating section " + curSection;
                var sectionStart = startLon + (curSection * lonStep);
                var sectionEnd = startLon + ((curSection + 1) * lonStep);
                var subList = new List<T>();
                var section = from entry in this
                              where ((sectionStart <= entry.Longitude) && (entry.Longitude < sectionEnd))
                              select entry;
                if (backgroundTask != null) backgroundTask.RunState = "deduping section " + curSection;
                foreach (var entry in section)
                {
                    var checkForDuplicates = true;
                    var duplicate = false;
                    if ((entry is SedimentSample) && ((entry as SedimentSample).Data.IsHighResolution)) checkForDuplicates = false;
                    if (checkForDuplicates)
                    {
                        foreach (var subItem in subList)
                            if (entry.Equals(subItem))
                            {
                                duplicate = true;
                                duplicateCount++;
                                if (backgroundTask != null) backgroundTask.Status = string.Format("Found {0} duplicates in {1} raw data points", duplicateCount, originalCount);
                                break;
                            }
                    }
                    if (!duplicate) subList.Add(entry);
                }
                subLists.Add(subList);
                uniqueList.AddRange(subList);
            }
#else
            foreach (var curEntry in from curEntry in this
                                     let foundMatch = uniqueList.Any(curEntry.Equals)
                                     where !foundMatch
                                     select curEntry) {uniqueList.Add(curEntry);}
#endif
            Clear();
            AddRange(uniqueList);
        }

        private static List<double> DistinctSortedLatLonList(IEnumerable<double> rawEnumerable)
        {
            var rawList = rawEnumerable.Distinct().ToList();
            rawList.Sort();
            var result = new List<double>();
            for (var index = 0; index < rawList.Count - 1; index++)
            {
                result.Add(Math.Round(rawList[index], 4));
                int duplicateCount;
                for (duplicateCount = 0; duplicateCount < (rawList.Count - 1 - index); duplicateCount++)
                    if (Math.Round(rawList[index + duplicateCount], 4) != Math.Round(rawList[index + duplicateCount + 1], 4)) break;
                index += duplicateCount;
            }
            return result;
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
            get { return _longitudes ?? (_longitudes = DistinctSortedLatLonList(this.Select(point => point.Longitude))); }
        }

        [XmlIgnore]
        List<double> _longitudes;

        [XmlIgnore]
        public List<double> Latitudes
        {
            get
            {
                if (_latitudes == null)
                {
                    _latitudes = DistinctSortedLatLonList(this.Select(point => point.Latitude));
                    //_latitudes = this.Select(point => point.Latitude).Distinct().ToList();
                    //_latitudes.Sort();
                }
                return _latitudes;
            }
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
