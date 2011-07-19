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
    public class EnvironmentData<T> : SortedList<LatLonKey, T> where T : EarthCoordinate
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
                    var curDistance = item.Value.DistanceKilometers(location);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    closestSample = item.Value;
                }
                return closestSample;
            }
        }

        public T this[uint lonIndex, uint latIndex]
        {
            get
            {
                if ((lonIndex >= Longitudes.Count) || (latIndex >= Latitudes.Count)) throw new IndexOutOfRangeException(string.Format("EnvironmentData: Attempted to access [{0}, {1}] when max indices are [{2}, {3}]", lonIndex, latIndex, Longitudes.Count, Latitudes.Count));
                var key = new LatLonKey(Latitudes[(int)latIndex], Longitudes[(int)lonIndex]);
                return this[key];
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
            var matchingPoints = this.Where(point => trimRect.Contains(point.Value)).ToList();
            var pointsToKeep = from point in matchingPoints
                               select point.Value;
            Clear();
            AddRange(pointsToKeep);
        }

        #region List<T> overrides
        public void Add(T item)
        {
            try
            {
                var key = new LatLonKey(item);
                if (FindKey(key) == null)
                {
                    Add(key, item);
                    _latitudes = _longitudes = null;
                }
                //if (Keys.Contains(key)) return;
            }
            catch { }
        }

        LatLonKey FindKey(LatLonKey key)
        {
            if (Keys.Count == 0) return null;
            var min = 0;
            var max = Keys.Count - 1;
            while (min < max)
            {
                var mid = (max + min) / 2;
                var midKey = Keys[mid];
                var comp = LatLonKey.Compare(midKey, key);
                if (comp < 0) min = mid + 1;
                else if (comp > 0) max = mid - 1;
                else return midKey;
            }
            if (min == max && LatLonKey.Compare(Keys[min], key) == 0)
                return Keys[min];
            return null;
        }

        public void AddRange(IEnumerable<T> collection)
        {
            foreach (var item in collection) Add(item);
            _latitudes = _longitudes = null;
        }

        public new void Clear()
        {
            base.Clear();
            _latitudes = _longitudes = null;
        }

        public bool Remove(T item)
        {
            var result = Remove(new LatLonKey(item));
            if (result) _latitudes = _longitudes = null;
            return result;
        }

        public new void RemoveAt(int index)
        {
            base.RemoveAt(index);
            _latitudes = _longitudes = null;
        }

        #endregion

        public void RemoveDuplicates(BackgroundTask backgroundTask = null)
        {
            var uniqueList = new List<T>();
            foreach (var curEntry in from curEntry in this
                                     let foundMatch = uniqueList.Any(curEntry.Equals)
                                     where !foundMatch
                                     select curEntry) {uniqueList.Add(curEntry.Value);}
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
            get { return _longitudes ?? (_longitudes = SortedList(this.Select(point => Math.Round(point.Value.Longitude, 4)).Distinct())); }
        }

        [XmlIgnore]
        List<double> _longitudes;

        [XmlIgnore]
        public List<double> Latitudes
        {
            get { return _latitudes ?? (_latitudes = SortedList(this.Select(point => Math.Round(point.Value.Latitude, 4)).Distinct())); }
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

    public class LatLonKey : IComparer<LatLonKey>, IComparable<LatLonKey>
    {
        public LatLonKey(Geo geo)
        {
            Latitude = Math.Round(geo.Latitude, 4);
            Longitude = Math.Round(geo.Longitude, 4);
        }

        public LatLonKey(double latitude, double longitude)
        {
            Latitude = Math.Round(latitude, 4);
            Longitude = Math.Round(longitude, 4);
        }

        double Latitude { get; set; }
        double Longitude { get; set; }

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
        public static int Compare(LatLonKey e1, LatLonKey e2)
        {
            if (e1.Latitude < e2.Latitude) return -1;
            if (e1.Latitude > e2.Latitude) return 1;
            if (e1.Longitude < e2.Longitude) return -1;
            return e1.Longitude > e2.Longitude ? 1 : 0;
        }

        /// <summary>
        /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>, as shown in the following table.Value Meaning Less than zero<paramref name="x"/> is less than <paramref name="y"/>.Zero<paramref name="x"/> equals <paramref name="y"/>.Greater than zero<paramref name="x"/> is greater than <paramref name="y"/>.
        /// </returns>
        /// <param name="x">The first object to compare.</param><param name="y">The second object to compare.</param>
        int IComparer<LatLonKey>.Compare(LatLonKey x, LatLonKey y) { return Compare(x, y); }

        /// <summary>
        /// Compares the current object with another object of the same type.
        /// </summary>
        /// <returns>
        /// A value that indicates the relative order of the objects being compared. The return value has the following meanings: Value Meaning Less than zero This object is less than the <paramref name="other"/> parameter.Zero This object is equal to <paramref name="other"/>. Greater than zero This object is greater than <paramref name="other"/>. 
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public int CompareTo(LatLonKey other) { return Compare(this, other); }
        public override string ToString() { return string.Format("{0:0.#####},{1:0.#####}", Latitude, Longitude); }
        public override int GetHashCode() { return ToString().GetHashCode(); }
    }
    
    public class TimePeriodEnvironmentData<T> where T : EarthCoordinate
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<EarthCoordinate<T>>.ReferencedTypes);
        public TimePeriodEnvironmentData() { EnvironmentData = new EnvironmentData<T>(); }

        public NAVOTimePeriod TimePeriod { get; set; }
        public EnvironmentData<T> EnvironmentData { get; set; }
    }
}
