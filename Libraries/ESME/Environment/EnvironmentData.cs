using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using HRC.LinqStatistics;
using HRC.Navigation;
using System.Windows;

namespace ESME.Environment
{
    public class EnvironmentData<T> : IList<T> where T : EarthCoordinate
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
                typeof (EarthCoordinate),
                typeof (Geo),
                typeof (Point),
                typeof (T),
        };

        readonly SortedList<LatLonKey, T> _sortedList = new SortedList<LatLonKey, T>();

        public T this[int index] { get { return _sortedList.Values[index]; } }

        public T this[EarthCoordinate location]
        {
            get
            {
                var minDistance = double.MaxValue;
                T closestSample = null;
                foreach (var item in _sortedList)
                {
                    var curDistance = item.Value.DistanceKilometers(location);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    closestSample = item.Value;
                }
                return closestSample;
            }
        }

        #region T[,] TwoDIndex { get; set; }

        T[,] TwoDIndex
        {
            get 
            {
                if (_twoDIndex != null) return _twoDIndex;
                _twoDIndex = new T[Longitudes.Count,Latitudes.Count];
                for (var lonIndex = 0; lonIndex < Longitudes.Count; lonIndex++)
                {
                    var lon = Longitudes[lonIndex];
                    for (var latIndex = 0; latIndex < Latitudes.Count; latIndex++)
                    {
                        var lat = Latitudes[latIndex];
                        var key = new LatLonKey(lat, lon);
                        _twoDIndex[lonIndex, latIndex] = _sortedList[key];
                    }
                }
                return _twoDIndex;
            }
        }

        T[,] _twoDIndex;

        #endregion


        public T this[uint lonIndex, uint latIndex]
        {
            get
            {
                return TwoDIndex[lonIndex, latIndex];
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
            var resultList = new List<T>();
            for (var heightIndex = 0; heightIndex < outputHeight; heightIndex++)
            {
                var sourceHeightIndex = (uint)((heightIndex * heightStep) + (heightStep / 2));
                for (var widthIndex = 0; widthIndex < outputWidth; widthIndex++)
                {
                    var sourceWidthIndex = (uint)((widthIndex * widthStep) + (widthStep / 2));
                    resultList.Add(source.TwoDIndex[sourceWidthIndex, sourceHeightIndex]);
                }
            }
            result.AddRange(resultList);
            return result;
        }

        public void TrimToNearestPoints(GeoRect geoRect)
        {
            var southWest = this[geoRect.SouthWest];
            var northEast = this[geoRect.NorthEast];
            var trimRect = GeoRect.InflateWithGeo(new GeoRect(northEast.Latitude, southWest.Latitude, northEast.Longitude, southWest.Longitude), 0.01);
            var matchingPoints = _sortedList.Where(point => trimRect.Contains(point.Value)).ToList();
            var pointsToKeep = from point in matchingPoints
                               select point.Value;
            Clear();
            AddRange(pointsToKeep);
        }

        #region List<T> overrides
        public void Add(T item) { throw new NotImplementedException(); }

        void ClearHelperIndices()
        {
            _latitudes = _longitudes = null;
            _twoDIndex = null;
        }

        public void AddRange(IEnumerable<T> collection)
        {
            _sortedList.Capacity += collection.Count();
            foreach (var item in collection)
            {
                try
                {
                    var key = new LatLonKey(item);
                    _sortedList.Add(key, item);
                }
                catch { }
            }
            ClearHelperIndices();
        }

        public void Clear()
        {
            _sortedList.Clear();
            ClearHelperIndices();
        }

        public bool Contains(T item) { return _sortedList.ContainsKey(new LatLonKey(item)); }

        public void CopyTo(T[] array, int arrayIndex) { _sortedList.Values.CopyTo(array, arrayIndex); }

        public bool Remove(T item)
        {
            var result = _sortedList.Remove(new LatLonKey(item));
            if (result) ClearHelperIndices();
            return result;
        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { return _sortedList.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return _sortedList.Values.IsReadOnly; }
        }

        /// <summary>
        /// Determines the index of a specific item in the <see cref="T:System.Collections.Generic.IList`1"/>.
        /// </summary>
        /// <returns>
        /// The index of <paramref name="item"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.IList`1"/>.</param>
        public int IndexOf(T item) { return _sortedList.IndexOfKey(new LatLonKey(item)); }

        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.Generic.IList`1"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="item"/> should be inserted.</param><param name="item">The object to insert into the <see cref="T:System.Collections.Generic.IList`1"/>.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public void Insert(int index, T item) { throw new NotImplementedException(); }

        public void RemoveAt(int index)
        {
            _sortedList.RemoveAt(index);
            ClearHelperIndices();
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <returns>
        /// The element at the specified index.
        /// </returns>
        /// <param name="index">The zero-based index of the element to get or set.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        T IList<T>.this[int index]
        {
            get { return _sortedList.Values[index]; }
            set { throw new NotImplementedException(); }
        }
        #endregion

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
            get { return _longitudes ?? (_longitudes = SortedList(_sortedList.Select(point => Math.Round(point.Value.Longitude, 4)).Distinct())); }
        }

        [XmlIgnore]
        List<double> _longitudes;

        [XmlIgnore]
        public List<double> Latitudes
        {
            get { return _latitudes ?? (_latitudes = SortedList(_sortedList.Select(point => Math.Round(point.Value.Latitude, 4)).Distinct())); }
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

        #region Implementation of IEnumerable
        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<T> GetEnumerator() { return _sortedList.Values.GetEnumerator(); }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        #endregion
    }

    internal class LatLonKey : IComparer<LatLonKey>, IComparable<LatLonKey>
    {
        public LatLonKey(Geo geo)
        {
            var lat = (int)Math.Round(geo.Latitude * 10000);
            var lon = (int)Math.Round(geo.Longitude * 10000);
            _key = (lat * 100000000) + lon;
        }

        public LatLonKey(double latitude, double longitude)
        {
            var lat = (int)Math.Round(latitude * 10000);
            var lon = (int)Math.Round(longitude * 10000);
            _key = (lat * 100000000) + lon;
        }

        readonly long _key;

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
            if (e1._key < e2._key) return -1;
            return e1._key > e2._key ? 1 : 0;
        }

        /// <summary>
        /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// A signed integer that indicates the relative values of <paramref name="x"/> and <paramref name="y"/>, as shown in the following table.Value Meaning Less than zero<paramref name="x"/> is less than <paramref name="y"/>.Zero<paramref name="x"/> equals <paramref name="y"/>.Greater than zero<paramref name="x"/> is greater than <paramref name="y"/>.
        /// </returns>
        /// <param name="x">The first object to compare.</param><param name="y">The second object to compare.</param>
        int IComparer<LatLonKey>.Compare(LatLonKey x, LatLonKey y)
        {
            if (x._key < y._key) return -1;
            return x._key > y._key ? 1 : 0;
        }

        /// <summary>
        /// Compares the current object with another object of the same type.
        /// </summary>
        /// <returns>
        /// A value that indicates the relative order of the objects being compared. The return value has the following meanings: Value Meaning Less than zero This object is less than the <paramref name="other"/> parameter.Zero This object is equal to <paramref name="other"/>. Greater than zero This object is greater than <paramref name="other"/>. 
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public int CompareTo(LatLonKey other)
        {
            if (_key < other._key) return -1;
            return _key > other._key ? 1 : 0;
        }

        public override string ToString() { return _key.ToString(); }
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
