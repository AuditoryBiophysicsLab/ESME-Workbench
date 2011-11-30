using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using C5;
using ESME.Environment.NAVO;
using HRC.Collections;
using HRC.LinqStatistics;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class EnvironmentData<T> : System.Collections.Generic.IList<T> where T : EarthCoordinate, new()
    {
        public EnvironmentData()
        {
            _indexLockObject = new object();
            _hashLockObject = new object();
        }

        public static EnvironmentData<T> Deserialize(BinaryReader reader, Func<BinaryReader, T> readFunc)
        {
            var result = new EnvironmentData<T>();
            var itemCount = reader.ReadInt32();
            for (var i = 0; i < itemCount; i++) result._arrayList.Add(readFunc(reader));
            result.ClearHelperIndices();
            return result;
        }

        public virtual void Serialize(BinaryWriter writer, Action<BinaryWriter, T> writeAction)
        {
            writer.Write(Count);
            foreach (var item in _arrayList)
                writeAction(writer, item);
        }

        readonly List<T> _arrayList = new List<T>();

        public T this[int index] { get { return _arrayList[index]; } }

        public T this[Geo location]
        {
            get
            {
                if (_arrayList.Count == 0) throw new IndexOutOfRangeException(string.Format("The data point at {0} was not found because the list is empty", location));
                if (_arrayList.Count < 10000) return FindNearestInSublist(location, _arrayList, 0, _arrayList.Count);
                var cpuCount = System.Environment.ProcessorCount;
                var arraySliceLength = _arrayList.Count / cpuCount;
                var nearestCandidates = new List<T>();
                Parallel.For(0, cpuCount, () => new T(), (i, loop, j) =>
                {
                    if (i < cpuCount)
                        return FindNearestInSublist(location, _arrayList, arraySliceLength * i, arraySliceLength);
                    return FindNearestInSublist(location, _arrayList, arraySliceLength * i, _arrayList.Count - (arraySliceLength * i));
                },
                returnValue =>
                {
                    lock (nearestCandidates)
                    {
                        nearestCandidates.Add(returnValue);
                    }
                });
                return FindNearestInSublist(location, nearestCandidates, 0, nearestCandidates.Count);
            }
        }

        public T Nearest(Geo location)
        {
            var targetLat = location.Latitude;
            var targetLon = location.Longitude;
            var maxIndexRange = Math.Max(Latitudes.Count, Longitudes.Count);
            for (var window = 0; window < maxIndexRange; window++)
            {
                var latRange = GetSearchRange(_latitudes, targetLat, window);
                var lonRange = GetSearchRange(_longitudes, targetLon, window);
                var candidatePoints = latRange.SelectMany(lat => lonRange, (lat, lon) => this[lon, lat]).Where(curPoint => curPoint != null).ToList();
                if (candidatePoints.Count > 0)
                    return FindNearestInSublist(location, candidatePoints, 0, candidatePoints.Count);
            }
            return null;
        }

        static IEnumerable<double> GetSearchRange(C5.IList<double> axis, double target, int indexRange = 0)
        {
            int startIndex, endIndex;
            for (startIndex = 0; startIndex < axis.Count; startIndex++)
                if (axis[startIndex] >= target)
                {
                    if (startIndex > 0) startIndex--;
                    break;
                }
            for (endIndex = startIndex; endIndex < axis.Count; endIndex++)
                if (axis[endIndex] > target) break;

            startIndex = Math.Max(0, startIndex - indexRange);
            startIndex = Math.Min(axis.Count - 1, startIndex);
            if (startIndex < 0) Debugger.Break();
            endIndex = Math.Max(1, endIndex + indexRange);
            endIndex = Math.Min(axis.Count, endIndex + indexRange);
            var result = new List<double>();
            for (var index = startIndex; index < endIndex; index++)
                result.Add(axis[index]);
            return result;
        }

        static T FindNearestInSublist(Geo location, System.Collections.Generic.IList<T> list, int startIndex, int itemCount)
        {
            var minDistance = double.MaxValue;
            T closestSample = null;
            for (var indexOffset = 0; indexOffset < itemCount; indexOffset++)
            {
                var item = list[startIndex + indexOffset];
                var curDistance = item.DistanceKilometers(location);
                if (curDistance >= minDistance) continue;
                minDistance = curDistance;
                closestSample = item;
            }
            if (closestSample == null) Debugger.Break();
            return closestSample;
        }

        public bool IsSorted { get; private set; }

        public void Sort()
        {
            if (IsSorted || _arrayList.GetIsSorted())
            {
                Debug.WriteLine("{0}: Call to Sort() avoided.  List already sorted.", DateTime.Now);
                IsSorted = true;
                return;
            }

            // System.Diagnostics.Debug.WriteLine("{0}: About to call ParallelSort", DateTime.Now);
            var array = _arrayList.ToArray();
            array.ParallelSort();
            _arrayList.Clear();
            _arrayList.AddRange(array);
            // System.Diagnostics.Debug.WriteLine("{0}: Returned from ParallelSort", DateTime.Now);
            IsSorted = true;
        }

        public T this[double longitude, double latitude]
        {
            get
            {
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                if (_latitudes == null || _longitudes == null) throw new ApplicationException("_latitudes or _longitudes is null");
                if (_latitudeHashTable == null || _longitudeHashTable == null) CreateHashTables();
                if (_latitudeHashTable == null || _longitudeHashTable == null) throw new ApplicationException("One or both hash tables are null");
                longitude = Math.Round(longitude, 4);
                latitude = Math.Round(latitude, 4);
                if (!_longitudeHashTable.Contains(longitude)) return null;
                if (!_longitudeHashTable[longitude].Contains(latitude)) return null;
                return _longitudeHashTable[longitude][latitude];
            }
        }

        public T this[uint lonIndex, uint latIndex]
        {
            get
            {
                if (_twoDIndex != null) return _twoDIndex[lonIndex, latIndex];
                Debug.WriteLine("{0}: EnvironmentData: About to calculate 2D index", DateTime.Now);
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                if (_latitudes == null || _longitudes == null) throw new ApplicationException("_latitudes or _longitudes is null");
                _twoDIndex = new T[_longitudes.Count, _latitudes.Count];
                if (_latitudeHashTable == null || _longitudeHashTable == null) CreateHashTables();
                if (_latitudeHashTable == null || _longitudeHashTable == null) throw new ApplicationException("One or both hash tables are null");
                for (var curLatIndex = 0; curLatIndex < _latitudes.Count; curLatIndex++)
                {
                    var lat = _latitudes[curLatIndex];
                    if (!_latitudeHashTable.Contains(lat)) continue;
                    var curLatHashBucket = _latitudeHashTable[lat];
                    for (var curLonIndex = 0; curLonIndex < _longitudes.Count; curLonIndex++)
                    {
                        var lon = _longitudes[curLonIndex];
                        if (!curLatHashBucket.Contains(lon)) continue;
                        _twoDIndex[curLonIndex, curLatIndex] = curLatHashBucket[lon];
                    }
                }
                Debug.WriteLine("{0}: EnvironmentData: 2D index calculation complete", DateTime.Now);
                return _twoDIndex[lonIndex, latIndex];
            }
        }
        [NonSerialized] T[,] _twoDIndex;

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
                    resultList.Add(source[sourceWidthIndex, sourceHeightIndex]);
                }
            }
            result.AddRange(resultList);
            return result;
        }

        public void TrimToNearestPoints(GeoRect geoRect, double? rangeOutKm = null)
        {
            var trimRect = GeoRect.Inflate(geoRect, rangeOutKm.HasValue ? rangeOutKm.Value : 0.01);
            _arrayList.RemoveAll(point => (point != null) && (!trimRect.Contains(point)));
        }

        #region List<T> overrides
        public void Add(T item)
        {
            _arrayList.Add(item);
            ClearHelperIndices();
        }

        void ClearHelperIndices()
        {
            _latitudes = _longitudes = null;
            _twoDIndex = null;
            IsSorted = false;
        }

        public void AddRange(IEnumerable<T> collection)
        {
            foreach (var item in collection)
                _arrayList.Add(item);
            ClearHelperIndices();
        }

        public void Clear()
        {
            _arrayList.Clear();
            ClearHelperIndices();
        }

        public bool Contains(T item) { return _arrayList.Contains(item); }

        public void CopyTo(T[] array, int arrayIndex) { _arrayList.CopyTo(array, arrayIndex); }

        public bool Remove(T item)
        {
            var result = _arrayList.Remove(item);
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
            get { return _arrayList.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return false; }
        }

        /// <summary>
        /// Determines the index of a specific item in the <see cref="T:System.Collections.Generic.IList`1"/>.
        /// </summary>
        /// <returns>
        /// The index of <paramref name="item"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.IList`1"/>.</param>
        public int IndexOf(T item) { return _arrayList.IndexOf(item); }

        /// <summary>
        /// Inserts an item to the <see cref="T:System.Collections.Generic.IList`1"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="item"/> should be inserted.</param><param name="item">The object to insert into the <see cref="T:System.Collections.Generic.IList`1"/>.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        public void Insert(int index, T item) { throw new NotImplementedException(); }

        public void RemoveAt(int index)
        {
            _arrayList.RemoveAt(index);
            ClearHelperIndices();
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <returns>
        /// The element at the specified index.
        /// </returns>
        /// <param name="index">The zero-based index of the element to get or set.</param><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is not a valid index in the <see cref="T:System.Collections.Generic.IList`1"/>.</exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IList`1"/> is read-only.</exception>
        T System.Collections.Generic.IList<T>.this[int index]
        {
            get { return _arrayList[index]; }
            set { throw new NotImplementedException(); }
        }
        #endregion

        static double CalculateResolution(System.Collections.Generic.IList<double> axis)
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
                var lonRes = CalculateResolution(_longitudes);
                var latRes = CalculateResolution(_latitudes);
                return Math.Abs(lonRes - latRes) < 0.0001 ? lonRes : double.NaN;
            }
        }

        [NonSerialized] HashDictionary<double, HashDictionary<double, T>> _longitudeHashTable;
        [NonSerialized] HashDictionary<double, HashDictionary<double, T>> _latitudeHashTable;

        object _hashLockObject;
        void CreateHashTables()
        {
            if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
            if (_latitudes == null || _longitudes == null) throw new ApplicationException("_latitudes or _longitudes is null");
            if (_hashLockObject == null) lock (_classLockObject) _hashLockObject = new object();
            lock (_hashLockObject)
            {
                _latitudeHashTable = new HashDictionary<double, HashDictionary<double, T>>();
                _longitudeHashTable = new HashDictionary<double, HashDictionary<double, T>>();
                foreach (var lat in _latitudes) _latitudeHashTable[lat] = new HashDictionary<double, T>();
                foreach (var lon in _longitudes) _longitudeHashTable[lon] = new HashDictionary<double, T>();
                Debug.WriteLine("{0}: EnvironmentData: About to populate hash table", DateTime.Now);
                foreach (var item in _arrayList)
                {
                    var lat = Math.Round(item.Latitude, 4);
                    var lon = Math.Round(item.Longitude, 4);
                    _latitudeHashTable[lat][lon] = item;
                    _longitudeHashTable[lon][lat] = item;
                }
                Debug.WriteLine("{0}: EnvironmentData: Hash table population complete", DateTime.Now);
            }
        }

        object _indexLockObject;
        static readonly object _classLockObject = new object();

        void CreateLatLonIndices()
        {
            if (_indexLockObject == null) lock (_classLockObject) _indexLockObject = new object();
            lock (_indexLockObject)
            {
                _latitudes = new HashedArrayList<double>();
                _longitudes = new HashedArrayList<double>();
                foreach (var point in _arrayList)
                {
                    _latitudes.Add(Math.Round(point.Latitude, 4));
                    _longitudes.Add(Math.Round(point.Longitude, 4));
                }
                _latitudes.Sort();
                _longitudes.Sort();
            }
        }

        HashedArrayList<double> _latitudes;
        HashedArrayList<double> _longitudes;
        public HashedArrayList<double> Latitudes
        {
            get
            {
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                return _latitudes;
            }
        }

        public HashedArrayList<double> Longitudes
        {
            get
            {
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                return _longitudes;
            }
        }

        /// <summary>
        /// The GeoRect that contains the field data
        /// </summary>
        public virtual GeoRect GeoRect
        {
            get
            {
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                if (_latitudes == null || _longitudes == null) throw new ApplicationException("_latitudes or _longitudes is null");
                return new GeoRect(_latitudes.Last(), _latitudes.First(), _longitudes.Last(), _longitudes.First());
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
        public IEnumerator<T> GetEnumerator() { return _arrayList.GetEnumerator(); }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
        #endregion

        internal class DoubleComparer : IEqualityComparer<double>
        {
            public bool Equals(double x, double y) { return x.Equals(y); }
            public int GetHashCode(double obj) { return obj.GetHashCode(); }
        }
    }
#if false
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
#endif

    [Serializable]
    public class TimePeriodEnvironmentData<T> where T : EarthCoordinate, new()
    {
        public TimePeriodEnvironmentData() { EnvironmentData = new EnvironmentData<T>(); }

        public NAVOTimePeriod TimePeriod { get; set; }
        public EnvironmentData<T> EnvironmentData { get; set; }
    }
}
