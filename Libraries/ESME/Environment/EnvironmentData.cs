using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using C5;
using HRC.Collections;
using HRC.LinqStatistics;
using HRC.Navigation;

namespace ESME.Environment
{
    [Serializable]
    public class EnvironmentData<T> : System.Collections.Generic.IList<T> where T : Geo, new()
    {
        /// <summary>
        /// Construct an EnvironmentData object
        /// </summary>
        /// <param name="maxPrecision">
        /// Optional.  Number of decimal digits of precision to consider when matching points.  Default value is 4, 
        /// which means that matches within .0001 degree (four decimal places) will be considered exact matches.
        /// According to http://genaud.net/2010/06/meter-per-degree-longitude, at the equator (the worst case, as 
        /// a degree of longitude covers the most ground at the equator) this would mean that points within 
        /// approximately 11 meters are considered to be equivalent.  At higher latitudes, the potential error in longitude
        /// shrinks with the cosine of the latitude.
        /// For the length of a degree of latitude, please see http://en.wikipedia.org/wiki/Latitude#The_length_of_a_degree_of_latitude
        /// </param>
        public EnvironmentData(int maxPrecision = 4)
        {
            _maxPrecision = maxPrecision;
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

        readonly int _maxPrecision;
        readonly List<T> _arrayList = new List<T>();

        public T this[int index] { get { return _arrayList[index]; } }

#if false
        public T this[Geo location]
        {
            get
            {
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
#endif
        /// <summary>
        /// Returns the nearest point in the data set, optionally within a maximum distance from the requested point
        /// </summary>
        /// <param name="location">The location of interest</param>
        /// <param name="maxDistance">The maximum distance, in kilometers, within which to find the nearest point.  
        /// Default value is double.NaN, which means any distance is acceptable</param>
        /// <returns>The nearest point that satisfies the distance criteria</returns>
        public T GetNearestPoint(Geo location, double maxDistance = double.NaN)
        {
            if (!double.IsNaN(maxDistance) && (maxDistance < 0)) throw new ArgumentOutOfRangeException("maxDistance", "Must be double.NaN or a non-negative value");
            var maxIndexRange = Math.Max(Latitudes.Count, Longitudes.Count);
            // Start with a minimum search window (the next-lowest to the next-highest value in both latitude and longitude), 
            // expanding the search window if no points are found within the smaller window values
            var candidatePoints = new List<T>();
            for (var searchWindow = 0; searchWindow < maxIndexRange; searchWindow++)
            {
                int latStartIndex, latEndIndex;
                int lonStartIndex, lonEndIndex;
                // Clear the list of candidate points each time we expand the search window.
                candidatePoints.Clear();
                // Find the minimum range to search within the latitude axis, within the current search window
                GetSearchRange(_latitudes, location.Latitude, out latStartIndex, out latEndIndex, searchWindow);
                // Find the minimum range to search within the longitude axis, within the current search window
                GetSearchRange(_longitudes, location.Longitude, out lonStartIndex, out lonEndIndex, searchWindow);
                T matchingPoint;
                if (_twoDIndex == null) Build2DIndex();
                for (var curLatIndex = latStartIndex; curLatIndex <= latEndIndex; curLatIndex++)
                {
                    var lat = _latitudes[curLatIndex];
                    if (!_latitudeHashTable.Contains(lat)) continue;
                    var curLatHashBucket = _latitudeHashTable[lat];
                    for (var curLonIndex = lonStartIndex; curLonIndex <= lonEndIndex; curLonIndex++)
                    {
                        var lon = _longitudes[curLonIndex];
                        if (!curLatHashBucket.Contains(lon)) continue;
                        matchingPoint = curLatHashBucket[lon];
                        if (double.IsNaN(maxDistance) || (location.DistanceKilometers(matchingPoint) <= maxDistance)) 
                            candidatePoints.Add(matchingPoint);
                    }
                }
                // If we found no candidate points within the current window size, expand the window and try again
                if (candidatePoints.Count == 0) continue;
                var minDistance = double.MaxValue;
                matchingPoint = null;
                foreach (var item in candidatePoints) 
                {
                    var curDistance = item.DistanceKilometers(location);
                    if (curDistance >= minDistance) continue;
                    minDistance = curDistance;
                    matchingPoint = item;
                }
                if (matchingPoint != null) return matchingPoint;
                // If we get to this point, something really bad/wierd has happened
                throw new InvalidOperationException("GetNearestPoint found matching points, but none were selected as being the nearest.  Should never happen!");
            }
            throw new ArgumentOutOfRangeException("location", "No matching data found, or data set is empty");
        }

        /// <summary>
        /// Returns the nearest point in the data set, optionally within a maximum distance from the requested point
        /// </summary>
        /// <param name="latitude">The latitude of the requested point</param>
        /// <param name="longitude">The longitude of the requested point</param>
        /// <param name="maxDistance">The maximum distance, in kilometers, within which to find the nearest point.  
        /// Default value is double.NaN, which means any distance is acceptable</param>
        /// <returns>The nearest point that satisfies the distance criteria</returns>
        public T GetNearestPoint(double latitude, double longitude, double maxDistance = double.NaN) { return GetNearestPoint(new Geo(latitude, longitude), maxDistance); }

        /// <summary>
        /// Tries to point in the data set that is nearest to a requested point, optionally within a distance constraint from the requested point.
        /// </summary>
        /// <param name="location">The location of interest</param>
        /// <param name="result">The point in the data set closest to the requested point.  This value is only meaningful if the method returns true.</param>
        /// <param name="maxDistance">The maximum distance, in kilometers, within which to find the nearest point.  
        /// Default value is double.NaN, which means any distance is acceptable</param>
        /// <returns>true if a point was found within the distance constraint, false otherwise</returns>
        public bool TryGetNearestPoint(Geo location, out T result, double maxDistance = double.NaN)
        {
            result = null;
            try
            {
                result = GetNearestPoint(location, maxDistance);
                return true;
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Tries to point in the data set that is nearest to a requested point, optionally within a distance constraint from the requested point.
        /// </summary>
        /// <param name="latitude">The latitude of the requested point</param>
        /// <param name="longitude">The longitude of the requested point</param>
        /// <param name="result">The point in the data set closest to the requested point.  This value is only meaningful if the method returns true.</param>
        /// <param name="maxDistance">The maximum distance, in kilometers, within which to find the nearest point.  
        /// Default value is double.NaN, which means any distance is acceptable</param>
        /// <returns>true if a point was found within the distance constraint, false otherwise</returns>
        public bool TryGetNearestPoint(double latitude, double longitude, out T result, double maxDistance = double.NaN) { return TryGetNearestPoint(new Geo(latitude, longitude), out result, maxDistance); }

        /// <summary>
        /// Returns the requested point from the data set
        /// </summary>
        /// <param name="latitude">The latitude of the requested point</param>
        /// <param name="longitude">The longitude of the requested point</param>
        /// <returns>The requested point</returns>
        public T GetExactPoint(double latitude, double longitude)
        {
            if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
            if (_latitudes == null || _longitudes == null) throw new InvalidOperationException("_latitudes or _longitudes is null");
            if (_latitudeHashTable == null || _longitudeHashTable == null) CreateHashTables();
            if (_latitudeHashTable == null || _longitudeHashTable == null) throw new InvalidOperationException("One or both hash tables are null");
            longitude = Math.Round(longitude, _maxPrecision);
            latitude = Math.Round(latitude, _maxPrecision);
            if (!_longitudeHashTable.Contains(longitude)) throw new ArgumentOutOfRangeException("longitude", "Specified longitude not found in hash table");
            if (!_longitudeHashTable[longitude].Contains(latitude)) throw new ArgumentOutOfRangeException("latitude", "Specified latitude not found in hash table");
            return _longitudeHashTable[longitude][latitude];
        }

        /// <summary>
        /// Returns the requested point from the data set
        /// </summary>
        /// <param name="location">The location of the requested point</param>
        /// <returns>The requested point</returns>
        public T GetExactPoint(Geo location) { return GetExactPoint(location.Latitude, location.Longitude); }

        /// <summary>
        /// Tries to retrieve the requested point from the data set
        /// </summary>
        /// <param name="latitude">The latitude of the requested point</param>
        /// <param name="longitude">The longitude of the requested point</param>
        /// <param name="result">The requested point.  This value is only meaningful if the function returns true</param>
        /// <returns>true if the data set contains the requested point, false otherwise</returns>
        public bool TryGetExactPoint(double latitude, double longitude, out T result)
        {
            result = null;
            try
            {
                result = GetExactPoint(latitude, longitude);
                return true;
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Tries to retrieve the requested point from the data set
        /// </summary>
        /// <param name="location">The location of the requested point</param>
        /// <param name="result">The requested point.  This value is only meaningful if the function returns true</param>
        /// <returns>true if the data set contains the requested point, false otherwise</returns>
        public bool TryGetExactPoint(Geo location, out T result) { return TryGetExactPoint(location.Latitude, location.Longitude, out result); }

        /// <summary>
        /// Return a range of indices in a given list that contain a target value (does not to have to be an exact match), with an optional range of indices to search.
        /// For example, if axis contains the following values: {0, 1, 2, 3, 4, 5, 6, 7, 8, 9} is searched for target 5.5 with the default indexRange of 0, this function will return {5, 6}, because these indices bracket the target value.
        /// If for the same axis, the target value is 5, the returned indices are {4, 5, 6} because the first index in the returned axis is always less than the target, if possible
        /// If for the same axis the target value is 5.5, and the indexRange is 2, the function will return {3, 4, 5, 6, 7, 8}, which is plus and minus 2 indices to either side of the 0-bracketed return value
        /// If for the same axis, the target is 0, the returned indices are {0, 1} because the target value cannot be bracketed since it lies at the start of the axis
        /// This function should never return any values that are not in the original list
        /// </summary>
        /// <param name="axis">The list of values that is to be searched</param>
        /// <param name="target">The target that is being searched for</param>
        /// <param name="startIndex">The index of the start of the search range</param>
        /// <param name="endIndex">The index of the end of the search range</param>
        /// <param name="indexRange">The number of index values to either side of the bracketed values to return</param>
        /// <returns></returns>
        static void GetSearchRange(C5.IList<double> axis, double target, out int startIndex, out int endIndex, int indexRange = 0)
        {
            // Find the start index (the highest index that contains a value that is less than the target value, or index 0, whichever is greater)
            for (startIndex = 0; startIndex < axis.Count; startIndex++)
                if (axis[startIndex] >= target)
                {
                    if (startIndex > 0) startIndex--;
                    break;
                }

            startIndex = Math.Max(0, startIndex - indexRange);
            startIndex = Math.Min(axis.Count - 1, startIndex);

            // Find the end index (the lowest index that contains a value that is greater than the target value, or the last index, whichever is lower)
            for (endIndex = startIndex; endIndex < axis.Count; endIndex++)
                if (axis[endIndex] > target) break;

            if (startIndex < 0) throw new IndexOutOfRangeException("Invalid start index");
            endIndex = Math.Max(0, endIndex + indexRange);
            endIndex = Math.Min(axis.Count - 1, endIndex + indexRange);
        }

#if false
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
#endif

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

#if false
        public T this[double longitude, double latitude]
        {
            get
            {
                if (_latitudes == null || _longitudes == null) CreateLatLonIndices();
                if (_latitudes == null || _longitudes == null) throw new ApplicationException("_latitudes or _longitudes is null");
                if (_latitudeHashTable == null || _longitudeHashTable == null) CreateHashTables();
                if (_latitudeHashTable == null || _longitudeHashTable == null) throw new ApplicationException("One or both hash tables are null");
                longitude = Math.Round(longitude, _maxPrecision);
                latitude = Math.Round(latitude, _maxPrecision);
                if (!_longitudeHashTable.Contains(longitude)) throw new ArgumentException("Specified longitude not found in hash table", "longitude");
                if (!_longitudeHashTable[longitude].Contains(latitude)) throw new ArgumentException("Specified latitude not found in hash table", "latitude");
                return _longitudeHashTable[longitude][latitude];
            }
        }
#endif

        public T this[uint lonIndex, uint latIndex]
        {
            get
            {
                if (_twoDIndex == null) Build2DIndex();
                if (_twoDIndex != null) return _twoDIndex[lonIndex, latIndex];
                throw new InvalidOperationException("_twoDIndex should not be null at this point!");
            }
        }
        [NonSerialized] T[,] _twoDIndex;

        void Build2DIndex()
        {
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
                    var lat = Math.Round(item.Latitude, _maxPrecision);
                    var lon = Math.Round(item.Longitude, _maxPrecision);
                    _latitudeHashTable[lat][lon] = item;
                    _longitudeHashTable[lon][lat] = item;
                }
                Debug.WriteLine("{0}: EnvironmentData: Hash table population complete", DateTime.Now);
            }
        }

        object _indexLockObject;
        readonly object _classLockObject = new object();

        void CreateLatLonIndices()
        {
            if (_indexLockObject == null) lock (_classLockObject) _indexLockObject = new object();
            lock (_indexLockObject)
            {
                _latitudes = new HashedArrayList<double>();
                _longitudes = new HashedArrayList<double>();
                foreach (var point in _arrayList)
                {
                    _latitudes.Add(Math.Round(point.Latitude, _maxPrecision));
                    _longitudes.Add(Math.Round(point.Longitude, _maxPrecision));
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

    [Serializable]
    public class TimePeriodEnvironmentData<T> where T : Geo, new()
    {
        public TimePeriodEnvironmentData() { EnvironmentData = new EnvironmentData<T>(); }

        public TimePeriod TimePeriod { get; set; }
        public EnvironmentData<T> EnvironmentData { get; set; }
    }
}
