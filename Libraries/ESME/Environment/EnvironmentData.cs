using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using HRC.Navigation;
using System.Windows;

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

        public void RemoveDuplicates()
        {
            var uniqueList = new List<T>();
            foreach (var curEntry in from curEntry in this
                                     let foundMatch = uniqueList.Any(curEntry.Equals)
                                     where !foundMatch
                                     select curEntry) {uniqueList.Add(curEntry);}
            Clear();
            AddRange(uniqueList);
        }

        [XmlIgnore]
        public List<double> Longitudes
        {
            get
            {
                if (_longitudes == null)
                {
                    _longitudes = this.Select(point => point.Longitude).Distinct().ToList();
                    _longitudes.Sort();
                }
                return _longitudes;
            }
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
                    _latitudes = this.Select(point => point.Longitude).Distinct().ToList();
                    _latitudes.Sort();
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
