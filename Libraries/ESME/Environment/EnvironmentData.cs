using System;
using System.Collections.Generic;
using System.Linq;
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
    }

    internal class EarthCoordinateComparer : IEqualityComparer<EarthCoordinate>
    {
        public bool Equals(EarthCoordinate x, EarthCoordinate y)
        {
            //Check whether the compared objects reference the same data.
            if (ReferenceEquals(x, y)) return true;
            //Check whether any of the compared objects is null.
            if (ReferenceEquals(x, null) || ReferenceEquals(y, null))
                return false;

            return x.DistanceTo(y) < 10;
        }

        public int GetHashCode(EarthCoordinate obj)
        {
            // Check whether the object is null
            if (ReferenceEquals(obj, null)) return 0;
            // Get the hash codes for the X, Y and Z fields
            var xHash = obj.X.GetHashCode();
            var yHash = obj.Y.GetHashCode();
            var zHash = obj.Z.GetHashCode();
            return xHash ^ yHash ^ zHash;
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
