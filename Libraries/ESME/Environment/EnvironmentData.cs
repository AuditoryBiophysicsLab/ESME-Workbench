using System;
using System.Collections.Generic;
using System.Linq;
using HRC.Navigation;
using System.Windows;

namespace ESME.Environment
{
    public class EnvironmentData<T> : List<T> where T: EarthCoordinate, new()
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
                typeof (EarthCoordinate),
                typeof (Geo),
                typeof (Point),
                typeof (T),
        };

        public virtual T this[EarthCoordinate location]
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
    }
}
