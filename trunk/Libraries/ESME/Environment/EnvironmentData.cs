using System;
using System.Collections.Generic;
using System.Linq;
using ESME.Environment.NAVO;
using HRC.Navigation;
using System.Windows;

namespace ESME.Environment
{
    public class EnvironmentData<TList> : List<TList> where TList: EarthCoordinate
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
                typeof (EarthCoordinate),
                typeof (Geo),
                typeof (Point),
                typeof (TList),
        };

        public TList this[EarthCoordinate location]
        {
            get
            {
                var minDistance = double.MaxValue;
                TList closestSample = null;
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

    public class TimePeriodEnvironmentData<TList> : EnvironmentData<TList> where TList : EarthCoordinate, new()
    {
        new public static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<TList>.ReferencedTypes);

        public NAVOTimePeriod TimePeriod { get; set; }
    }

    static class Tester
    {
        static TimePeriodEnvironmentData<EarthCoordinate<float>> _test1 = new TimePeriodEnvironmentData<EarthCoordinate<float>>();
        static TimeBasedEnvironmentData<EarthCoordinate<float>, float> _test2 = new TimeBasedEnvironmentData<EarthCoordinate<float>, float>();
    }

}
