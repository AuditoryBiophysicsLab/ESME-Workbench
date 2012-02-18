using System;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IESMEPlugin
    {
        float[] Resolutions { get; }
        string DataLocation { get; set; }
        string DataLocationHelp { get; }
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration = null, IProgress<float> progress = null);
    }

    public interface IGDEM3DataSource<out T> : IEnvironmentalDataSource<T>
    {
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, EarthCoordinate<float> deepestPoint, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null);
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, Bathymetry bathymetry, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null);
    }
}
