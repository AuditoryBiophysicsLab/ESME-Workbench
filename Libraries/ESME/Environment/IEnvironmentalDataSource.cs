using System;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IESMEPlugin
    {
        float[] Resolutions { get; }
        string DataLocation { get; set; }
        string DataLocationHelp { get; }
        bool IsDataLocationValid { get; }
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null);
    }

    public interface IGDEM3DataSource<out T> : IEnvironmentalDataSource<T>
    {
        T ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null);
        T ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null);
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, EarthCoordinate<float> deepestPoint, IProgress<float> progress = null);
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, Bathymetry bathymetry, IProgress<float> progress = null);
    }
}
