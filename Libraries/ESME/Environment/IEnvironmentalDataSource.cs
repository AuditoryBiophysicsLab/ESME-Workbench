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
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null);
    }

    public interface IGDEM3DataSource<out T> : IEnvironmentalDataSource<T>
    {
        T ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null);
        T ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null);
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, EarthCoordinate<float> deepestPoint, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null);
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, Bathymetry bathymetry, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null);
    }
}
