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
        T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod);
    }

    public interface IGDEM3DataSource<out T> : IEnvironmentalDataSource<T>
    {
        T ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod);
        T ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod);
    }

    public interface ISoundSpeedFieldExtender
    {
        SoundSpeedField Extend(SoundSpeedField temperatureField, SoundSpeedField salinityField, EarthCoordinate<float> deepestPoint = null);
    }
}
