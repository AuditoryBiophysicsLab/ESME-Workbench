using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IESMEPlugin
    {
        EnvironmentDataType EnvironmentDataType { get; }
        float[] AvailableResolutions { get; }
        bool IsTimeVariantData { get; }
        TimePeriod[] AvailableTimePeriods { get; }
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, PercentProgress progress = null);
    }

    public enum EnvironmentDataType
    {
        Wind,
        SoundSpeed,
        Bathymetry,
        Sediment
    }
}
