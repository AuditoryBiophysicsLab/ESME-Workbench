using System;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IESMEPlugin
    {
        float[] AvailableResolutions { get; }
        bool IsTimeVariantData { get; }
        TimePeriod[] AvailableTimePeriods { get; }
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, IProgress<float> progress = null);
    }

    public interface IWindDataSource : IEnvironmentalDataSource<Wind> { }
    public interface ISoundSpeedDataSource : IEnvironmentalDataSource<SoundSpeed> { }
    public interface ISedimentDataSource : IEnvironmentalDataSource<Sediment> { }
    public interface IBathymetryDataSource : IEnvironmentalDataSource<Bathymetry> { }
}
