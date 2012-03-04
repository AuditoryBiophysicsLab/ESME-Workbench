using System;
using System.ComponentModel.Composition;
using ESME.Environment;
using HRC.Navigation;

namespace ESME.Plugins
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Wind",
                Name = "None",
                Description = "Wind data not available or disabled by user")]
    public sealed class NoWindData : EnvironmentalDataSourcePluginBase<Wind>
    {
        public NoWindData()
        {
            SetPropertiesFromAttributes(GetType());
            IsSelectable = true;
            IsConfigured = false;
        }
        public override Wind Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null) { throw new NotImplementedException(); }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Sound Speed",
                Name = "None",
                Description = "Sound speed data not available or disabled by user")]
    public sealed class NoSoundSpeedData : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public NoSoundSpeedData()
        {
            SetPropertiesFromAttributes(GetType());
            IsSelectable = true;
            IsConfigured = false;
        }
        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null) { throw new NotImplementedException(); }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Sediment",
                Name = "None",
                Description = "Wind data not available or disabled by user")]
    public sealed class NoSedimentData : EnvironmentalDataSourcePluginBase<Sediment>
    {
        public NoSedimentData()
        {
            SetPropertiesFromAttributes(GetType());
            IsSelectable = true;
            IsConfigured = false;
        }
        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null) { throw new NotImplementedException(); }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Bathymetry",
                Name = "None",
                Description = "Wind data not available or disabled by user")]
    public sealed class NoBathymetryData : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        public NoBathymetryData()
        {
            SetPropertiesFromAttributes(GetType());
            IsSelectable = true;
            IsConfigured = false;
        }
        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null) { throw new NotImplementedException(); }
    }
}
