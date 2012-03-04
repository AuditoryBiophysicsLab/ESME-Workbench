using System;
using System.ComponentModel.Composition;
using ESME.Environment;
using HRC.Navigation;

namespace ESME.Plugins
{
    public class EmptyEnvironmentalDataSource<T> : EnvironmentalDataSourcePluginBase<T>
    {
        public EmptyEnvironmentalDataSource() 
        {
            IsSelectable = true;
            IsConfigured = false;
        }
        protected override void Save() { throw new NotImplementedException(); }
        public override T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null) { throw new NotImplementedException(); }
    }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource, Subtype = "Wind", Name = "None", Description = "Wind data not available or disabled by user")]
    public sealed class NoWindData : EmptyEnvironmentalDataSource<Wind> { public NoWindData() { SetPropertiesFromAttributes(GetType()); } }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource, Subtype = "Sound Speed", Name = "None", Description = "Sound speed data not available or disabled by user")]
    public sealed class NoSoundSpeedData : EmptyEnvironmentalDataSource<SoundSpeed> { public NoSoundSpeedData() { SetPropertiesFromAttributes(GetType()); } }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource, Subtype = "Sediment", Name = "None", Description = "Wind data not available or disabled by user")]
    public sealed class NoSedimentData : EmptyEnvironmentalDataSource<Sediment> { public NoSedimentData() { SetPropertiesFromAttributes(GetType()); } }

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource, Subtype = "Bathymetry", Name = "None", Description = "Wind data not available or disabled by user")]
    public sealed class NoBathymetryData : EmptyEnvironmentalDataSource<Bathymetry> { public NoBathymetryData() { SetPropertiesFromAttributes(GetType()); } }
}
