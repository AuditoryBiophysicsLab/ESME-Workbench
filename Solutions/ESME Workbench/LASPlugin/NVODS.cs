using System;
using System.ComponentModel.Composition;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;



namespace LASPlugin
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "NVODS",
                           Description = "National Virtual Oceanographic Data System")]
    public sealed class NVODS:EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            throw new NotImplementedException();
        }
    }
}
