using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.Plugins;
using ESME.Views.Locations;
using HRC.Navigation;
using HRC.Utility;
using Microsoft.Win32;
using NAVODatabaseAdapter;

namespace InstallableNAVOPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.SoundSpeed,
                           Name = "GDEM-V 3.0 for ESME Workbench",
                           Description = "Generalized Digital Environment Model, Variable Resolution version 3.0 from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class GDEM3ForESME : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new float[] { 15 };
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           GDEM.IsDirectoryValid(_dataDirectory);
            UsageOptionsControl = new MultipleSelectionsView
            {
                DataContext = new MultipleSelectionsViewModel<float>
                {
                    UnitName = " min",
                    AvailableSelections = AvailableResolutions,
                }
            };
        }

        readonly string _dataDirectory;

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(_dataDirectory, timePeriod, geoRect));
            return result;
        }
    }
}