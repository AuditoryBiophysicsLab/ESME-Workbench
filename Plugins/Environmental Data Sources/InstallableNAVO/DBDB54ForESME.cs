using System.ComponentModel.Composition;
using System.IO;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using ESME.Views.Locations;
using HRC.Navigation;
using HRC.Utility;
using Microsoft.Win32;
using NAVODatabaseAdapter;

namespace InstallableNAVOPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Bathymetry,
                           Name = "DBDB-V 5.4 for ESME Workbench",
                           Description = "Digital Bathymetric Data Base - Variable Resolution v5.4, from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class DBDB54ForESME : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        const string RequiredDBDBFilename = "dbdbv5_level0c_0.h5";
        const string RequiredDBDBExtractionProgram = "dbv5_command.exe";

        public DBDB54ForESME()
        {
            SetPropertiesFromAttributes(GetType());

            AvailableResolutions = new[] { 2, 1, 0.5f, 0.1f, 0.05f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\DBDB-V 5.4");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredDBDBFilename)) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredDBDBExtractionProgram));
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

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return DBDB.Extract(Path.Combine(_dataDirectory, RequiredDBDBFilename), Path.Combine(_dataDirectory, RequiredDBDBExtractionProgram), resolution, geoRect, progress);
        }
    }
}