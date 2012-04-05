using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Locations;
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
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }

        readonly string _dataDirectory;

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return DBDB.Extract(Path.Combine(_dataDirectory, RequiredDBDBFilename), Path.Combine(_dataDirectory, RequiredDBDBExtractionProgram), resolution, geoRect, progress);
        }
        public override IEnumerable<EnvironmentalDataSet> SelectedDataSets
        {
            get
            {
                return from simpleSelection in ((MultipleSelectionsViewModel<float>)SelectionControlViewModel).SimpleSelectionViewModels
                       where simpleSelection.IsSelected
                       select new EnvironmentalDataSet
                       {
                           SourcePlugin = PluginIdentifier,
                           Resolution = simpleSelection.Value,
                           TimePeriod = TimePeriod.Invalid,
                       };
            }
        }
    }
}