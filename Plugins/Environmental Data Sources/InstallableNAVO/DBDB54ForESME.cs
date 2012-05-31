using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Management;
using System.Windows.Controls;
using ESME.Environment;
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

            var mo = new ManagementObject("Win32_Processor.DeviceID='CPU0'");
            var regKey = Registry.LocalMachine.OpenSubKey(string.Format(@"Software{0}\Boston University\ESME Workbench\Data Sources\DBDB-V 5.4", (ushort)mo["AddressWidth"] == 64 ? @"\Wow6432Node" : ""));
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
        public override EnvironmentalDataSet SelectedDataSet
        {
            get
            {
                var selectedItem = ((MultipleSelectionsViewModel<float>)SelectionControlViewModel).SelectedItem;
                return new EnvironmentalDataSet { SourcePlugin = PluginIdentifier, Resolution = selectedItem.Value, TimePeriod = TimePeriod.Invalid };
            }
        }
    }
}