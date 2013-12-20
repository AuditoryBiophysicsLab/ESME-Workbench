using System.ComponentModel.Composition;
using System.IO;
using System.Management;
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
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "BST 2.0 for ESME Workbench",
                           Description = "Bottom Sediments Type Database Version 2.0 Repacked from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class BST20ForESME : EnvironmentalDataSourcePluginBase<Sediment>
    {
        const string RequiredBSTFilename = "hfevav2.h5";

        public BST20ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            AvailableResolutions = new[] { 5f, 0.1f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };
            var mo = new ManagementObject("Win32_Processor.DeviceID='CPU0'");
            var regKey = Registry.LocalMachine.OpenSubKey(string.Format(@"Software{0}\Boston University\ESME Workbench\Data Sources\BST 2.0", (ushort)mo["AddressWidth"] == 64 ? @"\Wow6432Node" : ""));
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredBSTFilename));
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }
        public override string Xml { get; set; }

        readonly string _dataDirectory;

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return BST.Extract(Path.Combine(_dataDirectory, RequiredBSTFilename), geoRect, resolution, progress);
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