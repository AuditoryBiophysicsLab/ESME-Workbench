using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Management;
using ESME.Environment;
using ESME.Environment.NAVO;
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
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.SoundSpeed,
                           Name = "GDEM-V 3.0 for ESME Workbench",
                           Description = "Generalized Digital Environment Model, Variable Resolution version 3.0 from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class GDEM3ForESME : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllTimePeriods.ToArray();
            AvailableResolutions = new float[] { 15 };
            var mo = new ManagementObject("Win32_Processor.DeviceID='CPU0'");
            var regKey = Registry.LocalMachine.OpenSubKey(string.Format(@"Software{0}\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0", (ushort)mo["AddressWidth"] == 64 ? @"\Wow6432Node" : ""));
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           GDEM.IsDirectoryValid(_dataDirectory);
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }
        public override string Xml { get; set; }

        readonly string _dataDirectory;

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(_dataDirectory, timePeriod, geoRect));
            return result;
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