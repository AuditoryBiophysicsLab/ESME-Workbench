using System;
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

namespace InstallableNAVOPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Wind,
                           Name = "SMGC 2.0 for ESME Workbench",
                           Description = "Surface Marine Gridded Climatology Database v2.0 from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class SMGC20ForESME : EnvironmentalDataSourcePluginBase<Wind>
    {
        const string RequiredSMGCFilename = "smgc.wind";

        public SMGC20ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            //DataLocationHelp = "A file called smgc.wind";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllTimePeriods.ToArray();
            AvailableResolutions = new[] { 60f };
            var mo = new ManagementObject("Win32_Processor.DeviceID='CPU0'");
            var regKey = Registry.LocalMachine.OpenSubKey(string.Format(@"Software{0}\Boston University\ESME Workbench\Data Sources\SMGC 2.0 Minimal", (ushort)mo["AddressWidth"] == 64 ? @"\Wow6432Node" : ""));
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredSMGCFilename));
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }

        readonly string _dataDirectory;

        #region Wind GlobalDataset { get; }

        Wind GlobalDataset
        {
            get
            {
                if (_globalDataset != null) return _globalDataset;
                lock (_lockObject)
                {
                    _globalDataset = Wind.Load(Path.Combine(_dataDirectory, RequiredSMGCFilename));
                    return _globalDataset;
                }
            }
        }

        Wind _globalDataset;
        readonly object _lockObject = new object();

        #endregion

        public override Wind Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var timePeriodData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = timePeriod };
            timePeriodData.EnvironmentData.AddRange(GlobalDataset[timePeriod].EnvironmentData.PointsWithin(geoRect));
            var result = new Wind();
            result.TimePeriods.Add(timePeriodData);
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
