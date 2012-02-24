using System;
using System.IO;
using System.Linq;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;

namespace InstallableNAVO
{
    public sealed class SMGC20Minimal : EnvironmentalDataSourcePluginBase<Wind>
    {
        const string RequiredSMGCFilename = "smgc.wind";

        public SMGC20Minimal()
        {
            PluginName = "SMGC 2.0 Minimal";
            PluginDescription = "Surface Marine Gridded Climatology Database v2.0, from US Navy/NAVOCEANO";
            //DataLocationHelp = "A file called smgc.wind";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            PluginType = PluginType.EnvironmentalDataSource;
            AvailableResolutions = new[] { 60f };
            Subtype = "Wind";
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\SMGC 2.0 Minimal");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredSMGCFilename));
#if false
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "File must exist and be named dbdbv5_level0c_0.h5",
                    RuleDelegate = (o, r) => ((DBDB54)o).DataLocation != null && Directory.Exists(((DBDB54)o).DataLocation) && File.Exists(Path.Combine(DataLocation, RequiredDBDBFilename)),
                },
            });
#endif
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

        public override Wind Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var timePeriodData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = timePeriod };
            timePeriodData.EnvironmentData.AddRange(GlobalDataset[timePeriod].EnvironmentData);
            timePeriodData.EnvironmentData.TrimToNearestPoints(geoRect);
            var result = new Wind();
            result.TimePeriods.Add(timePeriodData);
            return result;
        }
    }
}
