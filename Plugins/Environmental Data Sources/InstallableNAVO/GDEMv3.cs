using System;
using System.ComponentModel.Composition;
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
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Sound Speed",
                Name = "GDEM-V 3.0 (installed)",
                Description = "Generalized Digital Environment Model, Variable Resolution, version 3.0, from US Navy/NAVOCEANO")]
    public sealed class GDEM3 : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3()
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
                           Databases.GDEM.IsDirectoryValid(_dataDirectory);
        }

        readonly string _dataDirectory;

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(Databases.GDEM.ReadFile(_dataDirectory, timePeriod, geoRect));
            return result;
        }
    }

#if false
    [Serializable]
    public sealed class GDEM3Configuration : PluginConfiguration
    {
        public GDEM3Configuration(IHRCPlugin plugin)
        {
            PluginType = plugin.GetType();
            PluginName = plugin.PluginName;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "Directory must exist and contain 24 appropriate GDEM NetCDF files (names like [t|s]gdemv3s[01-12].nc)",
                    RuleDelegate = (o, r) => ((GDEM3Configuration)o).DataLocation != null && IsDirectoryValid(((GDEM3Configuration)o).DataLocation),
                },
            });
        }

        #region public string DataLocation { get; set; }

        public string DataLocation
        {
            get { return _dataLocation; }
            set
            {
                if (_dataLocation == value) return;
                _dataLocation = value;
                NotifyPropertyChanged(DataLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<GDEM3Configuration>(x => x.DataLocation);
        string _dataLocation = "GDEM3";

        #endregion

        public const string DataLocationHelp = "A directory containing the expected 24 GDEM-V 3.0 NetCDF files with temperature and salinity data (tgdemv3s01.nc for example)";

        static bool IsDirectoryValid(string directoryPath)
        {
            if (!Directory.Exists(directoryPath)) return false;
            var requiredFiles = new[] {"", ""};
            return true;
        }
    }
#endif
}
