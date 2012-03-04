using System;
using System.IO;
using ESME;
using ESME.Environment;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;

namespace InstallableNAVO
{
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Bathymetry",
                Name = "DBDB-V 5.4 (installed)",
                Description = "Digital Bathymetric Data Base - Variable Resolution v5.4, from US Navy/NAVOCEANO")]
    public sealed class DBDB54 : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        const string RequiredDBDBFilename = "dbdbv5_level0c_0.h5";

        public DBDB54()
        {
            SetPropertiesFromAttributes(GetType());
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            PluginType = PluginType.EnvironmentalDataSource;
            AvailableResolutions = new[] { 2, 1, 0.5f, 0.1f, 0.05f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\DBDB-V 5.4");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredDBDBFilename));
        }

        readonly string _dataDirectory;

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.DBDB.Extract(_dataDirectory, resolution, geoRect, progress);
        }
    }
#if false
    [Serializable]
    public sealed class DBDB54Configuration : PluginConfiguration
    {
        public DBDB54Configuration(IHRCPlugin plugin)
        {
            PluginType = plugin.GetType();
            PluginName = plugin.PluginName;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "Directory must exist and contain 24 appropriate GDEM NetCDF files (names like [t|s]gdemv3s[01-12].nc)",
                    RuleDelegate = (o, r) => ((DBDB54Configuration)o).DataLocation != null && IsDirectoryValid(((DBDB54Configuration)o).DataLocation),
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

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<DBDB54Configuration>(x => x.DataLocation);
        string _dataLocation = "DBDB54";

        #endregion

        public const string DataLocationHelp = "A file called dbdbv5_level0c_0.h5";

        static bool IsDirectoryValid(string directoryPath)
        {
            if (!Directory.Exists(directoryPath)) return false;
            var requiredFiles = new[] { "", "" };
            return true;
        }
    }
#endif
}
