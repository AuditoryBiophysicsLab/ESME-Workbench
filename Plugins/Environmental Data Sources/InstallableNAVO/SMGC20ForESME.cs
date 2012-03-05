using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using InstallableNAVO.Controls;
using InstallableNAVO.Databases;
using Microsoft.Win32;

namespace InstallableNAVO
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Wind",
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
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new[] { 60f };
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

    [PartCreationPolicy(CreationPolicy.Shared)]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Wind",
                Name = "SMGC 2.0 for NAVO",
                Description = "Surface Marine Gridded Climatology Database v2.0 from US Navy/NAVOCEANO")]
    public sealed class SMGC20ForNAVO : EnvironmentalDataSourcePluginBase<Wind>
    {
        public SMGC20ForNAVO()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The SMGC data directory, which should contain 64,800 *.stt files";
            ControlCaption = "SMGC data directory";
            DialogTitle = "Please locate one of the 64,800 SMGC data files, such as 'n00e000.stt'";
            FilenameFilter = "SMGC data files (*.stt)|*.stt|All files (*.*)|*.*";
            ConfigurationControl = new NAVOConfigurationControl { DataContext = this };

            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new[] { 60f };

            IsSelectable = true;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "File must exist and be named dbdbv5_level0c_0.h5",
                    RuleDelegate = (o, r) => ((SMGC20ForNAVO)o).IsConfigured,
                },
            });
        }

        public override bool IsConfigured
        {
            get
            {
                return IsDirectoryValid(DataLocation);
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<SMGC20ForNAVO> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<SMGC20ForNAVO>.Load(ConfigurationFile, null);
            DataLocation = settings.DataLocation;
        }

        bool IsDirectoryValid(string directory)
        {
            if (string.IsNullOrEmpty(directory)) return false;
            var files = Directory.GetFiles(directory, "*.stt", SearchOption.AllDirectories);
            return files.Length >= 64800;
        }

        [XmlIgnore] public string ControlCaption { get; set; }
        [XmlIgnore] public string DialogTitle { get; set; }
        [XmlIgnore] public string FilenameFilter { get; set; }
        [XmlIgnore] public string DataLocationHelp { get; set; }
        #region public string DataLocation { get; set; }

        public string DataLocation
        {
            get { return _dataLocation; }
            set
            {
                if (_dataLocation == value) return;
                _dataLocation = value;
                if (_dataLocation == null) return;
                // If the user chose a file, set _dataLocation to the directory that contains the file
                if ((File.GetAttributes(_dataLocation) & FileAttributes.Directory) != FileAttributes.Directory)
                    _dataLocation = Path.GetDirectoryName(_dataLocation);
                // If the directory pointed to by _dataLocation is not valid, make _dataLocation point to the parent directory
                if (!string.IsNullOrEmpty(_dataLocation) && Directory.Exists(_dataLocation))
                    if (!IsDirectoryValid(_dataLocation)) _dataLocation = Path.GetDirectoryName(_dataLocation);
                // If the directory still is not valid, set it back to the user's original choice
                if (!string.IsNullOrEmpty(_dataLocation) && Directory.Exists(_dataLocation))
                    if (!IsDirectoryValid(_dataLocation)) _dataLocation = value;
                NotifyPropertyChanged(DataLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<SMGC20ForNAVO>(x => x.DataLocation);
        string _dataLocation;

        #endregion

        public override Wind Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return SMGC.Import(geoRect, DataLocation);
        }
    }
}
