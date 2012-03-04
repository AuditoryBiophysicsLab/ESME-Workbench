using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Xml.Serialization;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using InstallableNAVO.Controls;
using Microsoft.Win32;

namespace InstallableNAVO
{
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Bathymetry",
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
        }

        readonly string _dataDirectory;

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.DBDB.Extract(Path.Combine(_dataDirectory, RequiredDBDBFilename), Path.Combine(_dataDirectory, RequiredDBDBExtractionProgram), resolution, geoRect, progress);
        }
    }
    
    [Serializable]
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Bathymetry",
                Name = "DBDB-V 5.4 for NAVO",
                Description = "Digital Bathymetric Data Base - Variable Resolution v5.4 from US Navy/NAVOCEANO")]
    public sealed class DBDB54ForNAVO : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        const string RequiredDBDBFilename = "dbdbv5_level0c_0.h5";
        const string RequiredDBDBExtractionProgram = "dbv5_command.exe";

        public DBDB54ForNAVO()
        {
            SetPropertiesFromAttributes(GetType());
            DatabaseLocationHelp = "The DBDB-V database file 'dbdbv5_level0c_0.h5'";
            DatabaseControlCaption = "DBDB-V database file";
            DatabaseDialogTitle = "Please locate the DBDB-V database 'dbdbv5_level0c_0.h5'";
            DatabaseFilenameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*";

            ExtractorLocationHelp = "The DBDB-V extraction program 'dbv5_command.exe'";
            ExtractorControlCaption = "DBDB-V extraction program";
            ExtractorDialogTitle = "Please locate the DBDB-V extraction program 'dbv5_command.exe'";
            ExtractorFilenameFilter = "Executable files (*.exe)|*.exe|All files (*.*)|*.*";

            ConfigurationControl = new DBDBConfigurationControl { DataContext = this };
            AvailableResolutions = new[] { 2, 1, 0.5f, 0.1f, 0.05f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            IsSelectable = true;
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DatabaseLocation",
                    Description = "File must exist and be named dbdbv5_level0c_0.h5",
                    RuleDelegate = (o, r) => ((DBDB54ForNAVO)o).DatabaseLocation != null &&
                                             File.Exists(((DBDB54ForNAVO)o).DatabaseLocation) &&
                                             Path.GetFileName(DatabaseLocation) == RequiredDBDBFilename,
                },
                new ValidationRule
                {
                    PropertyName = "ExtractorLocation",
                    Description = "File must exist and be named dbv5_command.exe",
                    RuleDelegate = (o, r) => ((DBDB54ForNAVO)o).ExtractorLocation != null &&
                                             File.Exists(((DBDB54ForNAVO)o).ExtractorLocation) &&
                                             Path.GetFileName(ExtractorLocation) == RequiredDBDBExtractionProgram,
                },
            });
        }

        public override bool IsConfigured
        {
            get
            {
                return DatabaseLocation != null &&
                       Directory.Exists(DatabaseLocation) &&
                       File.Exists(DatabaseLocation) &&
                       File.Exists(ExtractorLocation);
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<DBDB54ForNAVO> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<DBDB54ForNAVO>.Load(ConfigurationFile, null);
            DatabaseLocation = settings.DatabaseLocation;
            ExtractorLocation = settings.ExtractorLocation;
        }

        [XmlIgnore] public string DatabaseControlCaption { get; set; }
        [XmlIgnore] public string DatabaseDialogTitle { get; set; }
        [XmlIgnore] public string DatabaseFilenameFilter { get; set; }
        [XmlIgnore] public string DatabaseLocationHelp { get; set; }
        [XmlIgnore] public string ExtractorControlCaption { get; set; }
        [XmlIgnore] public string ExtractorDialogTitle { get; set; }
        [XmlIgnore] public string ExtractorFilenameFilter { get; set; }
        [XmlIgnore] public string ExtractorLocationHelp { get; set; }
        #region public string DatabaseLocation { get; set; }

        public string DatabaseLocation
        {
            get { return _databaseLocation; }
            set
            {
                if (_databaseLocation == value) return;
                _databaseLocation = value;
                NotifyPropertyChanged(DataLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<DBDB54ForNAVO>(x => x.DatabaseLocation);
        string _databaseLocation;

        #endregion
        #region public string ExtractorLocation { get; set; }

        public string ExtractorLocation
        {
            get { return _extractorLocation; }
            set
            {
                if (_extractorLocation == value) return;
                _extractorLocation = value;
                NotifyPropertyChanged(ExtractionProgramLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractionProgramLocationChangedEventArgs = ObservableHelper.CreateArgs<DBDB54ForNAVO>(x => x.ExtractorLocation);
        string _extractorLocation;

        #endregion

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.DBDB.Extract(DatabaseLocation, ExtractorLocation, resolution, geoRect, progress);
        }
    }
}
