using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Xml.Serialization;
using StandaloneNAVO.Controls;

namespace StandaloneNAVO
{
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