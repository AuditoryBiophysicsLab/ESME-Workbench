using System;
using System.Collections.Generic;
using System.ComponentModel;
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
using NAVODatabaseAdapter;

namespace InstallableNAVO
{
    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
            Subtype = "Sound Speed",
            Name = "GDEM-V 3.0 for NAVO",
            Description = "Generalized Digital Environment Model, Variable Resolution version 3.0 from US Navy/NAVOCEANO")]
    public sealed class GDEM3ForNAVO : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3ForNAVO()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The directory containing the GDEM database files, such as 'sgdemv3s01.nc'";
            ControlCaption = "Directory containing the GDEM database files";
            DialogTitle = "Please locate one GDEM database file, such as 'sgdemv3s01.nc'";
            FilenameFilter = "NetCDF files (*.nc)|*.nc|All files (*.*)|*.*";
            ConfigurationControl = new NAVOConfigurationControl { DataContext = this };

            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new float[] { 15 };

            IsSelectable = true;

            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "Directory must exists and contain the 24 required GDEM database files",
                    RuleDelegate = (o, r) => ((GDEM3ForNAVO)o).IsConfigured,
                },
            });
        }

        public override bool IsConfigured
        {
            get
            {
                return DataLocation != null &&
                       Directory.Exists(DataLocation) &&
                       GDEM.IsDirectoryValid(DataLocation);
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<GDEM3ForNAVO> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<GDEM3ForNAVO>.Load(ConfigurationFile, null);
            DataLocation = settings.DataLocation;
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
                if (_dataLocation != null && (File.GetAttributes(_dataLocation) & FileAttributes.Directory) != FileAttributes.Directory) 
                    _dataLocation = Path.GetDirectoryName(_dataLocation);
                NotifyPropertyChanged(DataLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<GDEM3ForNAVO>(x => x.DataLocation);
        string _dataLocation;

        #endregion

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(DataLocation, timePeriod, geoRect));
            return result;
        }
    }
}
