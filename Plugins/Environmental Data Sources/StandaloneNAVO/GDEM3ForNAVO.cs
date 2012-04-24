using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Locations;
using ESME.Plugins;
using ESME.Views.Locations;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using NAVODatabaseAdapter;
using StandaloneNAVOPlugin.Controls;

namespace StandaloneNAVOPlugin
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.SoundSpeed,
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
            ConfigurationControl = new NAVOConfigurationControl {DataContext = this};

            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            AvailableResolutions = new float[] {15};

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
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }

        public override bool IsConfigured
        {
            get { return DataLocation != null && Directory.Exists(DataLocation) && GDEM.IsDirectoryValid(DataLocation); }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<GDEM3ForNAVO> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<GDEM3ForNAVO>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
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
                if (_dataLocation != null && (File.Exists(_dataLocation) || Directory.Exists(_dataLocation)) && (File.GetAttributes(_dataLocation) & FileAttributes.Directory) != FileAttributes.Directory) 
                    _dataLocation = Path.GetDirectoryName(_dataLocation);
                Save();
            }
        }

        string _dataLocation;

        #endregion

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(DataLocation, timePeriod, geoRect));
            return result;
        }
        public override IEnumerable<EnvironmentalDataSet> SelectedDataSets
        {
            get
            {
                return from simpleSelectionViewModel in ((MultipleSelectionsViewModel<float>)SelectionControlViewModel).SimpleSelectionViewModels
                       where simpleSelectionViewModel.IsSelected
                       from month in NAVOConfiguration.AllMonths
                       select new EnvironmentalDataSet
                       {
                           SourcePlugin = PluginIdentifier,
                           Resolution = simpleSelectionViewModel.Value,
                           TimePeriod = month,
                       };
            }
        }
    }
}
