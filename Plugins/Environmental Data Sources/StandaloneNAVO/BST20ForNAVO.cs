using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
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
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "BST 2.0 for NAVO",
                           Description = "Bottom Sediments Type Database Version 2.0 Repacked from US Navy/NAVOCEANO")]
    public sealed class BST20ForNAVO : EnvironmentalDataSourcePluginBase<Sediment>
    {
        public BST20ForNAVO()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The BST database file 'hfevav2.h5'";
            ControlCaption = "BST database file";
            DialogTitle = "Please locate the BST database 'hfeva2.h5'";
            FilenameFilter = "HDF5 files (*.h5)|*.h5|All files (*.*)|*.*";

            ConfigurationControl = new NAVOConfigurationControl { DataContext = this };
            AvailableResolutions = new[] { 5f, 0.1f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            IsSelectable = true;
            AddValidationRules(new ValidationRule<BST20ForNAVO>
            {
                PropertyName = "DataLocation",
                Description = "File must exist and be named hfevav2.h5",
                IsRuleValid = (target, rule) => target.IsConfigured,
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
            get { return DataLocation != null && File.Exists(DataLocation); }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<BST20ForNAVO> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<BST20ForNAVO>.LoadExistingFile(ConfigurationFile, null);
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
                _dataLocation = value;
                Save();
            }
        }

        string _dataLocation;

        #endregion

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return BST.Extract(DataLocation, geoRect, resolution, progress);
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