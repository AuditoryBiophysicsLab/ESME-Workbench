using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Validation;
using InstallableNAVO.Controls;
using Microsoft.Win32;

namespace InstallableNAVO
{

    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Sediment",
                Name = "BST 2.0 for ESME Workbench",
                Description = "Bottom Sediments Type Database Version 2.0 Repacked from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class BST20ForESME : EnvironmentalDataSourcePluginBase<Sediment>
    {
        const string RequiredBSTFilename = "hfevav2.h5";

        public BST20ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            //DataLocationHelp = "A file called hfevav2.h5";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            AvailableResolutions = new[] { 5f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\BST 2.0");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredBSTFilename));
        }

        readonly string _dataDirectory;

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.BST.Extract(Path.Combine(_dataDirectory, RequiredBSTFilename), geoRect, resolution, progress);
        }
    }

    [ESMEPlugin(PluginType = PluginType.EnvironmentalDataSource,
                Subtype = "Sediment",
                Name = "BST 2.0 for NAVO",
                Description = "Bottom Sediments Type Database Version 2.0 Repacked from US Navy/NAVOCEANO")]
    public sealed class BST20ForNAVO : EnvironmentalDataSourcePluginBase<Sediment>
    {
        const string RequiredBSTFilename = "hfevav2.h5";

        public BST20ForNAVO()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "A file called hfevav2.h5";
            DialogTitle = "Please locate the BST database 'hfeva2.h5'";
            FilenameFilter = "HDF5 (*.h5)|*.h5|All files (*.*)|*.*";
            IsDirectoryBrowser = false;

            ConfigurationControl = new NAVOConfigurationControl { DataContext = this };
            AvailableResolutions = new[] { 5f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           File.Exists(Path.Combine(DataLocation, RequiredBSTFilename));
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "File must exist and be named hfevav2.h5",
                    RuleDelegate = (o, r) => ((BST20ForNAVO)o).DataLocation != null && File.Exists(((BST20ForNAVO)o).DataLocation) && Path.GetFileName(DataLocation) == RequiredBSTFilename,
                },
            });
        }

        public string DialogTitle { get; set; }
        public string FilenameFilter { get; set; }
        public bool IsDirectoryBrowser { get; set; }
        public string DataLocationHelp { get; set; }
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

        static readonly PropertyChangedEventArgs DataLocationChangedEventArgs = ObservableHelper.CreateArgs<BST20ForNAVO>(x => x.DataLocation);
        string _dataLocation;

        #endregion

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.BST.Extract(DataLocation, geoRect, resolution, progress);
        }
    }
}