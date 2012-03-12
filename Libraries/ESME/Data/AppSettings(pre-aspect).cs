using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Collections;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Data
{
    [Serializable]
    public class OldAppSettings : ValidatingViewModel
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>
        {
            typeof (NAVOConfiguration),
        };

        static string _appSettingsDirectory;
        
        public static string ApplicationName
        {
            get { return _appName; }
            set
            {
                _appName = value;
                _appSettingsDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), _appName);
                if (!Directory.Exists(_appSettingsDirectory)) Directory.CreateDirectory(_appSettingsDirectory);
                AppSettingsFile = Path.Combine(_appSettingsDirectory, "settings.xml");
            }
        }

        static string _appName;

        public static string AppSettingsFile { get; private set; }

        public void SetDefaults()
        {
          if (OldRAMSettings != null) OldRAMSettings.SetDefaults();
          //if (DefaultPluginIdentifiers == null || DefaultPluginIdentifiers.Count == 0) SetDefaultPluginIdentifiers();
        }

        public void Save()
        {
            var serializer = new XmlSerializer<OldAppSettings> { Data = this };
            serializer.Save(AppSettingsFile, ReferencedTypes);
        }

        public void Save(List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            var serializer = new XmlSerializer<OldAppSettings> { Data = this };
            serializer.Save(AppSettingsFile, referencedTypes);
        }

        public static AppSettings Load()
        {
            return XmlSerializer<AppSettings>.Load(AppSettingsFile, ReferencedTypes);
        }

        public static AppSettings Load(string fileName)
        {
            return XmlSerializer<AppSettings>.Load(fileName, ReferencedTypes);
        }

        public static AppSettings Load(List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            return XmlSerializer<AppSettings>.Load(AppSettingsFile, referencedTypes);
        }

        public static AppSettings Load(string fileName, List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            return XmlSerializer<AppSettings>.Load(fileName, referencedTypes);
        }

        #region public SerializableDictionary<string, string> OpenFileServiceDirectories { get; set; }

        public SerializableDictionary<string, string> OpenFileServiceDirectories
        {
            get { return _openFileServiceDirectories ?? (_openFileServiceDirectories = new SerializableDictionary<string, string>()); }
            set
            {
                if (_openFileServiceDirectories == value) return;
                _openFileServiceDirectories = value;
                NotifyPropertyChanged(OpenFileServiceDirectoriesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OpenFileServiceDirectoriesChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.OpenFileServiceDirectories);
        SerializableDictionary<string, string> _openFileServiceDirectories;

        #endregion

        #region public string ScenarioDataDirectory { get; set; }

        static readonly PropertyChangedEventArgs ScenarioDataDirectoryChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.ScenarioDataDirectory);
        string _scenarioDataDirectory;

        public string ScenarioDataDirectory
        {
            get { return _scenarioDataDirectory; }
            set
            {
                if (_scenarioDataDirectory == value) return;
                _scenarioDataDirectory = value;
                NotifyPropertyChanged(ScenarioDataDirectoryChangedEventArgs);
            }
        }
        public bool ValidateScenarioDataDirectory(string simAreaFile, IMessageBoxService messageBoxService = null)
        {
            if (string.IsNullOrEmpty(simAreaFile) || !File.Exists(simAreaFile))
            {
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Scenario data directory must point to a valid file or directory"));
                return false;
            }

            var standardFilenames = new[]
            {
                "SimAreas.csv", "Species.csv", "PSM.csv"
            };
            var simAreaDirectory = Path.GetDirectoryName(simAreaFile);
            var files = Directory.GetFiles(simAreaDirectory, "*.csv");
            if (files.Length < 3)
            {
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating scenario data directory \"{0}\": Expected file(s) not found in this directory", simAreaDirectory));
                return false;
            }
            foreach (var file in files)
            {
                var curFile = Path.GetFileName(file).ToLower();
                var foundMatch = standardFilenames.Any(standardFile => curFile == standardFile.ToLower());
                if (foundMatch) continue;
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating scenario data directory \"{0}\": Expected file(s) not found in this directory", simAreaDirectory));
                return false;
            }
            ScenarioDataDirectory = simAreaDirectory;
            return true;
        }

        #endregion

        #region public OldBellhopSettings OldBellhopSettings { get; set; }

        public OldBellhopSettings OldBellhopSettings
        {
            get { return _bellhopSettings ?? (_bellhopSettings = new OldBellhopSettings()); }
            set
            {
                if (_bellhopSettings == value) return;
                _bellhopSettings = value;
                NotifyPropertyChanged(BellhopSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BellhopSettingsChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.OldBellhopSettings);
        OldBellhopSettings _bellhopSettings;

        #endregion

        #region public OldRAMSettings OldRAMSettings { get; set; }

        public OldRAMSettings OldRAMSettings
        {
            get { return _ramSettings ?? (_ramSettings = new OldRAMSettings()); }
            set
            {
                if (_ramSettings == value) return;
                _ramSettings = value;
                NotifyPropertyChanged(RamSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RamSettingsChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.OldRAMSettings);
        OldRAMSettings _ramSettings;

        #endregion

        #region public NAVOConfiguration NAVOConfiguration { get; set; }

        public NAVOConfiguration NAVOConfiguration
        {
            get { return _navoConfiguration ?? (_navoConfiguration = new NAVOConfiguration()); }
            set
            {
                if (_navoConfiguration == value) return;
                _navoConfiguration = value;
                NotifyPropertyChanged(NAVOConfigurationChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NAVOConfigurationChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.NAVOConfiguration);
        NAVOConfiguration _navoConfiguration;

        #endregion

        #region public List<string> ExperimentFiles { get; set; }
        // This list is maintained by the ESME Workbench.  When a new experiment is saved, the path to the experiment directory is added to this list
        // Periodically, the VerifyExperimentsStillExist() method is called, which will prune directories that no longer exist.

        public List<string> ExperimentFiles
        {
            get { return _experimentFiles ?? (_experimentFiles = new List<string>()); }
            set
            {
                if (_experimentFiles == value) return;
                _experimentFiles = value;
                NotifyPropertyChanged(ExperimentDirectoriesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExperimentDirectoriesChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.ExperimentFiles);
        List<string> _experimentFiles;

        /// <summary>
        /// Add the current experiment to the list for the Transmission Loss Calculator to keep track of
        /// </summary>
        /// <param name="curExperimentFile"></param>
        public void AddExperiment(string curExperimentFile)
        {
            if (ExperimentFiles.Any(experimentFile => experimentFile == curExperimentFile)) return;
            ExperimentFiles.Add(curExperimentFile);
            Save();
        }

        public void VerifyExperimentsStillExist()
        {
            //var tmpList = ExperimentFiles.Where(File.Exists).ToList();
            //ExperimentFiles.Clear();
            //ExperimentFiles.AddRange(tmpList);
            //Save();
        }

        #endregion

        #region public bool DisplayContoursOnTransmissionLoss { get; set; }

        public bool DisplayContoursOnTransmissionLoss
        {
            get { return _displayContoursOnTransmissionLoss; }
            set
            {
                if (_displayContoursOnTransmissionLoss == value) return;
                _displayContoursOnTransmissionLoss = value;
                NotifyPropertyChanged(DisplayContoursOnTransmissionLossChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DisplayContoursOnTransmissionLossChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.DisplayContoursOnTransmissionLoss);
        bool _displayContoursOnTransmissionLoss = true;

        #endregion

        #region public float TransmissionLossContourThreshold { get; set; }

        public float TransmissionLossContourThreshold
        {
            get { return _transmissionLossContourThreshold; }
            set
            {
                if (Math.Abs(_transmissionLossContourThreshold - value) < .01) return;
                _transmissionLossContourThreshold = value;
                NotifyPropertyChanged(TransmissionLossContourThresholdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossContourThresholdChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.TransmissionLossContourThreshold);
        float _transmissionLossContourThreshold = 120;

        #endregion

        #region public int MaxImportThreadCount { get; set; }

        public int MaxImportThreadCount
        {
            get { return _maxImportThreadCount; }
            set
            {
                if (_maxImportThreadCount == value) return;
                _maxImportThreadCount = value;
                NotifyPropertyChanged(MaxImportThreadCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxImportThreadCountChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.MaxImportThreadCount);
        int _maxImportThreadCount = 4;

        #endregion

        #region public List<PluginIdentifier> DefaultPluginIdentifiers { get; set; }
        // DefaultPluginIdentifiers should always come from the PluginManagerService.DefaultPluginIdentifiers
        // During application startup (initial deserialization of AppSettings, specificially), PluginManagerService has not yet been set.
        // Therefore, when PluginManagerService is null, we temporarily store DefaultPluginIdentifiers in _defaultPluginIdentifiers
        // When PluginManagerService gets set, PluginManagerService.DefaultPluginIdentifiers gets initialized from _defaultPluginIdentifiers
        public List<PluginIdentifier> DefaultPluginIdentifiers
        {
            get
            {
                return PluginManagerService == null ? _defaultPluginIdentifiers : PluginManagerService.DefaultPluginIdentifiers;
            }
            set
            {
                if (PluginManagerService == null) _defaultPluginIdentifiers = value;
                else PluginManagerService.DefaultPluginIdentifiers = value;
            }
        }

        List<PluginIdentifier> _defaultPluginIdentifiers;

        IPluginManagerService _pluginManagerService;

        // This is not serialized, but rather is set when the main application starts up (for ESME Workbench, that's in the MainViewModel
        // constructor).  The collection of default plugin identifiers is cached in _defaultPluginIdentifiers
        [XmlIgnore]
        public IPluginManagerService PluginManagerService
        {
            get { return _pluginManagerService; }
            set
            {
                _pluginManagerService = value;
                if (_pluginManagerService != null) _pluginManagerService.DefaultPluginIdentifiers = _defaultPluginIdentifiers;
            }
        }
        #endregion

        #region public string LocationDirectory { get; set; }

        public string LocationDirectory
        {
            get { return _locationDirectory; }
            set
            {
                if (_locationDirectory == value) return;
                _locationDirectory = value;
                NotifyPropertyChanged(LocationRootDirectoryChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs LocationRootDirectoryChangedEventArgs = ObservableHelper.CreateArgs<OldAppSettings>(x => x.LocationDirectory);
        string _locationDirectory;

        #endregion
    }

    public sealed class OldPluginSelection : ValidatingViewModel
    {
        #region public string DllFilename { get; set; }

        public string DllFilename
        {
            get { return _dllFilename; }
            set
            {
                if (_dllFilename == value) return;
                _dllFilename = value;
                NotifyPropertyChanged(DllFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DllFilenameChangedEventArgs = ObservableHelper.CreateArgs<OldPluginSelection>(x => x.DllFilename);
        string _dllFilename;

        #endregion

        #region public string ClassName { get; set; }

        public string ClassName
        {
            get { return _className; }
            set
            {
                if (_className == value) return;
                _className = value;
                NotifyPropertyChanged(ClassNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ClassNameChangedEventArgs = ObservableHelper.CreateArgs<OldPluginSelection>(x => x.ClassName);
        string _className;

        #endregion
    }

    public sealed class OldRAMSettings : ValidatingViewModel
    {
        public OldRAMSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldRAMSettings)o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldRAMSettings)o).DepthStepSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumRange",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldRAMSettings)o).MaximumRange;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldRAMSettings)o).RangeStepSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                         }
                );
        }
        public void SetDefaults()
        {
            MaximumDepth = 2000;
            DepthStepSize = 25;
            RangeStepSize = 50;
            MaximumRange = 100000;
        }

        #region public float MaximumDepth { get; set; }

        public float MaximumDepth
        {
            get { return _maximumDepth; }
            set
            {
                if (Math.Abs(_maximumDepth - value) < 0.0001) return;
                _maximumDepth = value;
                NotifyPropertyChanged(MaximumDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaximumDepthChangedEventArgs = ObservableHelper.CreateArgs<OldRAMSettings>(x => x.MaximumDepth);
        float _maximumDepth = 2000;

        #endregion

        #region public float DepthStepSize { get; set; }

        public float DepthStepSize
        {
            get { return _depthStepSize; }
            set
            {
                if (Math.Abs(_depthStepSize - value) < 0.0001) return;
                _depthStepSize = value;
                NotifyPropertyChanged(DepthCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<OldRAMSettings>(x => x.DepthStepSize);
        float _depthStepSize = 25;

        #endregion

        #region public float MaximumRange { get; set; }

        public float MaximumRange
        {
            get { return _maximumRange; }
            set
            {
                if (Math.Abs(_maximumRange - value) < 0.0001) return;
                _maximumRange = value;
                NotifyPropertyChanged(MaximumRangeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaximumRangeChangedEventArgs = ObservableHelper.CreateArgs<OldRAMSettings>(x => x.MaximumRange);
        float _maximumRange;

        #endregion

        #region public float RangeStepSize { get; set; }

        public float RangeStepSize
        {
            get { return _rangeStepSize; }
            set
            {
                if (Math.Abs(_rangeStepSize - value) < 0.0001) return;
                _rangeStepSize = value;
                NotifyPropertyChanged(RangeCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<OldRAMSettings>(x => x.RangeStepSize);
        float _rangeStepSize = 50;

        #endregion
    }

    public sealed class OldBellhopSettings : ValidatingViewModel
    {
        public OldBellhopSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldBellhopSettings)o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },

                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldBellhopSettings)o).RangeCellSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue, false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((OldBellhopSettings)o).DepthCellSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },

                                         });

        }

        #region public float MaximumDepth { get; set; }

        public float MaximumDepth
        {
            get { return _maximumDepth; }
            set
            {
                if (Math.Abs(_maximumDepth - value) < 0.0001) return;
                _maximumDepth = value;
                NotifyPropertyChanged(MaximumDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaximumDepthChangedEventArgs = ObservableHelper.CreateArgs<OldBellhopSettings>(x => x.MaximumDepth);
        float _maximumDepth = 2000;

        #endregion

        #region public float RangeCellSize { get; set; }

        public float RangeCellSize
        {
            get { return _rangeCellSize; }
            set
            {
                if (Math.Abs(_rangeCellSize - value) < 0.0001) return;
                _rangeCellSize = value;
                NotifyPropertyChanged(RangeCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<OldBellhopSettings>(x => x.RangeCellSize);
        float _rangeCellSize = 25;

        #endregion

        #region public float DepthCellSize { get; set; }

        public float DepthCellSize
        {
            get { return _depthCellSize; }
            set
            {
                if (Math.Abs(_depthCellSize - value) < 0.0001) return;
                _depthCellSize = value;
                NotifyPropertyChanged(DepthCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<OldBellhopSettings>(x => x.DepthCellSize);
        float _depthCellSize = 5;

        #endregion
    }
}