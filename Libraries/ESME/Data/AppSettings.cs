using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
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
    public class AppSettings : ValidatingViewModel
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
          if (RAMSettings != null) RAMSettings.SetDefaults();
          if (DefaultPluginConfigurations == null || DefaultPluginConfigurations.Count == 0) SetDefaultPluginConfiguration();
        }

        public void Save()
        {
            var serializer = new XmlSerializer<AppSettings> { Data = this };
            serializer.Save(AppSettingsFile, ReferencedTypes);
        }

        public void Save(List<Type> referencedTypes)
        {
            if (referencedTypes == null) referencedTypes = ReferencedTypes;
            else referencedTypes.AddRange(ReferencedTypes);
            var serializer = new XmlSerializer<AppSettings> { Data = this };
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

        static readonly PropertyChangedEventArgs OpenFileServiceDirectoriesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.OpenFileServiceDirectories);
        SerializableDictionary<string, string> _openFileServiceDirectories;

        #endregion

        #region public string ScenarioDataDirectory { get; set; }

        static readonly PropertyChangedEventArgs ScenarioDataDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioDataDirectory);
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
                var foundMatch = false;
                foreach (var standardFile in standardFilenames)
                    if (curFile == standardFile.ToLower())
                    {
                        foundMatch = true;
                        break;
                    }
                if (foundMatch) continue;
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating scenario data directory \"{0}\": Expected file(s) not found in this directory", simAreaDirectory));
                return false;
            }
            ScenarioDataDirectory = simAreaDirectory;
            return true;
        }

        #endregion

        #region public BellhopSettings BellhopSettings { get; set; }

        public BellhopSettings BellhopSettings
        {
            get { return _bellhopSettings ?? (_bellhopSettings = new BellhopSettings()); }
            set
            {
                if (_bellhopSettings == value) return;
                _bellhopSettings = value;
                NotifyPropertyChanged(BellhopSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BellhopSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.BellhopSettings);
        BellhopSettings _bellhopSettings;

        #endregion

        #region public RAMSettings RAMSettings { get; set; }

        public RAMSettings RAMSettings
        {
            get { return _ramSettings ?? (_ramSettings = new RAMSettings()); }
            set
            {
                if (_ramSettings == value) return;
                _ramSettings = value;
                NotifyPropertyChanged(RamSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RamSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.RAMSettings);
        RAMSettings _ramSettings;

        #endregion

        #region public NAVOConfiguration NAVOConfiguration { get; set; }

        public NAVOConfiguration NAVOConfiguration
        {
            get { return _nAVOConfiguration ?? (_nAVOConfiguration = new NAVOConfiguration()); }
            set
            {
                if (_nAVOConfiguration == value) return;
                _nAVOConfiguration = value;
                NotifyPropertyChanged(NAVOConfigurationChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NAVOConfigurationChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.NAVOConfiguration);
        NAVOConfiguration _nAVOConfiguration;

        #endregion

        
        // This list is maintained by the ESME Workbench.  When a new experiment is saved, the path to the experiment directory is added to this list
        // Periodically, the VerifyExperimentsStillExist() method is called, which will prune directories that no longer exist.
        #region public List<string> ExperimentFiles { get; set; }

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

        static readonly PropertyChangedEventArgs ExperimentDirectoriesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ExperimentFiles);
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

        static readonly PropertyChangedEventArgs DisplayContoursOnTransmissionLossChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.DisplayContoursOnTransmissionLoss);
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

        static readonly PropertyChangedEventArgs TransmissionLossContourThresholdChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.TransmissionLossContourThreshold);
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

        static readonly PropertyChangedEventArgs MaxImportThreadCountChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.MaxImportThreadCount);
        int _maxImportThreadCount = 4;

        #endregion

        #region public List<DefaultPluginConfiguration> DefaultPluginConfigurations { get; set; }
        public List<DefaultPluginConfiguration> DefaultPluginConfigurations
        {
            get { return _defaultPluginConfigurations; }
            set { _defaultPluginConfigurations = value; }
        }
        List<DefaultPluginConfiguration> _defaultPluginConfigurations;

        void SetDefaultPluginConfiguration()
        {
            DefaultPluginConfigurations = new List<DefaultPluginConfiguration>
            {
                new DefaultPluginConfiguration
                {
                    PluginType = PluginType.EnvironmentalDataSource,
                    PluginSubtype = PluginSubtype.Wind,
                    Type = typeof (NoWindData).ToString(),
                },
                new DefaultPluginConfiguration
                {
                    PluginType = PluginType.EnvironmentalDataSource,
                    PluginSubtype = PluginSubtype.SoundSpeed,
                    Type = typeof (NoSoundSpeedData).ToString(),
                },
                new DefaultPluginConfiguration
                {
                    PluginType = PluginType.EnvironmentalDataSource,
                    PluginSubtype = PluginSubtype.Sediment,
                    Type = typeof (NoSedimentData).ToString(),
                },
                new DefaultPluginConfiguration
                {
                    PluginType = PluginType.EnvironmentalDataSource,
                    PluginSubtype = PluginSubtype.Bathymetry,
                    Type = typeof (NoBathymetryData).ToString(),
                }

            };
        }
        #endregion
    }

    public sealed class PluginSelection : ValidatingViewModel
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

        static readonly PropertyChangedEventArgs DllFilenameChangedEventArgs = ObservableHelper.CreateArgs<PluginSelection>(x => x.DllFilename);
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

        static readonly PropertyChangedEventArgs ClassNameChangedEventArgs = ObservableHelper.CreateArgs<PluginSelection>(x => x.ClassName);
        string _className;

        #endregion

    }

    public sealed class RAMSettings : ValidatingViewModel
    {
        public RAMSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).DepthStepSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumRange",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings)o).MaximumRange;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).RangeStepSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
#if false
                                             new ValidationRule
                                                 {
                                                     PropertyName = "SpeedDial",
                                                     Description = "Must be an integer",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).SpeedDial;
                                                                            return RangeCheck(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "SSPUnits",
                                                     Description = "Must be an integer",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).SSPUnits;
                                                                            return RangeCheck(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "CASSLevel",
                                                     Description = "Must be an integer",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).CASSLevel;
                                                                            return RangeCheck(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "BathymetryMetric",
                                                     Description = "Must be an integer",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((RAMSettings) o).BathymetryMetric;
                                                                            return RangeCheck(ruleTarget);
                                                                        },
                                                 },
#endif
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

        static readonly PropertyChangedEventArgs MaximumDepthChangedEventArgs = ObservableHelper.CreateArgs<RAMSettings>(x => x.MaximumDepth);
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

        static readonly PropertyChangedEventArgs DepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<RAMSettings>(x => x.DepthStepSize);
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

        static readonly PropertyChangedEventArgs MaximumRangeChangedEventArgs = ObservableHelper.CreateArgs<RAMSettings>(x => x.MaximumRange);
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

        static readonly PropertyChangedEventArgs RangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<RAMSettings>(x => x.RangeStepSize);
        float _rangeStepSize = 50;

        #endregion
    }

    public sealed class BellhopSettings : ValidatingViewModel
    {
        public BellhopSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((BellhopSettings)o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },

                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget =((BellhopSettings) o).RangeCellSize;
                                                                            return RangeCheck(ruleTarget, 0,float.MaxValue, false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthCellSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((BellhopSettings) o).DepthCellSize;
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

        static readonly PropertyChangedEventArgs MaximumDepthChangedEventArgs = ObservableHelper.CreateArgs<BellhopSettings>(x => x.MaximumDepth);
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

        static readonly PropertyChangedEventArgs RangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<BellhopSettings>(x => x.RangeCellSize);
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

        static readonly PropertyChangedEventArgs DepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<BellhopSettings>(x => x.DepthCellSize);
        float _depthCellSize = 5;

        #endregion
    }
}