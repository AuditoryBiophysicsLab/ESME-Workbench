using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Aspects;
using HRC.Collections;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Data
{
    [Serializable, NotifyPropertyChanged]
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

        public void Save()
        {
            StaticXmlSerializer.Save(AppSettingsFile, this);
        }

        public static AppSettings Load()
        {
            return (AppSettings)StaticXmlSerializer.Load(AppSettingsFile, typeof(AppSettings));
        }

        public static AppSettings Load(string fileName)
        {
            return (AppSettings)StaticXmlSerializer.Load(fileName, typeof(AppSettings));
        }

        [Initialize]
        public SerializableDictionary<string, string> OpenFileServiceDirectories { get; set; }

#if false
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
                var foundMatch = standardFilenames.Any(standardFile => curFile == standardFile.ToLower());
                if (foundMatch) continue;
                if (messageBoxService != null) messageBoxService.ShowError(string.Format("Error validating scenario data directory \"{0}\": Expected file(s) not found in this directory", simAreaDirectory));
                return false;
            }
            ScenarioDataDirectory = simAreaDirectory;
            return true;
        }

        #endregion
#endif

        [Initialize]
        public BellhopSettings BellhopSettings { get; set; }

        [Initialize]
        public RAMSettings RAMSettings { get; set; }

        [Initialize]
        public NAVOConfiguration NAVOConfiguration { get; set; }
#if false
        #region public List<string> ExperimentFiles { get; set; }
#if UseAspects
        [Initialize]
        public List<string> ExperimentFiles { get; set; }
#else
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

        static readonly PropertyChangedEventArgs ExperimentDirectoriesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ExperimentFiles);
        List<string> _experimentFiles;
#endif

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
#endif

        [Initialize(true)]
        public bool DisplayContoursOnTransmissionLoss { get; set; }

        [Initialize(120f)]
        public float TransmissionLossContourThreshold { get; set; }

        [Initialize(-1)]
        public int MaxImportThreadCount { get; set; }

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

        public string DatabaseDirectory { get; set; }
    }

    [NotifyPropertyChanged]
    public sealed class PluginSelection : ValidatingViewModel
    {
        public string DllFilename { get; set; }

        public string ClassName { get; set; }
    }

    [NotifyPropertyChanged]
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
                                         }
                );
        }

        [Initialize(2000f)]
        public float MaximumDepth { get; set; }

        [Initialize(25f)]
        public float DepthStepSize { get; set; }

        [Initialize(100000f)]
        public float MaximumRange { get; set; }

        [Initialize(50f)]
        public float RangeStepSize { get; set; }
    }

    [NotifyPropertyChanged]
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

        [Initialize(2000f)]
        public float MaximumDepth { get; set; }

        [Initialize(25f)]
        public float RangeCellSize { get; set; }

        [Initialize(25f)]
        public float DepthCellSize { get; set; }
    }
}