﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Xml.Serialization;
using C5;
using Cinch;
using ESME.Environment.NAVO;
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

        //FileSystemWatcher _fileSystemWatcher;

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
            if (CASSSettings != null) CASSSettings.SetDefaults();
            if (RAMSettings != null) RAMSettings.SetDefaults();
            if (NAVOConfiguration != null) NAVOConfiguration.SetDefaults();
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

        #region public string EnvironmentDatabaseDirectory { get; set; }

        static readonly PropertyChangedEventArgs EnvironmentDatabaseDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.EnvironmentDatabaseDirectory);
        string _environmentDatabaseDirectory;


        public string EnvironmentDatabaseDirectory
        {
            get { return _environmentDatabaseDirectory; }
            set
            {
                if (_environmentDatabaseDirectory == value) return;
                _environmentDatabaseDirectory = value;
                NotifyPropertyChanged(EnvironmentDatabaseDirectoryChangedEventArgs);
            }
        }

        #endregion

        #region public NAVOConfiguration NAVOConfiguration { get; set; }

        static readonly PropertyChangedEventArgs NAVOConfigurationChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.NAVOConfiguration);
        NAVOConfiguration _navoConfiguration;

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

        #endregion

        #region public bool IsNavyVersion { get; set; }

        public bool IsNavyVersion
        {
            get { return _isNavyVersion; }
            set
            {
                if (_isNavyVersion == value) return;
                _isNavyVersion = value;
                NotifyPropertyChanged(IsNavyVersionChangedEventArgs);
                NotifyPropertyChanged(NavyOptionsVisibilityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsNavyVersionChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.IsNavyVersion);
        bool _isNavyVersion = true;

        #region public Visibility NavyOptionsVisibility { get; set; }

        [XmlElement]
        public Visibility NavyOptionsVisibility
        {
            get { return _isNavyVersion ? Visibility.Visible : Visibility.Collapsed; }
        }

        [XmlIgnore]
        static readonly PropertyChangedEventArgs NavyOptionsVisibilityChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.NavyOptionsVisibility);

        #endregion

        #endregion

        #region public CASSSettings CASSSettings { get; set; }

        public CASSSettings CASSSettings
        {
            get { return _cassSettings ?? (_cassSettings = new CASSSettings()); }
            set
            {
                if (_cassSettings == value) return;
                _cassSettings = value;
                NotifyPropertyChanged(CASSSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.CASSSettings);
        CASSSettings _cassSettings;

        #endregion

        #region public REFMSSettings REFMSSettings { get; set; }

        public REFMSSettings REFMSSettings
        {
            get { return _refmsSettings ?? (_refmsSettings = new REFMSSettings()); }
            set
            {
                if (_refmsSettings == value) return;
                _refmsSettings = value;
                NotifyPropertyChanged(REFMSSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs REFMSSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.REFMSSettings);
        REFMSSettings _refmsSettings;

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

        #region public ScenarioSimulatorSettings ScenarioSimulatorSettings { get; set; }

        public ScenarioSimulatorSettings ScenarioSimulatorSettings
        {
            get { return _scenarioSimulatorSettings ?? (_scenarioSimulatorSettings = new ScenarioSimulatorSettings()); }
            set
            {
                if (_scenarioSimulatorSettings == value) return;
                _scenarioSimulatorSettings = value;
                NotifyPropertyChanged(ScenarioSimulatorSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScenarioSimulatorSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioSimulatorSettings);
        ScenarioSimulatorSettings _scenarioSimulatorSettings;

        #endregion

        #region public NAEMOTools NAEMOTools { get; set; }

        public NAEMOTools NAEMOTools
        {
            get { return _naemoTools ?? (_naemoTools = new NAEMOTools()); }
            set
            {
                if (_naemoTools == value) return;
                _naemoTools = value;
                NotifyPropertyChanged(NAEMOToolsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAEMOToolsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.NAEMOTools);
        NAEMOTools _naemoTools;

        #endregion

        // This list is maintained by the ESME WorkBench.  When a new experiment is saved, the path to the experiment directory is added to this list
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
    }

    public sealed class NAEMOTools : ValidatingViewModel
    {
        public NAEMOTools()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                        {
                                            new ValidationRule
                                                {
                                                    PropertyName = "RAMExecutable",
                                                    Description = "File must exist",
                                                    RuleDelegate = (o, r) =>
                                                                       {
                                                                           var ruleTarget = ((NAEMOTools) o).RAMExecutable;
                                                                           return File.Exists(ruleTarget);
                                                                       },
                                                },
                                            new ValidationRule
                                                {
                                                    PropertyName = "JavaExecutablePath",
                                                    Description = "File must exist",
                                                    RuleDelegate = (o, r) =>
                                                                       {
                                                                           var ruleTarget = ((NAEMOTools) o).JavaExecutablePath;
                                                                           return File.Exists(ruleTarget);
                                                                       },
                                                },
                                            new ValidationRule
                                                {
                                                    PropertyName = "NAEMOToolsDirectory",
                                                    Description = "Directory must exist and be nonempty",
                                                    RuleDelegate = (o, r) =>
                                                                       {
                                                                           var ruleTarget = ((NAEMOTools) o).NAEMOToolsDirectory;
                                                                           return (Directory.Exists(ruleTarget) && (Directory.GetFiles(ruleTarget).Length > 0));
                                                                       },
                                                },

                                 
                                 
                                 
                                        });
        }

        #region public string JavaExecutablePath { get; set; }

        public string JavaExecutablePath
        {
            get { return _javaExecutablePath; }
            set
            {
                if (_javaExecutablePath == value) return;
                _javaExecutablePath = value;
                NotifyPropertyChanged(JavaExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JavaExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<NAEMOTools>(x => x.JavaExecutablePath);
        string _javaExecutablePath;

        #endregion

        #region public string NAEMOToolsDirectory { get; set; }

        public string NAEMOToolsDirectory
        {
            get { return _naemoToolsDirectory; }
            set
            {
                if (_naemoToolsDirectory == value) return;
                _naemoToolsDirectory = value;
                NotifyPropertyChanged(NAEMOToolsDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAEMOToolsDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAEMOTools>(x => x.NAEMOToolsDirectory);
        string _naemoToolsDirectory;

        #endregion

        #region public string RAMExecutable { get; set; }

        public string RAMExecutable
        {
            get { return _rAMExecutable; }
            set
            {
                if (_rAMExecutable == value) return;
                _rAMExecutable = value;
                NotifyPropertyChanged(RAMExecutableChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RAMExecutableChangedEventArgs = ObservableHelper.CreateArgs<NAEMOTools>(x => x.RAMExecutable);
        string _rAMExecutable;

        #endregion

        [XmlIgnore]
        public string ScenarioEditorExecutablePath { get { return NAEMOTool("scenario-builder.jar"); } }

        [XmlIgnore]
        public string ReportGeneratorExecutablePath { get { return NAEMOTool("post-processor.jar"); } }

        [XmlIgnore]
        public string ExposureReportGeneratorExecutablePath { get { return NAEMOTool("range-complex.jar"); } }

        [XmlIgnore]
        public string RAMSupportJarFile { get { return NAEMOTool("ram-support.jar"); } }

        [XmlIgnore]
        public string ScenarioExecutablePath { get { return NAEMOTool("scene-sim.jar"); } }

        string NAEMOTool(string toolFileName) { return string.IsNullOrEmpty(NAEMOToolsDirectory) ? null : Path.Combine(NAEMOToolsDirectory, toolFileName); }
        
    }

    public sealed class CASSSettings : ValidatingViewModel
    {
        public CASSSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MaximumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).MaximumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DepthStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).DepthStepSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "RangeStepSize",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).RangeStepSize;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "PythonExecutablePath",
                                                     Description = "File must exist",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).PythonExecutablePath;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "CASSExecutablePath",
                                                     Description = "File must exist",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).CASSExecutablePath;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "PythonScriptPath",
                                                     Description = "File must exist",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((CASSSettings) o).PythonScriptPath;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                         });
        }

        public void SetDefaults()
        {
            if (CASSParameterFiles.Count == 0)
            {
                CASSParameterFiles.Add(new CASSTemplate
                {
                    FileName = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "base_template.cass"),
                    IsEnabled = true,
                    MatchString = "",
                });
                CASSParameterFiles.Add(new CASSTemplate
                {
                    FileName = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "fathometer_template.cass"),
                    IsEnabled = true,
                    MatchString = "Fathometer",
                });
            }
        }

        #region public bool GeneratePlotFiles { get; set; }

        public bool GeneratePlotFiles
        {
            get { return _generatePlotFiles; }
            set
            {
                if (_generatePlotFiles == value) return;
                _generatePlotFiles = value;
                NotifyPropertyChanged(GeneratePlotFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GeneratePlotFilesChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.GeneratePlotFiles);
        bool _generatePlotFiles;

        #endregion

        #region public bool GenerateBinaryFiles { get; set; }

        public bool GenerateBinaryFiles
        {
            get { return _generateBinaryFiles; }
            set
            {
                if (_generateBinaryFiles == value) return;
                _generateBinaryFiles = value;
                NotifyPropertyChanged(GenerateBinaryFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GenerateBinaryFilesChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.GenerateBinaryFiles);
        bool _generateBinaryFiles = true;

        #endregion

        #region public bool GeneratePressureFiles { get; set; }

        public bool GeneratePressureFiles
        {
            get { return _generatePressureFiles; }
            set
            {
                if (_generatePressureFiles == value) return;
                _generatePressureFiles = value;
                NotifyPropertyChanged(GeneratePressureFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GeneratePressureFilesChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.GeneratePressureFiles);
        bool _generatePressureFiles;

        #endregion

        #region public bool GenerateEigenrayFiles { get; set; }

        public bool GenerateEigenrayFiles
        {
            get { return _generateEigenrayFiles; }
            set
            {
                if (_generateEigenrayFiles == value) return;
                _generateEigenrayFiles = value;
                NotifyPropertyChanged(GenerateEigenrayFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GenerateEigenrayFilesChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.GenerateEigenrayFiles);
        bool _generateEigenrayFiles;

        #endregion

        #region public string PythonExecutablePath { get; set; }

        public string PythonExecutablePath
        {
            get { return _pythonExecutablePath; }
            set
            {
                if (_pythonExecutablePath == value) return;
                _pythonExecutablePath = value;
                NotifyPropertyChanged(PythonExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PythonExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.PythonExecutablePath);
        string _pythonExecutablePath;

        #endregion

        #region public string PythonScriptPath { get; set; }

        public string PythonScriptPath
        {
            get { return _pythonScriptPath; }
            set
            {
                if (_pythonScriptPath == value) return;
                _pythonScriptPath = value;
                NotifyPropertyChanged(PythonScriptPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PythonScriptPathChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.PythonScriptPath);
        string _pythonScriptPath;

        #endregion

        #region public string CASSExecutablePath { get; set; }

        public string CASSExecutablePath
        {
            get { return _cASSExecutablePath; }
            set
            {
                if (_cASSExecutablePath == value) return;
                _cASSExecutablePath = value;
                NotifyPropertyChanged(CASSExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.CASSExecutablePath);
        string _cASSExecutablePath;

        #endregion

        #region public float MaximumDepth { get; set; }

        public float MaximumDepth
        {
            get { return _maximumDepth; }
            set
            {
                if (_maximumDepth == value) return;
                _maximumDepth = value;
                NotifyPropertyChanged(MaximumDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaximumDepthChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.MaximumDepth);
        float _maximumDepth = 2000.0f;

        #endregion

        #region public float DepthStepSize { get; set; }

        public float DepthStepSize
        {
            get { return _depthStepSize; }
            set
            {
                if (_depthStepSize == value) return;
                _depthStepSize = value;
                NotifyPropertyChanged(DepthStepSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthStepSizeChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.DepthStepSize);
        float _depthStepSize = 25.0f;

        #endregion

        #region public float RangeStepSize { get; set; }

        public float RangeStepSize
        {
            get { return _rangeStepSize; }
            set
            {
                if (_rangeStepSize == value) return;
                _rangeStepSize = value;
                NotifyPropertyChanged(RangeStepSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeStepSizeChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.RangeStepSize);
        float _rangeStepSize = 25.0f;

        #endregion

        #region public ObservableCollection<CASSTemplate> CASSParameterFiles { get; set; }

        public ObservableCollection<CASSTemplate> CASSParameterFiles
        {
            get { return _cassParameterFiles ?? (_cassParameterFiles = new ObservableCollection<CASSTemplate>()); }
            set
            {
                if (_cassParameterFiles == value) return;
                if (_cassParameterFiles != null) _cassParameterFiles.CollectionChanged -= CASSParameterFilesCollectionChanged;
                _cassParameterFiles = value;
                if (_cassParameterFiles != null) _cassParameterFiles.CollectionChanged += CASSParameterFilesCollectionChanged;
                NotifyPropertyChanged(CASSParameterFilesChangedEventArgs);
            }
        }

        void CASSParameterFilesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(CASSParameterFilesChangedEventArgs); }
        static readonly PropertyChangedEventArgs CASSParameterFilesChangedEventArgs = ObservableHelper.CreateArgs<CASSSettings>(x => x.CASSParameterFiles);
        ObservableCollection<CASSTemplate> _cassParameterFiles;

        #endregion

        #region AddCASSParameterFileCommand

        public SimpleCommand<object, object> AddCASSParameterFileCommand
        {
            get { return _addCASSParameterFile ?? (_addCASSParameterFile = new SimpleCommand<object, object>(delegate { CASSParameterFiles.Add(new CASSTemplate() { }); })); }
        }

        SimpleCommand<object, object> _addCASSParameterFile;

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
                if (_maximumDepth == value) return;
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
                if (_depthStepSize == value) return;
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
                if (_maximumRange == value) return;
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
                if (_rangeStepSize == value) return;
                _rangeStepSize = value;
                NotifyPropertyChanged(RangeCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeCellSizeChangedEventArgs = ObservableHelper.CreateArgs<RAMSettings>(x => x.RangeStepSize);
        float _rangeStepSize = 50;

        #endregion
    }

    public sealed class REFMSSettings : ValidatingViewModel
    {
        public REFMSSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
                                         {
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DefaultMinimumRange",
                                                     Description = "Must be greater than zero and less than the default maximum range",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings) o).DefaultMinimumRange;
                                                                            var defaultMaximumRange = ((REFMSSettings)o).DefaultMaximumRange;
                                                                            return RangeCheck(ruleTarget, 0, defaultMaximumRange, false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DefaultMaximumRange",
                                                     Description = "Must be greater than the default minimum range",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var defaultMinimumRange = ((REFMSSettings)o).DefaultMinimumRange;
                                                                            var ruleTarget = ((REFMSSettings) o).DefaultMaximumRange;
                                                                            return ruleTarget > defaultMinimumRange;
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "DefaultNumberOfRangePoints",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings) o).DefaultNumberOfRangePoints;
                                                                            return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "MinimumDepth",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings) o).MinimumDepth;
                                                                            return RangeCheck(ruleTarget,0,float.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "NumberOfDepthPoints",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings) o).NumberOfDepthPoints;
                                                                            return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "SPLCutoffSingleSource",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings)o).SPLCutoffSingleSource;
                                                                            return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "SPLCutoffMultiSource",
                                                     Description = "Must be positive",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings)o).SPLCutoffMultiSource;
                                                                            return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                                                        },
                                                 },
                                             new ValidationRule
                                                 {
                                                     PropertyName = "REFMSExecutablePath",
                                                     Description = "File must exist.",
                                                     RuleDelegate = (o, r) =>
                                                                        {
                                                                            var ruleTarget = ((REFMSSettings) o).REFMSExecutablePath;
                                                                            return File.Exists(ruleTarget);
                                                                        },
                                                 },
                                         });
        }

        #region public float DefaultMinimumRange { get; set; }

        public float DefaultMinimumRange
        {
            get { return _defaultMinimumRange; }
            set
            {
                if (_defaultMinimumRange == value) return;
                _defaultMinimumRange = value;
                NotifyPropertyChanged(DefaultMinimumRangeChangedEventArgs);
                NotifyPropertyChanged(DefaultMaximumRangeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DefaultMinimumRangeChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.DefaultMinimumRange);
        float _defaultMinimumRange = 15.24f;

        #endregion

        #region public float DefaultMaximumRange { get; set; }

        public float DefaultMaximumRange
        {
            get { return _defaultMaximumRange; }
            set
            {
                if (_defaultMaximumRange == value) return;
                _defaultMaximumRange = value;
                NotifyPropertyChanged(DefaultMaximumRangeChangedEventArgs);
                NotifyPropertyChanged(DefaultMinimumRangeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DefaultMaximumRangeChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.DefaultMaximumRange);
        float _defaultMaximumRange = 1609.344f;

        #endregion

        #region public int DefaultNumberOfRangePoints { get; set; }

        public int DefaultNumberOfRangePoints
        {
            get { return _defaultNumberOfRangePoints; }
            set
            {
                if (_defaultNumberOfRangePoints == value) return;
                _defaultNumberOfRangePoints = value;
                NotifyPropertyChanged(DefaultNumberOfRangePointsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DefaultNumberOfRangePointsChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.DefaultNumberOfRangePoints);
        int _defaultNumberOfRangePoints = 13;

        #endregion

        #region public float MinimumDepth { get; set; }

        public float MinimumDepth
        {
            get { return _minimumDepth; }
            set
            {
                if (_minimumDepth == value) return;
                _minimumDepth = value;
                NotifyPropertyChanged(MinimumDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MinimumDepthChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.MinimumDepth);
        float _minimumDepth = 0.478536f;

        #endregion

        #region public int NumberOfDepthPoints { get; set; }

        public int NumberOfDepthPoints
        {
            get { return _numberOfDepthPoints; }
            set
            {
                if (_numberOfDepthPoints == value) return;
                _numberOfDepthPoints = value;
                NotifyPropertyChanged(NumberOfDepthPointsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NumberOfDepthPointsChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.NumberOfDepthPoints);
        int _numberOfDepthPoints = 16;

        #endregion

        #region public float SPLCutoffSingleSource { get; set; }

        public float SPLCutoffSingleSource
        {
            get { return _splCutoffSingleSource; }
            set
            {
                if (_splCutoffSingleSource == value) return;
                _splCutoffSingleSource = value;
                NotifyPropertyChanged(SPLCutoffSingleSourceChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SPLCutoffSingleSourceChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.SPLCutoffSingleSource);
        float _splCutoffSingleSource = 200;

        #endregion

        #region public float SPLCutoffMultiSource { get; set; }

        public float SPLCutoffMultiSource
        {
            get { return _splCutoffMultiSource; }
            set
            {
                if (_splCutoffMultiSource == value) return;
                _splCutoffMultiSource = value;
                NotifyPropertyChanged(SPLCutoffMultiSourceChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SPLCutoffMultiSourceChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.SPLCutoffMultiSource);
        float _splCutoffMultiSource;

        #endregion

        #region public string SELCutoffLine { get; set; }

        public string SELCutoffLine
        {
            get { return _selCutoffLine; }
            set
            {
                if (_selCutoffLine == value) return;
                _selCutoffLine = value;
                NotifyPropertyChanged(SELCutoffLineChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SELCutoffLineChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.SELCutoffLine);
        string _selCutoffLine = "163  800  171  1100  183";

        #endregion

        #region public string REFMSExecutablePath { get; set; }

        public string REFMSExecutablePath
        {
            get { return _refmsExecutablePath; }
            set
            {
                if (_refmsExecutablePath == value) return;
                _refmsExecutablePath = value;
                NotifyPropertyChanged(REFMSExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs REFMSExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.REFMSExecutablePath);
        string _refmsExecutablePath;

        #endregion
    }

    public sealed class ScenarioSimulatorSettings : ValidatingViewModel
    {
        public ScenarioSimulatorSettings()
        {
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                    {
                        PropertyName = "Iterations",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).Iterations;
                                               return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                           },
                    },
                new ValidationRule
                    {
                        PropertyName = "DecibelCutoff",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).DecibelCutoff;
                                               return RangeCheck(ruleTarget,0,double.MaxValue,false);
                                           },
                    },
                new ValidationRule
                    {
                        PropertyName = "ParallelSimulations",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).ParallelSimulations;
                                               return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                           },
                    },
                new ValidationRule
                    {
                        PropertyName = "OutputBufferSize",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).OutputBufferSize;
                                               return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                           },
                    },
                new ValidationRule
                    {
                        PropertyName = "SpeciesFileSize",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).SpeciesFileSize;
                                               return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                           },
                    },
                new ValidationRule
                    {
                        PropertyName = "CASSFileSize",
                        Description = "Must be positive",
                        RuleDelegate = (o, r) =>
                                           {
                                               var ruleTarget = ((ScenarioSimulatorSettings) o).CASSFileSize;
                                               return RangeCheck(ruleTarget,0,int.MaxValue,false);
                                           },
                    },
            });
           
            //these are log4j levels http://en.wikipedia.org/wiki/Log4j#Log_level
            LogLevels = new List<string>
                            {
                                "Off",
                                "Severe Errors",
                                "Errors",
                                "Warnings",
                                "Info",
                                "Debug",
                                "Trace",
                            };
        }

        #region public bool IsRandomized { get; set; }

        public bool IsRandomized
        {
            get { return _isRandomized; }
            set
            {
                if (_isRandomized == value) return;
                _isRandomized = value;
                NotifyPropertyChanged(IsRandomizedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsRandomizedChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.IsRandomized);
        bool _isRandomized;

        #endregion

        #region public int Iterations { get; set; }

        public int Iterations
        {
            get { return _iterations; }
            set
            {
                if (_iterations == value) return;
                _iterations = value;
                NotifyPropertyChanged(IterationsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IterationsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.Iterations);
        int _iterations = 1;

        #endregion

        #region public bool OptimizeBuffer { get; set; }

        public bool OptimizeBuffer
        {
            get { return _optimizeBuffer; }
            set
            {
                if (_optimizeBuffer == value) return;
                _optimizeBuffer = value;
                NotifyPropertyChanged(OptimizeBufferChangedEventArgs);
                NotifyPropertyChanged(NotOptimizeBufferChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OptimizeBufferChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.OptimizeBuffer);
        bool _optimizeBuffer = true;

        #endregion

        #region public bool NotOptimizeBuffer { get; set; }

        public bool NotOptimizeBuffer
        {
            get { return !_optimizeBuffer; }
        }

        static readonly PropertyChangedEventArgs NotOptimizeBufferChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.NotOptimizeBuffer);

        #endregion

        #region public int OutputBufferSize { get; set; }

        public int OutputBufferSize
        {
            get { return _outputBufferSize; }
            set
            {
                if (_outputBufferSize == value) return;
                _outputBufferSize = value;
                NotifyPropertyChanged(OutputBufferSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OutputBufferSizeChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.OutputBufferSize);
        int _outputBufferSize = 16;

        #endregion

        #region public bool CreateSimulationLogfile { get; set; }

        public bool CreateSimulationLogfile
        {
            get { return _createSimulationLogfile; }
            set
            {
                if (_createSimulationLogfile == value) return;
                _createSimulationLogfile = value;
                NotifyPropertyChanged(CreateSimulationLogfileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreateSimulationLogfileChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.CreateSimulationLogfile);
        bool _createSimulationLogfile;

        #endregion

        #region public double DecibelCutoff { get; set; }

        public double DecibelCutoff
        {
            get { return _decibelCutoff; }
            set
            {
                if (_decibelCutoff == value) return;
                _decibelCutoff = value;
                NotifyPropertyChanged(DecibelCutoffChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DecibelCutoffChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.DecibelCutoff);
        double _decibelCutoff = 0.01;

        #endregion

        #region public int SimOutputLevel { get; set; }

        public int SimOutputLevel
        {
            get { return _simOutputLevel; }
            set
            {
                if (_simOutputLevel == value) return;
                _simOutputLevel = value;
                NotifyPropertyChanged(LogLevelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LogLevelChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.SimOutputLevel);
        int _simOutputLevel = 5;

        #endregion

        #region public int ParallelSimulations { get; set; }

        public int ParallelSimulations
        {
            get { return _parallelSimulations; }
            set
            {
                if (_parallelSimulations == value) return;
                _parallelSimulations = value;
                NotifyPropertyChanged(ParallelSimulationsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ParallelSimulationsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.ParallelSimulations);
        int _parallelSimulations = System.Environment.ProcessorCount - 1;

        #endregion

        #region public int SpeciesFileSize { get; set; }

        public int SpeciesFileSize
        {
            get { return _speciesFileSize; }
            set
            {
                if (_speciesFileSize == value) return;
                _speciesFileSize = value;
                NotifyPropertyChanged(SpeciesFileSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpeciesFileSizeChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.SpeciesFileSize);
        int _speciesFileSize = 8192;

        #endregion

        #region public int CASSFileSize { get; set; }

        public int CASSFileSize
        {
            get { return _cASSFileSize; }
            set
            {
                if (_cASSFileSize == value) return;
                _cASSFileSize = value;
                NotifyPropertyChanged(CASSFileSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSFileSizeChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.CASSFileSize);
        int _cASSFileSize = 1024;

        #endregion

        #region public bool ReadAllMammals { get; set; }

        public bool ReadAllMammals
        {
            get { return _readAllMammals; }
            set
            {
                if (_readAllMammals == value) return;
                _readAllMammals = value;
                NotifyPropertyChanged(ReadAllMammalsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReadAllMammalsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.ReadAllMammals);
        bool _readAllMammals = true;

        #endregion

        #region public bool ClipOutsideFootprint { get; set; }

        public bool ClipOutsideFootprint
        {
            get { return _clipOutsideFootprint; }
            set
            {
                if (_clipOutsideFootprint == value) return;
                _clipOutsideFootprint = value;
                NotifyPropertyChanged(ClipOutsideFootprintChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ClipOutsideFootprintChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.ClipOutsideFootprint);
        bool _clipOutsideFootprint = true;

        #endregion

        [XmlIgnore]
        public List<string> LogLevels { get; private set; }

      
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
                if (_maximumDepth == value) return;
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
                if (_rangeCellSize == value) return;
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
                if (_depthCellSize == value) return;
                _depthCellSize = value;
                NotifyPropertyChanged(DepthCellSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthCellSizeChangedEventArgs = ObservableHelper.CreateArgs<BellhopSettings>(x => x.DepthCellSize);
        float _depthCellSize = 5;

        #endregion
    }
}