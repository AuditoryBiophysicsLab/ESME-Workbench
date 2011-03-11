using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Utility;

namespace ESME.Data
{
    [Serializable]
    public class AppSettings : SerializableData<AppSettings>
    {
        static Type[] _referencedTypes;

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

        public AppSettings()
        {
            FileName = AppSettingsFile;
        }

        public AppSettings(AppSettings that) : this() { CopyFrom(that); }

        public static Type[] ReferencedTypes
        {
            get
            {
                return _referencedTypes ?? (_referencedTypes = new[]
                                                               {
                                                                   typeof (NAVOConfiguration)
                                                               });
            }
        }

        public void Save() { Save(FileName, ReferencedTypes); }

        public void Reload() { Reload(ReferencedTypes); }

        #region public string ScenarioEditorExecutablePath { get; set; }

        static readonly PropertyChangedEventArgs ScenarioEditorExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioEditorExecutablePath);
        string _scenarioEditorExecutablePath;

        public string ScenarioEditorExecutablePath
        {
            get { return _scenarioEditorExecutablePath; }
            set
            {
                if (_scenarioEditorExecutablePath == value) return;
                _scenarioEditorExecutablePath = value;
                NotifyPropertyChanged(ScenarioEditorExecutablePathChangedEventArgs);
            }
        }

        #endregion

        #region public string ReportGeneratorExecutablePath { get; set; }

        public string ReportGeneratorExecutablePath
        {
            get { return _reportGeneratorExecutablePath; }
            set
            {
                if (_reportGeneratorExecutablePath == value) return;
                _reportGeneratorExecutablePath = value;
                NotifyPropertyChanged(ReportGeneratorExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReportGeneratorExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ReportGeneratorExecutablePath);
        string _reportGeneratorExecutablePath;

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
            get { return _navoConfiguration ?? (_navoConfiguration = new NAVOConfiguration());}
            set
            {
                if (_navoConfiguration == value) return;
                _navoConfiguration = value;
                NotifyPropertyChanged(NAVOConfigurationChangedEventArgs);
            }
        }

        #endregion

        #region public bool UseOAMLDataSources { get; set; }

        public bool UseOAMLDataSources
        {
            get { return _useOAMLDataSources; }
            set
            {
                if (_useOAMLDataSources == value) return;
                _useOAMLDataSources = value;
                UseESMEDataSources = !value;
                NotifyPropertyChanged(UseOAMLDataSourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseOAMLDataSourcesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.UseOAMLDataSources);
        bool _useOAMLDataSources = true;

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
        
        #region public bool UseESMEDataSources { get; set; }

        public bool UseESMEDataSources
        {
            get { return _useESMEDataSources; }
            set
            {
                if (_useESMEDataSources == value) return;
                _useESMEDataSources = value;
                UseOAMLDataSources = !value;
                NotifyPropertyChanged(UseESMEDataSourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseESMEDataSourcesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.UseESMEDataSources);
        bool _useESMEDataSources;

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

        #region public RamSettings RamSettings { get; set; }

        public RamSettings RamSettings
        {
            get { return _ramSettings ?? (_ramSettings = new RamSettings()); }
            set
            {
                if (_ramSettings == value) return;
                _ramSettings = value;
                NotifyPropertyChanged(RamSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RamSettingsChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.RamSettings);
        RamSettings _ramSettings;

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
            var tmpList = ExperimentFiles.Where(File.Exists).ToList();
            ExperimentFiles.Clear();
            ExperimentFiles.AddRange(tmpList);
            Save();
        }

        #endregion
    }

    public class CASSSettings : INotifyPropertyChanged
    {
        public void SetDefaultCASSParameterFiles()
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

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }

    public class REFMSSettings : INotifyPropertyChanged
    {
        #region public bool ScaleI { get; set; }

        public bool ScaleI
        {
            get { return _scaleI; }
            set
            {
                if (_scaleI == value) return;
                _scaleI = value;
                NotifyPropertyChanged(ScaleIChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ScaleIChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.ScaleI);
        bool _scaleI;

        #endregion

        #region public float DefaultMinimumRange { get; set; }

        public float DefaultMinimumRange
        {
            get { return _defaultMinimumRange; }
            set
            {
                if (_defaultMinimumRange == value) return;
                _defaultMinimumRange = value;
                NotifyPropertyChanged(DefaultMinimumRangeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DefaultMinimumRangeChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.DefaultMinimumRange);
        float _defaultMinimumRange = 50f;

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
            }
        }

        static readonly PropertyChangedEventArgs DefaultMaximumRangeChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.DefaultMaximumRange);
        float _defaultMaximumRange = 5280f;

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
        float _minimumDepth = 1.57f;

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

        #region public string REFMSExecutablePath { get; set; }

        public string REFMSExecutablePath
        {
            get { return _rEFMSExecutablePath; }
            set
            {
                if (_rEFMSExecutablePath == value) return;
                _rEFMSExecutablePath = value;
                NotifyPropertyChanged(REFMSExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs REFMSExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<REFMSSettings>(x => x.REFMSExecutablePath);
        string _rEFMSExecutablePath;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }

    public class ScenarioSimulatorSettings : INotifyPropertyChanged
    {
        #region public string ExecutablePath { get; set; }

        public string ExecutablePath
        {
            get { return _executablePath; }
            set
            {
                if (_executablePath == value) return;
                _executablePath = value;
                NotifyPropertyChanged(ExecutablePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorSettings>(x => x.ExecutablePath);
        string _executablePath;

        #endregion

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
        int _iterations;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }

    public class BellhopSettings : INotifyPropertyChanged
    {
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

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }

    public class RamSettings : INotifyPropertyChanged
    {
        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }
}