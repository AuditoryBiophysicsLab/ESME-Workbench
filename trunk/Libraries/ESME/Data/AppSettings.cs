using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
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
            if (NAVOConfiguration == null) NAVOConfiguration = new NAVOConfiguration();
            if (CASSTemplates == null) CASSTemplates = new ObservableCollection<CASSTemplate>();
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

        public void SetDefaultCASSTemplates()
        {
            if (CASSTemplates.Count == 0)
            {
                CASSTemplates.Add(new CASSTemplate
                {
                    FileName = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "base_template.cass"),
                    IsEnabled = true,
                    MatchString = "",
                });
                CASSTemplates.Add(new CASSTemplate
                {
                    FileName = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "fathometer_template.cass"),
                    IsEnabled = true,
                    MatchString = "Fathometer",
                });
            }
        }

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
            get { return _navoConfiguration; }
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
        bool _useOAMLDataSources;

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

        #region AddCASSTemplateCommand

        public SimpleCommand<object, object> AddCASSTemplateCommand
        {
            get { return _addCASSTemplate ?? (_addCASSTemplate = new SimpleCommand<object, object>(delegate { CASSTemplates.Add(new CASSTemplate(){});})); }
        }

        SimpleCommand<object, object> _addCASSTemplate;

        #endregion


        #region public ObservableCollection<CASSTemplate> CASSTemplates { get; set; }

        public ObservableCollection<CASSTemplate> CASSTemplates
        {
            get { return _cassTemplates; }
            set
            {
                if (_cassTemplates == value) return;
                if (_cassTemplates != null) _cassTemplates.CollectionChanged -= CASSTemplatesCollectionChanged;
                _cassTemplates = value;
                if (_cassTemplates != null) _cassTemplates.CollectionChanged += CASSTemplatesCollectionChanged;
                NotifyPropertyChanged(CASSTemplatesChangedEventArgs);
            }
        }

        void CASSTemplatesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(CASSTemplatesChangedEventArgs); }
        static readonly PropertyChangedEventArgs CASSTemplatesChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.CASSTemplates);
        ObservableCollection<CASSTemplate> _cassTemplates;

        #endregion


        // This list is maintained by the ESME WorkBench.  When a new experiment is saved, the path to the experiment directory is added to this list
        // Periodically, the VerifyExperimentsStillExist() method is called, which will prune directories that no longer exist.
        #region public List<string> ExperimentFiles { get; set; }

        public List<string> ExperimentFiles
        {
            get { return _experimentFiles; }
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
}