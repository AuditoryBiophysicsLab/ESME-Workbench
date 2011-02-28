using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
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

        [XmlIgnore] static string _appSettingsDirectory;

        [XmlIgnore]
        public static string ApplicationName
        {
            get { return _appName; }
            set
            {
                if (_appName != null) throw new ApplicationException("AppSettings.ApplicationName: Cannot set this value more than once.");
                _appName = value;
                _appSettingsDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), _appName);
                if (!Directory.Exists(_appSettingsDirectory)) Directory.CreateDirectory(_appSettingsDirectory);
                AppSettingsFile = Path.Combine(_appSettingsDirectory, "settings.xml");
            }
        }
        [XmlIgnore] static string _appName;

        [XmlIgnore]
        public static string AppSettingsFile { get; private set; }


        public AppSettings()
        {
            FileName = AppSettingsFile;
            if (NAVOConfiguration == null) NAVOConfiguration = new NAVOConfiguration();
        }

        public AppSettings(AppSettings that) : this() { CopyFrom(that); }
        [XmlIgnore]
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

        public void Save()
        {
            Save(FileName,ReferencedTypes);
        }

        public void Reload()
        {
            Reload(ReferencedTypes);
        }

        #region public string ScenarioEditorExecutablePath { get; set; }

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioEditorExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioEditorExecutablePath);
        [XmlIgnore] string _scenarioEditorExecutablePath;

        [XmlElement]
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

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioDataDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioDataDirectory);
        [XmlIgnore] string _scenarioDataDirectory;

        [XmlElement]
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

        [XmlIgnore] static readonly PropertyChangedEventArgs EnvironmentDatabaseDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.EnvironmentDatabaseDirectory);
        [XmlIgnore] string _environmentDatabaseDirectory;

        [XmlElement]
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


    }
}