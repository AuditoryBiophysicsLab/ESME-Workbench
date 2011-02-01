using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Utility;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public class AppSettings : SerializableData<AppSettings>
    {
        static Type[] _referencedTypes;

        [XmlIgnore] static readonly string AppSettingsDirectory;
        [XmlIgnore] public static readonly string AppSettingsFile;

        static AppSettings()
        {
            try
            {
                AppSettingsDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Path.GetFileNameWithoutExtension(Assembly.GetEntryAssembly().GetName().CodeBase));
            }
            catch (Exception)
            {
                AppSettingsDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            }
            if (!Directory.Exists(AppSettingsDirectory)) Directory.CreateDirectory(AppSettingsDirectory);
            AppSettingsFile = Path.Combine(AppSettingsDirectory, "settings.xml");

        }

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
        NAVOConfiguration _nAVOConfiguration;

        public NAVOConfiguration NAVOConfiguration
        {
            get { return _nAVOConfiguration; }
            set
            {
                if (_nAVOConfiguration == value) return;
                _nAVOConfiguration = value;
                NotifyPropertyChanged(NAVOConfigurationChangedEventArgs);
            }
        }

        #endregion
    }
}