using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using Cinch;
using HRC.Utility;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public class AppSettings : SerializableData<AppSettings>
    {
        #region public string ScenarioEditorExecutablePath { get; set; }

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
        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioEditorExecutablePathChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioEditorExecutablePath);
        [XmlIgnore] string _scenarioEditorExecutablePath;

        #endregion

        #region public string ScenarioDataDirectory { get; set; }

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

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioDataDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.ScenarioDataDirectory);
        [XmlIgnore] string _scenarioDataDirectory;

        #endregion

        #region public string EnvironmentDatabaseDirectory { get; set; }

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

        [XmlIgnore]
        static readonly PropertyChangedEventArgs EnvironmentDatabaseDirectoryChangedEventArgs = ObservableHelper.CreateArgs<AppSettings>(x => x.EnvironmentDatabaseDirectory);
        [XmlIgnore]
        string _environmentDatabaseDirectory;

        #endregion

        [XmlIgnore]
        static readonly string AppSettingsDirectory;
        [XmlIgnore] 
        public static readonly string AppSettingsFile;

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
        }

        public AppSettings(AppSettings that) : this()
        {
            CopyFrom(that);
        }
    }
}
