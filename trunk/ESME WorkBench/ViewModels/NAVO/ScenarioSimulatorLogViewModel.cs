using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;

namespace ESMEWorkBench.ViewModels.NAVO
{
    class ScenarioSimulatorLogViewModel:ViewModelBase 
    {
        public ScenarioSimulatorLogViewModel(string logDirectoryPath)
        {
            var logFiles = Directory.EnumerateFiles(logDirectoryPath, "*.log.*", SearchOption.TopDirectoryOnly);
            foreach (var logFile in logFiles)
            {
                LogFilePairs.Add(new KeyValuePair<string, string>(Path.GetFileNameWithoutExtension(logFile),logFile));
            }
        }

        #region public string LogDir { get; set; }

        public string LogDir
        {
            get { return _logDir; }
            set
            {
                if (_logDir == value) return;
                _logDir = value;
                NotifyPropertyChanged(LogDirChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs LogDirChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.LogDir);
        private string _logDir;

        #endregion


        #region public ObservableCollection<KeyValuePair<string,string>> LogFilePairs { get; set; }

        public ObservableCollection<KeyValuePair<string,string>> LogFilePairs
        {
            get { return _logFilePairs ?? (_logFilePairs = new ObservableCollection<KeyValuePair<string,string>>()); }
            set
            {
                if (_logFilePairs == value) return;
                if (_logFilePairs != null) _logFilePairs.CollectionChanged -= LogFilePairsCollectionChanged;
                _logFilePairs = value;
                if (_logFilePairs != null) _logFilePairs.CollectionChanged += LogFilePairsCollectionChanged;
                NotifyPropertyChanged(LogFilePairsChangedEventArgs);
            }
        }

        private void LogFilePairsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(LogFilePairsChangedEventArgs);
        }

        private static readonly PropertyChangedEventArgs LogFilePairsChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.LogFilePairs);
        private ObservableCollection<KeyValuePair<string,string>> _logFilePairs;

        #endregion

        #region public string SelectedLogFile { get; set; }

        public string SelectedLogFile
        {
            get { return _selectedLogFile; }
            set
            {
                if (_selectedLogFile == value) return;
                _selectedLogFile = value;
                NotifyPropertyChanged(SelectedLogFileChangedEventArgs);
                SelectedLogFileHeaderText = string.Format("Most recent contents of log file {0}",
                                                          Path.GetFileName(_selectedLogFile));
                SelectedLogFileText = File.ReadAllText(_selectedLogFile);
                if (_watcher != null)
                {
                    _watcher.EnableRaisingEvents = false;
                    _watcher.Dispose();
                }
                _watcher = new FileSystemWatcher(Path.GetDirectoryName(_selectedLogFile),Path.GetFileName(_selectedLogFile))
                               {
                                   EnableRaisingEvents = true,
                                   NotifyFilter = NotifyFilters.LastWrite,
                               };
                _watcher.Changed += (s, e) =>
                                        {
                                            Debug.WriteLine("changed");
                                            SelectedLogFileText = File.ReadAllText(_selectedLogFile);
                                        };
            }
        }

        private FileSystemWatcher _watcher;

        private static readonly PropertyChangedEventArgs SelectedLogFileChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.SelectedLogFile);
        private string _selectedLogFile;

        #endregion

        #region public string SelectedLogFileHeaderText { get; set; }

        public string SelectedLogFileHeaderText
        {
            get { return _selectedLogFileHeaderText; }
            set
            {
                if (_selectedLogFileHeaderText == value) return;
                _selectedLogFileHeaderText = value;
                NotifyPropertyChanged(SelectedLogFileHeaderTextChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs SelectedLogFileHeaderTextChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.SelectedLogFileHeaderText);
        private string _selectedLogFileHeaderText = "Log file contents";

        #endregion

        #region public string SelectedLogFileText { get; set; }

        public string SelectedLogFileText
        {
            get { return _selectedLogFileText; }
            set
            {
                if (_selectedLogFileText == value) return;
                _selectedLogFileText = value;
                NotifyPropertyChanged(SelectedLogFileTextChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs SelectedLogFileTextChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.SelectedLogFileText);
        private string _selectedLogFileText;

        #endregion

        #region CloseCommand

        public SimpleCommand<object, object> CloseCommand
        {
            get
            {
                return _close ??
                       (_close =
                        new SimpleCommand<object, object>(delegate { CloseHandler(); }));
            }
        }

        private SimpleCommand<object, object> _close;
        
        private void CloseHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}
