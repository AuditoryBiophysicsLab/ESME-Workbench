using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Timers;
using System.Windows.Threading;
using Cinch;

namespace ESMEWorkBench.ViewModels.NAVO
{
    class ScenarioSimulatorLogViewModel:ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        public ScenarioSimulatorLogViewModel(string logDirectory, string filePattern, Dispatcher dispatcher)
        {
            _dispatcher = dispatcher;
            FilePattern = filePattern;
            LogDirectory = logDirectory;
            
        }

        #region public string LogDirectory { get; set; }

        public string LogDirectory
        {
            get { return _logDirectory; }
            set
            {
                if (_logDirectory == value) return;
                _logDirectory = value;
                NotifyPropertyChanged(LogDirectoryChangedEventArgs);
                RefreshDirectory();

                if (_dirWatcher != null)
                {
                    _dirWatcher.EnableRaisingEvents = false;
                    _dirWatcher.Dispose();
                }
                _dirWatcher = new FileSystemWatcher(_logDirectory)
                {
                    EnableRaisingEvents = true,
                    NotifyFilter = (NotifyFilters.FileName | NotifyFilters.LastWrite | NotifyFilters.LastAccess | NotifyFilters.DirectoryName),
                };
                _dirWatcher.Created += DirectoryChanged;
                //_dirWatcher.Changed += DirectoryChanged;
                _dirWatcher.Deleted += DirectoryChanged;

            }
        }
        void DirectoryChanged(object sender, FileSystemEventArgs e)
        {
            Debug.WriteLine("[Raw] Directory: " + e.Name + " " + e.ChangeType);
            if (_dirTimer != null) return;
            _dirTimer = new Timer(1000) {AutoReset = false, Enabled = true};
            _dirTimer.Elapsed += (s1, e1) =>
                                     {
                                         _dirTimer = null;
                                         Debug.WriteLine("Directory: " + e.Name + " " + e.ChangeType);
                                         _dispatcher.Invoke(DispatcherPriority.Background, (Action) (RefreshDirectory));
                                     };

        }

        void RefreshDirectory()
        {
            _isRefreshingDirectory = true;
            var logFiles = Directory.EnumerateFiles(LogDirectory, FilePattern, SearchOption.TopDirectoryOnly);
            LogFilePairs.Clear();
            foreach (var logFile in logFiles)
            {
                LogFilePairs.Add(new KeyValuePair<string, string>(Path.GetFileNameWithoutExtension(logFile), logFile));
            }
            _isRefreshingDirectory = false;
            NotifyPropertyChanged(SelectedLogFileChangedEventArgs);
        }
        private static readonly PropertyChangedEventArgs LogDirectoryChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.LogDirectory);
        private string _logDirectory;
        private FileSystemWatcher _dirWatcher;
        private Timer _dirTimer;
        bool _isRefreshingDirectory;
        #endregion

        #region public string FilePattern { get; set; }

        public string FilePattern
        {
            get { return _filePattern; }
            set
            {
                if (_filePattern == value) return;
                _filePattern = value;
                NotifyPropertyChanged(FilePatternChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FilePatternChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.FilePattern);
        string _filePattern;

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
                if (_isRefreshingDirectory) return;
                if (_selectedLogFile == value) return;
                _selectedLogFile = value;
                FileIsSelected = !string.IsNullOrEmpty(_selectedLogFile);
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
                    if (_fileReloadTimer != null) return;
                    _fileReloadTimer = new Timer(1000) { AutoReset = false, Enabled = true };
                    _fileReloadTimer.Elapsed += (s1, e1) =>
                    {
                        _fileReloadTimer = null;
                        Debug.WriteLine("File: " + e.Name + " " + e.ChangeType);
                        _dispatcher.Invoke(DispatcherPriority.Background, (Action)(() => SelectedLogFileText = File.ReadAllText(_selectedLogFile)));
                    };
                };
            }
        }

        private FileSystemWatcher _watcher;
        Timer _fileReloadTimer;
        
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

        #region public bool AutoScroll { get; set; }

        public bool AutoScroll
        {
            get { return _autoScroll; }
            set
            {
                if (_autoScroll == value) return;
                _autoScroll = value;
                NotifyPropertyChanged(AutoScrollChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AutoScrollChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.AutoScroll);
        bool _autoScroll = true;

        #endregion

        #region public bool FileIsSelected { get; set; }

        public bool FileIsSelected
        {
            get { return _fileIsSelected; }
            set
            {
                if (_fileIsSelected == value) return;
                _fileIsSelected = value;
                NotifyPropertyChanged(FileIsSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FileIsSelectedChangedEventArgs = ObservableHelper.CreateArgs<ScenarioSimulatorLogViewModel>(x => x.FileIsSelected);
        bool _fileIsSelected;

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
