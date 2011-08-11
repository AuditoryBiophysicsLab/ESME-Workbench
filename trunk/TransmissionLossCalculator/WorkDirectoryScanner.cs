using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Timers;
using Cinch;

namespace TransmissionLossCalculator
{
    public class WorkDirectoryScanner : ViewModelBase
    {
        internal WorkDirectoryScanner(string directoryToScan, string filePattern, ObservableCollection<string> matchingFileCollection)
        {
            _matchingFiles = matchingFileCollection;
            _filePattern = filePattern;
            DirectoryPath = directoryToScan;
        }

        #region internal string DirectoryPath { get; set; }

        internal string DirectoryPath
        {
            get { return _directoryPath; }
            set
            {
                if (_directoryPath == value) return;
                _directoryPath = value;
                _dirWatcher = new FileSystemWatcher(_directoryPath, _filePattern)
                {
                    EnableRaisingEvents = true,
                    NotifyFilter = (NotifyFilters.FileName | NotifyFilters.LastWrite | NotifyFilters.LastAccess | NotifyFilters.DirectoryName),
                };
                _dirWatcher.Created += DirectoryChanged;
                //_dirWatcher.Changed += DirectoryChanged;
                _dirWatcher.Deleted += DirectoryChanged;
                ScanDirectory();
            }
        }

        void ScanDirectory()
        {
            var matchingFiles = Directory.EnumerateFiles(DirectoryPath, _filePattern, SearchOption.TopDirectoryOnly);
            foreach (var file in matchingFiles.Where(file => !_matchingFiles.Any(match => match == file)))
                _matchingFiles.Add(file);
        }

        string _directoryPath;
        FileSystemWatcher _dirWatcher;
        private Timer _dirTimer;
        readonly string _filePattern;
        ObservableCollection<string> _matchingFiles;

        void DirectoryChanged(object sender, FileSystemEventArgs e)
        {
#if true
            Debug.WriteLine("Directory: " + e.Name + " " + e.ChangeType);
            switch (e.ChangeType)
            {
                case WatcherChangeTypes.Created:
                    if (!_matchingFiles.Any(match => match == e.FullPath)) _matchingFiles.Add(e.FullPath);
                    _matchingFiles.Add(e.FullPath);
                    break;
                case WatcherChangeTypes.Changed:
                    break;
                case WatcherChangeTypes.Deleted:
                    var matchedFiles = _matchingFiles.Where(file => file == e.FullPath).ToList();
                    foreach (var matchedFile in matchedFiles) _matchingFiles.Remove(matchedFile);
                    break;
            }
#else
            Debug.WriteLine("[Raw] Directory: " + e.Name + " " + e.ChangeType);
            if (_dirTimer != null) return;
            _dirTimer = new Timer(1000) { AutoReset = false, Enabled = true };
            _dirTimer.Elapsed += (s1, e1) =>
            {
                _dirTimer = null;
                Debug.WriteLine("Directory: " + e.Name + " " + e.ChangeType);
                switch (e.ChangeType)
                {
                    case WatcherChangeTypes.Created:
                        if (!_matchingFiles.Any(match => match == e.FullPath)) _matchingFiles.Add(e.FullPath);
                        _matchingFiles.Add(e.FullPath);
                        break;
                    case WatcherChangeTypes.Changed:
                        break;
                    case WatcherChangeTypes.Deleted:
                        var matchedFiles = _matchingFiles.Where(file => file == e.FullPath).ToList();
                        foreach (var matchedFile in matchedFiles) _matchingFiles.Remove(matchedFile);
                        break;
                }
            };
#endif
        }

        #endregion

        internal void Stop()
        {
            if (_dirWatcher != null) _dirWatcher.EnableRaisingEvents = false;
            if (_dirTimer != null) _dirTimer.Stop();
        }

        protected override void OnDispose()
        {
            Stop();
            _dirWatcher = null;
            _dirTimer = null;
            _matchingFiles = null;
            base.OnDispose();
        }
    }

    public class WorkDirectoryScanners : List<WorkDirectoryScanner>
    {
        public WorkDirectoryScanners(ObservableCollection<string> matchingFileCollection) { _matchingFileCollection = matchingFileCollection; }

        readonly ObservableCollection<string> _matchingFileCollection;

        public void Add(string directoryToScan, string filePattern)
        {
            Add(new WorkDirectoryScanner(directoryToScan, filePattern, _matchingFileCollection));
        }

        public void Remove(string directoryToScan)
        {
            foreach (var scanner in this.Where(scanner => scanner.DirectoryPath == directoryToScan))
            {
                scanner.Stop();
                Remove(scanner);
                break;
            }
        }
    }
}
