using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Timers;
using HRC.Utility;

namespace ESME.Data
{
    public class WorkDirectories : List<string>, INotifyCollectionChanged, INotifyPropertyChanged
    {
        public static readonly List<Type> ReferencedTypes = new List<Type>();

        public static WorkDirectories Load(bool reloadOnFileChanged = false)
        {
            if (AppSettingsFile == null) throw new ApplicationException("WorkDirectories: ApplicationName has not been set!");
            if (!File.Exists(AppSettingsFile)) return new WorkDirectories();
            var result = XmlSerializer<WorkDirectories>.Load(AppSettingsFile, ReferencedTypes);
            result.DiscardInvalidDirectories();
            if (reloadOnFileChanged)
                result.ReloadOnFileChange = true;
            return result;
        }

        public void Reload()
        {
            var result = Load();
            foreach (var directory in result)
                Add(directory);
        }

        public void Save()
        {
            DiscardInvalidDirectories();
            var serializer = new XmlSerializer<WorkDirectories> { Data = this };
            serializer.Save(AppSettingsFile, ReferencedTypes);
        }

        public static string ApplicationName
        {
            get { return _appName; }
            set
            {
                _appName = value;
                _appSettingsDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), _appName);
                if (!Directory.Exists(_appSettingsDirectory)) Directory.CreateDirectory(_appSettingsDirectory);
                AppSettingsFile = Path.Combine(_appSettingsDirectory, "WorkDirectories.xml");
            }
        }

        static string _appName;
        static string _appSettingsDirectory;

        public static string AppSettingsFile { get; private set; }

        public void DiscardInvalidDirectories()
        {
            RemoveAll(directory => !Directory.Exists(directory));
        }

        public bool ReloadOnFileChange
        {
            get { return _reloadOnFileChange; }
            set
            {
                if (_reloadOnFileChange == value) return;
                _reloadOnFileChange = value;
                if (_watcher != null)
                {
                    _watcher.EnableRaisingEvents = false;
                    _watcher.Dispose();
                }
                if (_reloadOnFileChange == false) return;
                _watcher = new FileSystemWatcher(Path.GetDirectoryName(AppSettingsFile),
                                                 Path.GetFileName(AppSettingsFile))
                               {
                                   EnableRaisingEvents = true,
                                   NotifyFilter = NotifyFilters.LastWrite,
                               };
                _watcher.Changed += (s, e) =>
                                        {
                                            if (_fileReloadTimer != null) return;
                                            _fileReloadTimer = new Timer(1000) {AutoReset = false, Enabled = true};
                                            _fileReloadTimer.Elapsed += (s1, e1) =>
                                                                            {
                                                                                _fileReloadTimer = null;
                                                                                Debug.WriteLine("File: " + e.Name + " " +
                                                                                                e.ChangeType);
                                                                                Reload();
                                                                            };
                                        };
            }
        }

        private bool _reloadOnFileChange;
        private FileSystemWatcher _watcher;
        Timer _fileReloadTimer;

        #region ObservableCollection stuff
        public event NotifyCollectionChangedEventHandler CollectionChanged;
        public event PropertyChangedEventHandler PropertyChanged;

        public new void AddRange(IEnumerable<string> items)
        {
            var newItems = new List<string>();
            foreach (var item in items)
            {
                if (this.Any(directory => directory == item)) continue;
                newItems.Add(item);
                base.Add(item);
            }
            Save();
            if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, newItems));
        }

        public new void Add(string workDirectory)
        {
            if (this.Any(directory => directory == workDirectory)) return;
            base.Add(workDirectory);
            Save();
            if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, workDirectory));
        }

        public new void Clear()
        {
            base.Clear();
            if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public new bool Remove(string item)
        {
            var result = base.Remove(item);
            if (result && (CollectionChanged != null)) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
            return result;
        }

        public new void Insert(int index, string workDirectory)
        {
            base.Insert(index, workDirectory);
            if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, workDirectory));
        }

        public new void RemoveAt(int index)
        {
            var item = this[index];
            base.RemoveAt(index);
            if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
        }

        public new string this[int index]
        {
            get { return base[index]; }
            set
            {
                base[index] = value;
                if (CollectionChanged != null) CollectionChanged(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, value));
            }
        }
        #endregion
    }
}
