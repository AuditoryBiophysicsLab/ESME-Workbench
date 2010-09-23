using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Cinch;
using ESME.Environment;
using ESMERibbonDemo.ViewModels;

namespace ESMEWorkBench.ViewModels.Main
{
    public class EnvironmentDatabaseViewModel : ViewModelBase
    {
        public EnvironmentDatabaseViewModel(string databasePath)
        {
            DatabasePath = databasePath;

            WindSpeedData = new SelectableCollection<EnvironmentDataDescriptor>();
            SoundSpeedData = new SelectableCollection<EnvironmentDataDescriptor>();
            BottomTypeData = new SelectableCollection<EnvironmentDataDescriptor>();
            BathymetryData = new SelectableCollection<EnvironmentDataDescriptor>();

            WindSpeedSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(delegate(EnvironmentDataDescriptor param)
            {
                WindSpeedEnvironmentFile = param.FilePath;
            });
            SoundSpeedSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(delegate(EnvironmentDataDescriptor param)
            {
                SoundSpeedEnvironmentFile = param.FilePath;
            });
            BottomTypeSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(delegate(EnvironmentDataDescriptor param)
            {
                BottomTypeEnvironmentFile = param.FilePath;
            });
            BathymetrySelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(delegate(EnvironmentDataDescriptor param)
            {
                BathymetryEnvironmentFile = param.FilePath;
            });
        }

        public SimpleCommand<object, EnvironmentDataDescriptor> WindSpeedSelectionChangedCommand { get; private set; }
        public SimpleCommand<object, EnvironmentDataDescriptor> SoundSpeedSelectionChangedCommand { get; private set; }
        public SimpleCommand<object, EnvironmentDataDescriptor> BottomTypeSelectionChangedCommand { get; private set; }
        public SimpleCommand<object, EnvironmentDataDescriptor> BathymetrySelectionChangedCommand { get; private set; }

        #region public string DatabasePath { get; set; }

        public string DatabasePath
        {
            get { return _databasePath; }
            set
            {
                if (_databasePath == value) return;
                _databasePath = value;
                var popThread = new Thread(Populate)
                                {
                                    IsBackground = true
                                };
                popThread.Start();
                NotifyPropertyChanged(DatabasePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DatabasePathChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.DatabasePath);
        string _databasePath;

        /// <summary>
        /// This runs on a background thread
        /// </summary>
        private void Populate()
        {
            if ((DatabasePath == null) || (!Directory.Exists(DatabasePath))) return;

            WindSpeedData.Clear();
            SoundSpeedData.Clear();
            BottomTypeData.Clear();
            BathymetryData.Clear();
            var files = Directory.GetFiles(DatabasePath, "*.eeb");
            Parallel.ForEach(files, file =>
                                    {
                                        var dataFile = DataFile.Open(file);
                                        foreach (var layer in dataFile.Layers)
                                        {
                                            switch (layer.Name.ToLower())
                                            {
                                                case "windspeed":
                                                    WindSpeedData.Add(new EnvironmentDataDescriptor
                                                                      {
                                                                          FilePath = file,
                                                                          ShortName = string.Format("{0} [{1}]", Path.GetFileNameWithoutExtension(file), layer.TimePeriod),
                                                                          Command = WindSpeedSelectionChangedCommand,
                                                                      });
                                                    break;
                                                case "soundspeed":
                                                    SoundSpeedData.Add(new EnvironmentDataDescriptor
                                                                       {
                                                                           FilePath = file,
                                                                           ShortName = string.Format("{0} [{1}]", Path.GetFileNameWithoutExtension(file), layer.TimePeriod),
                                                                           Command = SoundSpeedSelectionChangedCommand,
                                                                       });
                                                    break;
                                                case "bottomtype":
                                                    BottomTypeData.Add(new EnvironmentDataDescriptor
                                                                       {
                                                                           FilePath = file,
                                                                           ShortName = string.Format("{0}", Path.GetFileNameWithoutExtension(file)),
                                                                           Command = BottomTypeSelectionChangedCommand,
                                                                       });
                                                    break;
                                                case "bathymetry":
                                                    BathymetryData.Add(new EnvironmentDataDescriptor
                                                                       {
                                                                           FilePath = file,
                                                                           ShortName = string.Format("{0}", Path.GetFileNameWithoutExtension(file)),
                                                                           Command = BathymetrySelectionChangedCommand,
                                                                       });
                                                    break;
                                            }
                                        }
                                    });
            // TODO: Put initialization code for the file selections here, when a per-experiment data model is available
            // Remove these lines when initializing 'for-real'
            WindSpeedEnvironmentFile = WindSpeedData[0].FilePath;
            SoundSpeedEnvironmentFile = SoundSpeedData[0].FilePath;
            BottomTypeEnvironmentFile = BottomTypeData[0].FilePath;
            BathymetryEnvironmentFile = BathymetryData[0].FilePath;
        }

        #endregion

        #region public string WindSpeedEnvironmentFile { get; set; }

        public string WindSpeedEnvironmentFile
        {
            get { return _windSpeedEnvironmentFile; }
            set
            {
                if (_windSpeedEnvironmentFile == value) return;
                _windSpeedEnvironmentFile = value;
                WindSpeedData.SelectedItem = WindSpeedData[_windSpeedEnvironmentFile];
                NotifyPropertyChanged(WindSpeedEnvironmentFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindSpeedEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.WindSpeedEnvironmentFile);
        string _windSpeedEnvironmentFile;

        #endregion

        #region public string SoundSpeedEnvironmentFile { get; set; }

        public string SoundSpeedEnvironmentFile
        {
            get { return _soundSpeedEnvironmentFile; }
            set
            {
                if (_soundSpeedEnvironmentFile == value) return;
                _soundSpeedEnvironmentFile = value;
                SoundSpeedData.SelectedItem = SoundSpeedData[_soundSpeedEnvironmentFile];
                NotifyPropertyChanged(SoundSpeedEnvironmentFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SoundSpeedEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.SoundSpeedEnvironmentFile);
        string _soundSpeedEnvironmentFile;

        #endregion

        #region public string BottomTypeEnvironmentFile { get; set; }

        public string BottomTypeEnvironmentFile
        {
            get { return _bottomTypeEnvironmentFile; }
            set
            {
                if (_bottomTypeEnvironmentFile == value) return;
                _bottomTypeEnvironmentFile = value;
                BottomTypeData.SelectedItem = BottomTypeData[_bottomTypeEnvironmentFile];
                NotifyPropertyChanged(BottomTypeEnvironmentFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomTypeEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.BottomTypeEnvironmentFile);
        string _bottomTypeEnvironmentFile;

        #endregion

        #region public string BathymetryEnvironmentFile { get; set; }

        public string BathymetryEnvironmentFile
        {
            get { return _bathymetryEnvironmentFile; }
            set
            {
                if (_bathymetryEnvironmentFile == value) return;
                _bathymetryEnvironmentFile = value;
                BathymetryData.SelectedItem = BathymetryData[_bathymetryEnvironmentFile];
                NotifyPropertyChanged(BathymetryEnvironmentFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.BathymetryEnvironmentFile);
        string _bathymetryEnvironmentFile;

        #endregion

        #region public SelectableCollection<EnvironmentDataDescriptor> WindSpeedData { get; set; }

        public SelectableCollection<EnvironmentDataDescriptor> WindSpeedData
        {
            get { return _windSpeedData; }
            set
            {
                if (_windSpeedData == value) return;
                if (_windSpeedData != null) _windSpeedData.CollectionChanged -= WindSpeedDataCollectionChanged;
                _windSpeedData = value;
                if (_windSpeedData != null) _windSpeedData.CollectionChanged += WindSpeedDataCollectionChanged;
                NotifyPropertyChanged(WindSpeedDataChangedEventArgs);
            }
        }

        void WindSpeedDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(WindSpeedDataChangedEventArgs); }

        static readonly PropertyChangedEventArgs WindSpeedDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.WindSpeedData);
        SelectableCollection<EnvironmentDataDescriptor> _windSpeedData;

        #endregion

        #region public SelectableCollection<EnvironmentDataDescriptor> SoundSpeedData { get; set; }

        public SelectableCollection<EnvironmentDataDescriptor> SoundSpeedData
        {
            get { return _soundSpeedData; }
            set
            {
                if (_soundSpeedData == value) return;
                if (_soundSpeedData != null) _soundSpeedData.CollectionChanged -= SoundSpeedDataCollectionChanged;
                _soundSpeedData = value;
                if (_soundSpeedData != null) _soundSpeedData.CollectionChanged += SoundSpeedDataCollectionChanged;
                NotifyPropertyChanged(SoundSpeedDataChangedEventArgs);
            }
        }

        void SoundSpeedDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SoundSpeedDataChangedEventArgs); }

        static readonly PropertyChangedEventArgs SoundSpeedDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.SoundSpeedData);
        SelectableCollection<EnvironmentDataDescriptor> _soundSpeedData;

        #endregion

        #region public SelectableCollection<EnvironmentDataDescriptor> BottomTypeData { get; set; }

        public SelectableCollection<EnvironmentDataDescriptor> BottomTypeData
        {
            get { return _bottomTypeData; }
            set
            {
                if (_bottomTypeData == value) return;
                if (_bottomTypeData != null) _bottomTypeData.CollectionChanged -= BottomTypeDataCollectionChanged;
                _bottomTypeData = value;
                if (_bottomTypeData != null) _bottomTypeData.CollectionChanged += BottomTypeDataCollectionChanged;
                NotifyPropertyChanged(BottomTypeDataChangedEventArgs);
            }
        }

        void BottomTypeDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BottomTypeDataChangedEventArgs); }

        static readonly PropertyChangedEventArgs BottomTypeDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.BottomTypeData);
        SelectableCollection<EnvironmentDataDescriptor> _bottomTypeData;

        #endregion

        #region public SelectableCollection<EnvironmentDataDescriptor> BathymetryData { get; set; }

        public SelectableCollection<EnvironmentDataDescriptor> BathymetryData
        {
            get { return _bathymetryData; }
            set
            {
                if (_bathymetryData == value) return;
                if (_bathymetryData != null) _bathymetryData.CollectionChanged -= BathymetryDataCollectionChanged;
                _bathymetryData = value;
                if (_bathymetryData != null) _bathymetryData.CollectionChanged += BathymetryDataCollectionChanged;
                NotifyPropertyChanged(BathymetryDataChangedEventArgs);
            }
        }

        void BathymetryDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BathymetryDataChangedEventArgs); }

        static readonly PropertyChangedEventArgs BathymetryDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDatabaseViewModel>(x => x.BathymetryData);
        SelectableCollection<EnvironmentDataDescriptor> _bathymetryData;

        #endregion
    }

    public class SelectableCollection<T> : ObservableCollection<T> where T : class, IHasName
    {
        #region public T SelectedItem { get; set; }

        public T SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if ((value != null) && (_selectedItem != null) && (_selectedItem.Equals(value))) return;
                _selectedItem = value;
                OnPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        public T this[string name]
        {
            get
            {
                var result = from item in this
                             where item.Name == name
                             select item;
                if (result == null) throw new IndexOutOfRangeException("SelectableCollection: Could not find item with name: " + name);
                return result.First();
                
            }
        }

        static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<SelectableCollection<T>>(x => x.SelectedItem);
        T _selectedItem;

        #endregion
    }

    public class EnvironmentDataDescriptor : ViewModelBase, IHasName
    {
        #region public string FilePath { get; set; }

        public string FilePath
        {
            get { return _filePath; }
            set
            {
                if (_filePath == value) return;
                _filePath = value;
                NotifyPropertyChanged(FilePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FilePathChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDataDescriptor>(x => x.FilePath);
        string _filePath;

        #endregion

        #region public string ShortName { get; set; }

        public string ShortName
        {
            get { return _shortName; }
            set
            {
                if (_shortName == value) return;
                _shortName = value;
                NotifyPropertyChanged(ShortNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ShortNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentDataDescriptor>(x => x.ShortName);
        string _shortName;

        #endregion

        public SimpleCommand<object, EnvironmentDataDescriptor> Command { get; set; }

        public override string ToString() { return ShortName; }

        public string Name
        {
            get { return FilePath; }
            set { FilePath = value; }
        }
    }
}
