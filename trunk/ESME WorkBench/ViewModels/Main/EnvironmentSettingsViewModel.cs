using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Environment;
using ESMERibbonDemo.ViewModels;

namespace ESMEWorkBench.ViewModels.Main
{
    public class EnvironmentSettingsViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        readonly string _experimentTimeFrame;

        public EnvironmentSettingsViewModel(string databasePath, string experimentTimeFrame)
        {
            _experimentTimeFrame = experimentTimeFrame;
            WindSpeedData = new SelectableCollection<EnvironmentDataDescriptor>();
            SoundSpeedData = new SelectableCollection<EnvironmentDataDescriptor>();
            BottomTypeData = new SelectableCollection<EnvironmentDataDescriptor>();
            BathymetryData = new SelectableCollection<EnvironmentDataDescriptor>();

            DatabasePath = databasePath;

            TimeFrames = new ObservableCollection<string>
                         {
                             "january",
                             "february",
                             "march",
                             "april",
                             "may",
                             "june",
                             "july",
                             "august",
                             "september",
                             "october",
                             "november",
                             "december",
                             "winter",
                             "spring",
                             "summer",
                             "fall",
                             "warm",
                             "cold"
                         };
            foreach (var timeFrame in TimeFrames)
                if (timeFrame == experimentTimeFrame) SelectedTimeFrameItem = timeFrame;
        }

        #region public ObservableCollection<string> TimeFrames { get; set; }

        public ObservableCollection<string> TimeFrames
        {
            get { return _timeFrames; }
            set
            {
                if (_timeFrames == value) return;
                if (_timeFrames != null) _timeFrames.CollectionChanged -= TimeFramesCollectionChanged;
                _timeFrames = value;
                if (_timeFrames != null) _timeFrames.CollectionChanged += TimeFramesCollectionChanged;
                NotifyPropertyChanged(TimeFramesChangedEventArgs);
            }
        }

        void TimeFramesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(TimeFramesChangedEventArgs); }
        static readonly PropertyChangedEventArgs TimeFramesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.TimeFrames);
        ObservableCollection<string> _timeFrames;

        #endregion

        #region WindSpeedSelectionChangedCommand

        public SimpleCommand<object, EnvironmentDataDescriptor> WindSpeedSelectionChangedCommand
        {
            get { return _windSpeedSelectionChangedCommand ?? (_windSpeedSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(param => { if (param != null) WindSpeedEnvironmentFile = param.FilePath; })); }
        }

        SimpleCommand<object, EnvironmentDataDescriptor> _windSpeedSelectionChangedCommand;

        #endregion

        #region SoundSpeedSelectionChangedCommand

        public SimpleCommand<object, EnvironmentDataDescriptor> SoundSpeedSelectionChangedCommand
        {
            get { return _soundSpeedSelectionChangedCommand ?? (_soundSpeedSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(param => { if (param != null) SoundSpeedEnvironmentFile = param.FilePath; })); }
        }

        SimpleCommand<object, EnvironmentDataDescriptor> _soundSpeedSelectionChangedCommand;

        #endregion

        #region BottomTypeSelectionChangedCommand

        public SimpleCommand<object, EnvironmentDataDescriptor> BottomTypeSelectionChangedCommand
        {
            get { return _bottomTypeSelectionChangedCommand ?? (_bottomTypeSelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(param => { if (param != null) BottomTypeEnvironmentFile = param.FilePath; })); }
        }

        SimpleCommand<object, EnvironmentDataDescriptor> _bottomTypeSelectionChangedCommand;

        #endregion

        #region BathymetrySelectionChangedCommand

        public SimpleCommand<object, EnvironmentDataDescriptor> BathymetrySelectionChangedCommand
        {
            get { return _bathymetrySelectionChangedCommand ?? (_bathymetrySelectionChangedCommand = new SimpleCommand<object, EnvironmentDataDescriptor>(param => { if (param != null) BathymetryEnvironmentFile = param.FilePath; })); }
        }

        SimpleCommand<object, EnvironmentDataDescriptor> _bathymetrySelectionChangedCommand;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(x => CloseActivePopUpCommand.Execute(true))); }
        }

        SimpleCommand<object, object> _okCommand;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancelCommand ?? (_cancelCommand = new SimpleCommand<object, object>(x => CloseActivePopUpCommand.Execute(false))); }
        }

        SimpleCommand<object, object> _cancelCommand;

        #endregion

        #region public string SelectedTimeFrameItem { get; set; }

        public string SelectedTimeFrameItem
        {
            get { return _selectedTimeFrameItem; }
            set
            {
                if (_selectedTimeFrameItem == value) return;
                _selectedTimeFrameItem = value;
                NotifyPropertyChanged(SelectedTimeFrameItemChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimeFrameItemChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.SelectedTimeFrameItem);
        string _selectedTimeFrameItem;

        #endregion

        #region public string DatabasePath { get; set; }

        public string DatabasePath
        {
            get { return _databasePath; }
            set
            {
                if (_databasePath == value) return;
                _databasePath = value;
                Populate();
                //var popThread = new Thread(Populate)
                //                {
                //                    IsBackground = true
                //                };
                //popThread.Start();
                NotifyPropertyChanged(DatabasePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DatabasePathChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.DatabasePath);
        string _databasePath;

        /// <summary>
        ///   This runs on a background thread
        /// </summary>
        void Populate()
        {
            if ((DatabasePath == null) || (!Directory.Exists(DatabasePath))) return;

            WindSpeedData.Clear();
            SoundSpeedData.Clear();
            BottomTypeData.Clear();
            BathymetryData.Clear();
            var files = Directory.GetFiles(DatabasePath, "*.eeb");
            foreach (var file in files)
            {
                var dataFile = DataFile.Open(file);
                foreach (var layer in dataFile.Layers)
                {
                    EnvironmentDataDescriptor curEnvironmentData;
                    switch (layer.Name.ToLower())
                    {
                        case "windspeed":
                            curEnvironmentData = new EnvironmentDataDescriptor
                                                 {
                                                     FilePath = file,
                                                     ShortName = string.Format("{0} [{1}]", Path.GetFileNameWithoutExtension(file), layer.TimePeriod),
                                                     Command = WindSpeedSelectionChangedCommand,
                                                 };
                            WindSpeedData.Add(curEnvironmentData);
                            if (layer.TimePeriod == _experimentTimeFrame) WindSpeedData.SelectedItem = curEnvironmentData;
                            break;
                        case "soundspeed":
                            curEnvironmentData = new EnvironmentDataDescriptor
                                                 {
                                                     FilePath = file,
                                                     ShortName = string.Format("{0} [{1}]", Path.GetFileNameWithoutExtension(file), layer.TimePeriod),
                                                     Command = SoundSpeedSelectionChangedCommand,
                                                 };
                            SoundSpeedData.Add(curEnvironmentData);
                            if (layer.TimePeriod == _experimentTimeFrame) SoundSpeedData.SelectedItem = curEnvironmentData;
                            break;
                        case "bottomtype":
                            curEnvironmentData = new EnvironmentDataDescriptor
                                                 {
                                                     FilePath = file,
                                                     ShortName = string.Format("{0}", Path.GetFileNameWithoutExtension(file)),
                                                     Command = BottomTypeSelectionChangedCommand,
                                                 };
                            BottomTypeData.Add(curEnvironmentData);
                            break;
                        case "bathymetry":
                            curEnvironmentData = new EnvironmentDataDescriptor
                                                 {
                                                     FilePath = file,
                                                     ShortName = string.Format("{0}", Path.GetFileNameWithoutExtension(file)),
                                                     Command = BathymetrySelectionChangedCommand,
                                                 };
                            BathymetryData.Add(curEnvironmentData);
                            break;
                    }
                }
            }
            // Initialize bottom type and bathymetry to be the first item in the list, as these data sets are time-invariant
            BottomTypeEnvironmentFile = BottomTypeData.Count > 0 ? BottomTypeData[0].FilePath : null;
            BathymetryEnvironmentFile = BathymetryData.Count > 0 ? BathymetryData[0].FilePath : null;
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

        static readonly PropertyChangedEventArgs WindSpeedEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.WindSpeedEnvironmentFile);
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

        static readonly PropertyChangedEventArgs SoundSpeedEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.SoundSpeedEnvironmentFile);
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

        static readonly PropertyChangedEventArgs BottomTypeEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.BottomTypeEnvironmentFile);
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

        static readonly PropertyChangedEventArgs BathymetryEnvironmentFileChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.BathymetryEnvironmentFile);
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

        static readonly PropertyChangedEventArgs WindSpeedDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.WindSpeedData);
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

        static readonly PropertyChangedEventArgs SoundSpeedDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.SoundSpeedData);
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

        static readonly PropertyChangedEventArgs BottomTypeDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.BottomTypeData);
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

        static readonly PropertyChangedEventArgs BathymetryDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSettingsViewModel>(x => x.BathymetryData);
        SelectableCollection<EnvironmentDataDescriptor> _bathymetryData;

        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
        }
    }

    public class SelectableCollection<T> : ObservableCollection<T>
        where T : class, IHasName
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