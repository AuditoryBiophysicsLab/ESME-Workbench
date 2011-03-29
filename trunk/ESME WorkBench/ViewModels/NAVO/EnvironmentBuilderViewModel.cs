using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.Data;
using ESME.Environment.NAVO;
using ESMEWorkBench.Data;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVO
{
    [ExportViewModel("EnvironmentBuilderViewModel")]
    public class EnvironmentBuilderViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly IUIVisualizerService _visualizerService;
        Dispatcher _dispatcher;
        IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;
        readonly Experiment _experiment;
        bool _extractionCanceled;

        public EnvironmentBuilderViewModel(IUIVisualizerService visualizerService, IMessageBoxService messageBoxService, AppSettings appSettings, Experiment experiment)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nEnvironmentBuilderViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _visualizerService = visualizerService;
            _messageBoxService = messageBoxService;
            AppSettings = appSettings;
            _experiment = experiment;
            ExtractButtonText = "Initializing...";
        }

        #region public float BufferZoneSize { get; set; }

        public float BufferZoneSize
        {
            get { return _bufferZoneSize; }
            set
            {
                if (_bufferZoneSize == value) return;
                if (value < 0) 
                {
                    _messageBoxService.ShowError("Buffer zone size may not be negative");
                    NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
                    return;
                }
                _bufferZoneSize = value;

                NotifyPropertyChanged(BufferZoneSizeChangedEventArgs);
                NAVODataSources.ExtractionArea = GeoRect.Inflate(_experiment.OpArea, BufferZoneSize * 1000, BufferZoneSize * 1000);
            }
        }

        static readonly PropertyChangedEventArgs BufferZoneSizeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.BufferZoneSize);
        float _bufferZoneSize;

        #endregion

        #region SelectAllMonthsCommand

        SimpleCommand<object, object> _selectAllMonths;

        public SimpleCommand<object, object> SelectAllMonthsCommand
        {
            get { return _selectAllMonths ?? (_selectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllMonthsCommand

        SimpleCommand<object, object> _unselectAllMonths;

        public SimpleCommand<object, object> UnselectAllMonthsCommand
        {
            get { return _unselectAllMonths ?? (_unselectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region SelectAllSeasonsCommand

        SimpleCommand<object, object> _selectAllSeasons;

        public SimpleCommand<object, object> SelectAllSeasonsCommand
        {
            get { return _selectAllSeasons ?? (_selectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllSeasonsCommand

        SimpleCommand<object, object> _unselectAllSeasons;

        public SimpleCommand<object, object> UnselectAllSeasonsCommand
        {
            get { return _unselectAllSeasons ?? (_unselectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region public ObservableCollection<NAVOTimePeriod> SelectedPeriods { get; set; }

        static readonly PropertyChangedEventArgs SelectedPeriodsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.SelectedPeriods);
        ObservableCollection<NAVOTimePeriod> _selectedPeriods;

        public ObservableCollection<NAVOTimePeriod> SelectedPeriods
        {
            get { return _selectedPeriods; }
            set
            {
                if (_selectedPeriods == value) return;
                if (_selectedPeriods != null) _selectedPeriods.CollectionChanged -= SelectedPeriodsCollectionChanged;
                _selectedPeriods = value;
                if (_selectedPeriods != null) _selectedPeriods.CollectionChanged += SelectedPeriodsCollectionChanged;
                NotifyPropertyChanged(SelectedPeriodsChangedEventArgs);
            }
        }

        void SelectedPeriodsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SelectedPeriodsChangedEventArgs); }

        #endregion

        #region public CheckboxSettings MonthCheckboxes { get; set; }

        public CheckboxSettings MonthCheckboxes
        {
            get { return _monthCheckboxes; }
            set
            {
                if (_monthCheckboxes == value) return;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged -= MonthCheckboxesCollectionChanged;
                _monthCheckboxes = value;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged += MonthCheckboxesCollectionChanged;
                NotifyPropertyChanged(MonthCheckboxesChangedEventArgs);
            }
        }

        void MonthCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(MonthCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs MonthCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.MonthCheckboxes);
        static CheckboxSettings _monthCheckboxes = new CheckboxSettings
                                                   {
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.January
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.February
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.March
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.April
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.May
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.June
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.July
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.August
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.September
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.October
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.November
                                                       },
                                                       new CheckboxSetting
                                                       {
                                                           TimePeriod = NAVOTimePeriod.December
                                                       },
                                                   };

        #endregion

        #region public CheckboxSettings SeasonCheckboxes { get; set; }

        public CheckboxSettings SeasonCheckboxes
        {
            get { return _seasonCheckboxes; }
            set
            {
                if (_seasonCheckboxes == value) return;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged -= SeasonCheckboxesCollectionChanged;
                _seasonCheckboxes = value;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged += SeasonCheckboxesCollectionChanged;
                NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs);
            }
        }

        void SeasonCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs SeasonCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.SeasonCheckboxes);

        CheckboxSettings _seasonCheckboxes = new CheckboxSettings
                                             {
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Spring
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Summer
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Fall
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Winter
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Warm
                                                 },
                                                 new CheckboxSetting
                                                 {
                                                     TimePeriod = NAVOTimePeriod.Cold
                                                 },
                                             };

        #endregion
        
        #region public AppSettings AppSettings { get; set; }

        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.AppSettings);
        AppSettings _appSettings;

        public AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }

        #endregion

        #region public NAVODataSources NAVODataSources { get; set; }

        public NAVODataSources NAVODataSources
        {
            get { return _navoDataSources; }
            set
            {
                if (_navoDataSources == value) return;
                _navoDataSources = value;
                NotifyPropertyChanged(NAVODataSourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAVODataSourcesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.NAVODataSources);
        NAVODataSources _navoDataSources;

        #endregion

        #region public string ExtractButtonText { get; set; }

        public string ExtractButtonText
        {
            get { return _extractButtonText; }
            set
            {
                if (_extractButtonText == value) return;
                _extractButtonText = value;
                NotifyPropertyChanged(ExtractButtonTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractButtonTextChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExtractButtonText);
        string _extractButtonText;

        #endregion

        #region public bool NotExtractingData { get; set; }

        public bool NotExtractingData
        {
            get { return _notExtractingData; }
            set
            {
                if (_notExtractingData == value) return;
                _notExtractingData = value;
                NotifyPropertyChanged(ExtractingDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractingDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.NotExtractingData);
        bool _notExtractingData = true;

        #endregion

        #region public bool ExportCASSData { get; set; }

        public bool ExportCASSData
        {
            get { return _exportCASSData; }
            set
            {
                if (_exportCASSData == value) return;
                _exportCASSData = value;
                NotifyPropertyChanged(ExportCASSDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportCASSDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExportCASSData);
        bool _exportCASSData;

        #endregion

        #region BufferZoneSizeTextChangedCommand

        public SimpleCommand<object, object> BufferZoneSizeTextChangedCommand
        {
            get
            {
                return _bufferZoneSizeTextChanged ?? (_bufferZoneSizeTextChanged = new SimpleCommand<object, object>(delegate(object cinchArgs)
                {
                    var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                    //var args = (TextChangedEventArgs)((EventToCommandArgs)cinchArgs).EventArgs;
                    if (sender != null && !string.IsNullOrEmpty(sender.Text)) BufferZoneSize = float.Parse(sender.Text);
                }));
            }
        }

        SimpleCommand<object, object> _bufferZoneSizeTextChanged;

        #endregion

        #region ExtractAllCommand

        SimpleCommand<object, object> _extractAll;

        public SimpleCommand<object, object> ExtractAllCommand
        {
            get
            {
                return _extractAll ?? (_extractAll = new SimpleCommand<object, object>(
                    delegate
                    {
                        return  ((NAVODataSources != null) && (NotExtractingData) && 
                                 ((MonthCheckboxes.SelectedTimePeriods.Count() > 0) || (SeasonCheckboxes.SelectedTimePeriods.Count() > 0))); }, 
                    delegate
                    {
                        var selectedTimePeriods = new List<NAVOTimePeriod>();
                        if (MonthCheckboxes != null)
                            selectedTimePeriods.AddRange(MonthCheckboxes.SelectedTimePeriods);
                        if (SeasonCheckboxes != null)
                            selectedTimePeriods.AddRange(SeasonCheckboxes.SelectedTimePeriods);
                        if (selectedTimePeriods.Count < 1) return;

                        NAVODataSources.SelectedTimePeriods = selectedTimePeriods;
                        NAVODataSources.ExportCASSData = ExportCASSData;
                        NAVODataSources.ExtractDataInBackground(delegate
                                                                {
                                                                    NotExtractingData = true;
                                                                    ExtractButtonText = "Extract";
                                                                    CommandManager.InvalidateRequerySuggested();
                                                                    if (_extractionCanceled) return;
                                                                    if (selectedTimePeriods.Count > 0)
                                                                    {
                                                                        var timePeriod = selectedTimePeriods[0];
                                                                        _experiment.WindSpeedFileName = NAVODataSources.WindFilename(timePeriod);
                                                                        _experiment.SoundSpeedFileName = NAVODataSources.SoundspeedFilename(timePeriod);
                                                                        _experiment.SedimentFileName = NAVODataSources.SedimentFilename(timePeriod);
                                                                        _experiment.BathymetryFileName = NAVODataSources.BathymetryFilename(timePeriod);
                                                                        _experiment.SimArea = NAVODataSources.ExtractionArea;
                                                                    }
                                                                    AppSettings.Save(); //remember the new values. 
                                                                    CloseActivePopUpCommand.Execute(true);
                                                                });
                        NotExtractingData = false;
                        ExtractButtonText = "Extracting...";
                        _extractionCanceled = false;
                        //close the view.
                    }));
            }
        }

        #endregion

        #region CancelCommand

        SimpleCommand<object, object> _cancel;

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                                                                               {
                                                                                   if (!NotExtractingData)
                                                                                   {
                                                                                       NAVODataSources.CancelExtraction();
                                                                                       _extractionCanceled = true;
                                                                                   }
                                                                                   else
                                                                                   {
                                                                                       AppSettings.Reload(); //invalidate all changes.
                                                                                       CloseActivePopUpCommand.Execute(false);
                                                                                   }
                                                                               }));
            }
        }

        #endregion
        
        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            NAVODataSources = new NAVODataSources(_experiment.OpArea, Globals.AppSettings.NAVOConfiguration, _dispatcher, _experiment.LocalStorageRoot, Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName));
            ExtractButtonText = "Extract";
        }

        #endregion
    }

    public class CheckboxSettings : ObservableCollection<CheckboxSetting>
    {
        public CheckboxSetting this[string caption]
        {
            get
            {
                foreach (var setting in this.Where(setting => setting.Caption == caption))
                    return setting;
                throw new IndexOutOfRangeException("CheckboxSettings: Specified setting \"" + caption + "\" not found");
            }
        }

        public bool IsAtLeastOneChecked { get { return this.Aggregate(false, (current, setting) => current | setting.IsChecked); } }

        public IEnumerable<NAVOTimePeriod> SelectedTimePeriods
        {
            get { return this.Where(setting => setting.IsChecked).Select(setting => setting.TimePeriod); }
        }
    }

    public class CheckboxSetting : ViewModelBase
    {
        public CheckboxSetting() { IsChecked = false; }

        #region public string Caption { get; set; }

        public string Caption
        {
            get { return TimePeriod.ToString(); }
        }

        #endregion

        public NAVOTimePeriod TimePeriod { get; set; }

        #region public bool IsChecked { get; set; }

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (_isChecked == value) return;
                _isChecked = value;
                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<CheckboxSetting>(x => x.IsChecked);
        bool _isChecked;

        #endregion
    }
}