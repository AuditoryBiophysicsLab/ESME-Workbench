using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using Cinch;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("TimePeriodSelectionViewModel")]
    internal class TimePeriodSelectionViewModel : ViewModelBase
    {
        readonly IUIVisualizerService _visualizerService;

        [ImportingConstructor]
        public TimePeriodSelectionViewModel(IViewAwareStatus viewAwareStatus, IUIVisualizerService visualizerService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTimePeriodSelectionViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _visualizerService = visualizerService;
            viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.RegisterTimePeriodSelectionViewModel, this);

            MonthCheckboxes = new CheckboxSettings
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
            SeasonCheckboxes = new CheckboxSettings
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
        }

        #region LaunchSeasonConfigurationViewCommand

        SimpleCommand<object, object> _launchSeasonConfigurationView;

        public SimpleCommand<object, object> LaunchSeasonConfigurationViewCommand
        {
            get { return _launchSeasonConfigurationView ?? (_launchSeasonConfigurationView = new SimpleCommand<object, object>(delegate
                                                                                                                               { 
                                                                                                                                   var popupViewModel = new SeasonConfigurationWindowViewModel();
                                                                                                                                   _visualizerService.ShowDialog("SeasonConfigurationWindowView", popupViewModel);
                                                                                                                               })); }
        }

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

        static readonly PropertyChangedEventArgs SelectedPeriodsChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.SelectedPeriods);
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
        static readonly PropertyChangedEventArgs MonthCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.MonthCheckboxes);
        CheckboxSettings _monthCheckboxes;

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
        static readonly PropertyChangedEventArgs SeasonCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.SeasonCheckboxes);
        CheckboxSettings _seasonCheckboxes;

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