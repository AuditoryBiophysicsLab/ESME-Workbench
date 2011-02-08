﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
using ESME.Views;
using ESMEWorkBench.Data;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("EnvironmentBuilderViewModel")]
    public class EnvironmentBuilderViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly IUIVisualizerService _visualizerService;
        Dispatcher _dispatcher;
        IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBoxService;
        readonly Experiment _experiment;
        TimePeriodSelectionViewModel _timePeriodSelectionViewModel;

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
            Months = new List<NAVOTimePeriod>
                     {
                         NAVOTimePeriod.January,
                         NAVOTimePeriod.February,
                         NAVOTimePeriod.March,
                         NAVOTimePeriod.April,
                         NAVOTimePeriod.May,
                         NAVOTimePeriod.June,
                         NAVOTimePeriod.July,
                         NAVOTimePeriod.August,
                         NAVOTimePeriod.September,
                         NAVOTimePeriod.October,
                         NAVOTimePeriod.November,
                         NAVOTimePeriod.December,
                         NAVOTimePeriod.Spring,
                         NAVOTimePeriod.Summer,
                         NAVOTimePeriod.Fall,
                         NAVOTimePeriod.Winter,
                         NAVOTimePeriod.Cold,
                         NAVOTimePeriod.Warm,
                     };
            ExtractionAreaPacket= new NAVOExtractionPacket
            {
                Filename = _experiment.LocalStorageRoot,
                North = _experiment.North,
                South = _experiment.South,
                East = _experiment.East,
                West = _experiment.West,
                //TimePeriod = SelectedTimePeriod,
            };
            
            NAVODataSources = new ESME.Environment.NAVO.NAVODataSources(Globals.AppSettings.NAVOConfiguration, ExtractionAreaPacket);
           
        }

        #region public NAVOExtractionPacket ExtractionAreaPacket { get; set; }

        public NAVOExtractionPacket ExtractionAreaPacket
        {
            get { return _extractionAreaPacket; }
            set
            {
                if (_extractionAreaPacket == value) return;
                _extractionAreaPacket = value;
                NotifyPropertyChanged(ExtractionAreaPacketChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractionAreaPacketChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExtractionAreaPacket);
        NAVOExtractionPacket _extractionAreaPacket;

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

        public ESME.Environment.NAVO.NAVODataSources NAVODataSources
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
        ESME.Environment.NAVO.NAVODataSources _navoDataSources;

        #endregion

        #region public List<NAVOTimePeriod> Months { get; set; }

        public List<NAVOTimePeriod> Months
        {
            get { return _months; }
            set
            {
                if (_months == value) return;
                _months = value;
                NotifyPropertyChanged(MonthsChangedEventArgs);
                SelectedTimePeriod = Months[0];
            }
        }

        static readonly PropertyChangedEventArgs MonthsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.Months);
        List<NAVOTimePeriod> _months;

        #endregion

        #region public NAVOTimePeriod SelectedTimePeriod { get; set; }

        public NAVOTimePeriod SelectedTimePeriod
        {
            get { return _selectedTimePeriod; }
            set
            {
                if (_selectedTimePeriod == value) return;
                _selectedTimePeriod = value;
                NotifyPropertyChanged(SelectedTimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.SelectedTimePeriod);
        NAVOTimePeriod _selectedTimePeriod;

        #endregion

        #region public NAVOTimePeriod StartTime { get; set; }

        public NAVOTimePeriod StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.StartTime);
        NAVOTimePeriod _startTime;

        #endregion

        #region public NAVOTimePeriod EndTime { get; set; }

        public NAVOTimePeriod EndTime
        {
            get { return _endTime; }
            set
            {
                if (_endTime == value) return;
                _endTime = value;
                NotifyPropertyChanged(EndTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EndTimeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.EndTime);
        NAVOTimePeriod _endTime;

        #endregion

        #region LaunchEnvironmentConfigurationViewCommand

        SimpleCommand<object, object> _launchEnvironmentConfigurationView;

        public SimpleCommand<object, object> LaunchEnvironmentConfigurationViewCommand
        {
            get
            {
                return _launchEnvironmentConfigurationView ?? (_launchEnvironmentConfigurationView = new SimpleCommand<object, object>(delegate
                                                                                                                                       {
                                                                                                                                           var environmentBuilderConfigurationViewModel = new EnvironmentBuilderConfigurationViewModel(AppSettings);
                                                                                                                                           bool? result = _visualizerService.ShowDialog("EnvironmentBuilderConfigurationView", environmentBuilderConfigurationViewModel);
                                                                                                                                           if (result.HasValue && result.Value) { }
                                                                                                                                       }));
            }
        }

        #endregion

        #region ExtractAllCommand

        SimpleCommand<object, object> _extractAll;

        public SimpleCommand<object, object> ExtractAllCommand
        {
            get
            {
                return _extractAll ?? (_extractAll = new SimpleCommand<object, object>(
                    delegate { return (((_timePeriodSelectionViewModel != null) && (_timePeriodSelectionViewModel.MonthCheckboxes != null) && (_timePeriodSelectionViewModel.MonthCheckboxes.SelectedTimePeriods.Count() > 0)) ||
                                       ((_timePeriodSelectionViewModel != null) && (_timePeriodSelectionViewModel.SeasonCheckboxes != null) && (_timePeriodSelectionViewModel.SeasonCheckboxes.SelectedTimePeriods.Count() > 0))); }, 
                    delegate
                    {
                        AppSettings.Save(); //remember the new values. 
                        var selectedTimePeriods = new List<NAVOTimePeriod>();
                        if (_timePeriodSelectionViewModel != null)
                        {
                            if (_timePeriodSelectionViewModel.MonthCheckboxes != null)
                                selectedTimePeriods.AddRange(_timePeriodSelectionViewModel.MonthCheckboxes.SelectedTimePeriods);
                            if (_timePeriodSelectionViewModel.SeasonCheckboxes != null)
                                selectedTimePeriods.AddRange(_timePeriodSelectionViewModel.SeasonCheckboxes.SelectedTimePeriods);
                        }
                        if (selectedTimePeriods.Count < 1) return;
                        //extract data from all data sources.
                        NAVODataSources.ExtractAreas(selectedTimePeriods);
                        //close the view.
#if false
                        _experiment.WindSpeedFileName = NAVODataSources.SMGC.OutputFilename;
                        _experiment.SoundSpeedFileName = NAVODataSources.GDEM.OutputFilename;
                        _experiment.TemperatureFileName = NAVODataSources.GDEM.TemperatureSourceFilename;
                        _experiment.SalinityFileName = NAVODataSources.GDEM.SalinitySourceFilename;
                        _experiment.BottomTypeFileName = NAVODataSources.BST.OutputFilename;
                        _experiment.BathymetryFileName = NAVODataSources.DBDB.OutputFilename;
#endif
                        CloseActivePopUpCommand.Execute(true);

                        //((EnvironmentBuilderView)_viewAwareStatus.View).Close();
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
                                                                                   AppSettings.Reload(); //invalidate all changes.
                                                                                   CloseActivePopUpCommand.Execute(false);
                                                                               }));
            }
        }

        #endregion
        

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }

        #endregion

        void InterpretTimes()
        {
            switch (SelectedTimePeriod)
            {
                case NAVOTimePeriod.January:
                case NAVOTimePeriod.February:
                case NAVOTimePeriod.March:
                case NAVOTimePeriod.April:
                case NAVOTimePeriod.May:
                case NAVOTimePeriod.June:
                case NAVOTimePeriod.July:
                case NAVOTimePeriod.August:
                case NAVOTimePeriod.September:
                case NAVOTimePeriod.October:
                case NAVOTimePeriod.November:
                case NAVOTimePeriod.December:
                    StartTime = SelectedTimePeriod;
                    EndTime = SelectedTimePeriod;
                    break;
                case NAVOTimePeriod.Spring:
                    StartTime = Globals.AppSettings.NAVOConfiguration.SpringStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.SpringStartMonth + 3; //really?
                    break;
                case NAVOTimePeriod.Summer:
                    StartTime = Globals.AppSettings.NAVOConfiguration.SummerStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.SummerStartMonth + 3;
                    break;
                case NAVOTimePeriod.Fall:
                    StartTime = Globals.AppSettings.NAVOConfiguration.FallStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.FallStartMonth + 3;
                    break;
                case NAVOTimePeriod.Winter:
                    StartTime = Globals.AppSettings.NAVOConfiguration.WinterStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.WinterStartMonth + 3;
                    break;
                case NAVOTimePeriod.Cold:
                    StartTime = Globals.AppSettings.NAVOConfiguration.ColdSeasonStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.ColdSeasonStartMonth + 3;
                    break;
                case NAVOTimePeriod.Warm:
                    StartTime = Globals.AppSettings.NAVOConfiguration.WarmSeasonStartMonth;
                    EndTime = Globals.AppSettings.NAVOConfiguration.WarmSeasonStartMonth + 3;
                    break;
            }

        }

        [MediatorMessageSink(MediatorMessage.RegisterTimePeriodSelectionViewModel)]
        void RegisterTimePeriodSelectionViewModel(TimePeriodSelectionViewModel timePeriodSelectionViewModel) { _timePeriodSelectionViewModel = timePeriodSelectionViewModel; }

    }
}