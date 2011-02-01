using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
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


        public EnvironmentBuilderViewModel(IUIVisualizerService visualizerService, AppSettings appSettings, Experiment experiment)
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
            AppSettings = appSettings;
            Experiment = experiment;
            var area = new NAVOExtractionPacket
            {
                Filename = Path.Combine(Experiment.LocalStorageRoot, "temp.xml"),
                North = Experiment.North,
                South = Experiment.South,
                East = Experiment.East,
                West = Experiment.West,
            };
            NAVODataSources = new ESME.Environment.NAVO.NAVODataSources(Globals.AppSettings.NAVOConfiguration, area);
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
        }

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
            get { return _nAVODataSources; }
            set
            {
                if (_nAVODataSources == value) return;
                _nAVODataSources = value;
                NotifyPropertyChanged(NAVODataSourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAVODataSourcesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.NAVODataSources);
        ESME.Environment.NAVO.NAVODataSources _nAVODataSources;

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
                return _extractAll ?? (_extractAll = new SimpleCommand<object, object>(delegate
                                                                                       {
                                                                                           AppSettings.Save(); //remember the new values. 
                                                                                           //set the times
                                                                                           
                                                                                           //extract data from all data sources.
                                                                                           
                                                                                           
                                                                                           ((EnvironmentBuilderView)_viewAwareStatus.View).Close();
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

        public Experiment Experiment { get; set; }

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }

        #endregion

        void SetTimes() { MediatorMessage.Send(MediatorMessage.SeasonsDefined); }

        //new mediator message sink for actually extracitng area -- will wind up also getting called when some idiot changes the boundaries.
        void ExtractArea()
        {
            var area = new NAVOExtractionPacket
                       {
                           Filename = Path.Combine(Experiment.LocalStorageRoot, "temp.xml"),
                           North = Experiment.North,
                           South = Experiment.South,
                           East = Experiment.East,
                           West = Experiment.West,
                       };
            MediatorMessage.Send(MediatorMessage.ExtractBST, area);
            MediatorMessage.Send(MediatorMessage.ExtractDBDB, area);
            MediatorMessage.Send(MediatorMessage.ExtractGDEM, area);
            MediatorMessage.Send(MediatorMessage.ExtractSMGC, area);
        }
    }
}