using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.NAVO;
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
        static TimePeriodSelectionViewModel _timePeriodSelectionViewModel;

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

        #region public bool ExtractingData { get; set; }

        public bool ExtractingData
        {
            get { return _extractingData; }
            set
            {
                if (_extractingData == value) return;
                _extractingData = value;
                NotifyPropertyChanged(ExtractingDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractingDataChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentBuilderViewModel>(x => x.ExtractingData);
        bool _extractingData = false;

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
                        return  ((NAVODataSources != null) && (!ExtractingData) && 
                                (((_timePeriodSelectionViewModel != null) && (_timePeriodSelectionViewModel.MonthCheckboxes != null) && (_timePeriodSelectionViewModel.MonthCheckboxes.SelectedTimePeriods.Count() > 0)) ||
                                 ((_timePeriodSelectionViewModel != null) && (_timePeriodSelectionViewModel.SeasonCheckboxes != null) && (_timePeriodSelectionViewModel.SeasonCheckboxes.SelectedTimePeriods.Count() > 0)))); }, 
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
                        //NAVODataSources.ExtractAreas(selectedTimePeriods);
                        NAVODataSources.SelectedTimePeriods = selectedTimePeriods;
                        NAVODataSources.ExtractDataInBackground(delegate
                                                                {
                                                                    if (selectedTimePeriods.Count > 0)
                                                                    {
                                                                        var timePeriod = selectedTimePeriods[0];
                                                                        _experiment.WindSpeedFileName = NAVODataSources.WindFilename(timePeriod);
                                                                        _experiment.SoundSpeedFileName = NAVODataSources.SoundspeedFilename(timePeriod);
                                                                        _experiment.TemperatureFileName = NAVODataSources.TemperatureFilename(timePeriod);
                                                                        _experiment.SalinityFileName = NAVODataSources.SalinityFilename(timePeriod);
                                                                        _experiment.SedimentFileName = NAVODataSources.SedimentFilename(timePeriod);
                                                                        _experiment.BathymetryFileName = NAVODataSources.BathymetryFilename(timePeriod);
                                                                    }
                                                                    CloseActivePopUpCommand.Execute(true);
                                                                    ExtractingData = false;
                                                                });
                        ExtractingData = true;
                        ExtractButtonText = "Extracting...";
                        //close the view.
#if false
                        _experiment.WindSpeedFileName = NAVODataSources.SMGC.OutputFilename;
                        _experiment.SoundSpeedFileName = NAVODataSources.GDEM.OutputFilename;
                        _experiment.TemperatureFileName = NAVODataSources.GDEM.TemperatureSourceFilename;
                        _experiment.SalinityFileName = NAVODataSources.GDEM.SalinitySourceFilename;
                        _experiment.BottomTypeFileName = NAVODataSources.BST.OutputFilename;
                        _experiment.BathymetryFileName = NAVODataSources.DBDB.OutputFilename;
#endif

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
                                                                                   if (ExtractingData)
                                                                                   {
                                                                                       NAVODataSources.CancelExtraction();
                                                                                   }
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
            //NAVODataSources = new ESME.Environment.NAVO.NAVODataSources(Globals.AppSettings.NAVOConfiguration, ExtractionAreaPacket, _dispatcher);
            NAVODataSources = new ESME.Environment.NAVO.NAVODataSources(Globals.AppSettings.NAVOConfiguration, _dispatcher, _experiment.LocalStorageRoot, _experiment.North, _experiment.South, _experiment.East, _experiment.West, Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName));
            ExtractButtonText = "Extract";
        }

        #endregion

        [MediatorMessageSink(MediatorMessage.RegisterTimePeriodSelectionViewModel)]
        void RegisterTimePeriodSelectionViewModel(TimePeriodSelectionViewModel timePeriodSelectionViewModel) { _timePeriodSelectionViewModel = timePeriodSelectionViewModel; }

    }
}