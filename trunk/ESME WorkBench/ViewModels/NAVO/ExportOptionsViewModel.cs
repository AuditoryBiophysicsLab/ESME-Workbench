using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESMEWorkBench.Data;
using Environment = ESME.Environment.Environment;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class ExportOptionsViewModel : ViewModelBase
    {
        readonly Experiment _experiment;
        readonly Dispatcher _dispatcher;

        public ExportOptionsViewModel(Experiment experiment, Dispatcher dispatcher)
        {
            _experiment = experiment;
            _dispatcher = dispatcher;
            AvailableTimePeriods = new CheckboxSettings();
            foreach (var curPeriod in _experiment.AvailableTimePeriods)
                AvailableTimePeriods.Add(new CheckboxSetting
                                         {
                                             TimePeriod = curPeriod
                                         });
            SetHeaderText();
        }

        #region public bool ExperimentHasAnalysisPoints { get; set; }

        public bool ExperimentHasAnalysisPoints
        {
            get { return _experiment.AnalysisPoints.Count > 0; }
        }

        #endregion

        #region public string TimePeriodGroupBoxHeader { get; set; }

        public string TimePeriodGroupBoxHeader
        {
            get { return _timePeriodGroupBoxHeader; }
            set
            {
                if (_timePeriodGroupBoxHeader == value) return;
                _timePeriodGroupBoxHeader = value;
                NotifyPropertyChanged(TimePeriodGroupBoxHeaderChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodGroupBoxHeaderChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.TimePeriodGroupBoxHeader);
        string _timePeriodGroupBoxHeader;

        void SetHeaderText()
        {
            if (ExportAnalysisPoints || ExportCASSClimatology)
            {
                TimePeriodGroupBoxHeader = "Select one or more available time periods";
                IsTimePeriodGroupBoxEnabled = true;
            }
            else
            {
                TimePeriodGroupBoxHeader = "Available time periods";
                IsTimePeriodGroupBoxEnabled = false;
            }
        }

        #endregion

        #region public bool IsTimePeriodGroupBoxEnabled { get; set; }

        public bool IsTimePeriodGroupBoxEnabled
        {
            get { return _isTimePeriodGroupBoxEnabled; }
            set
            {
                if (_isTimePeriodGroupBoxEnabled == value) return;
                _isTimePeriodGroupBoxEnabled = value;
                NotifyPropertyChanged(IsTimePeriodGroupBoxEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsTimePeriodGroupBoxEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.IsTimePeriodGroupBoxEnabled);
        bool _isTimePeriodGroupBoxEnabled;

        #endregion

        #region public CheckboxSettings AvailableTimePeriods { get; set; }

        public CheckboxSettings AvailableTimePeriods
        {
            get { return _availableTimePeriods; }
            set
            {
                if (_availableTimePeriods == value) return;
                _availableTimePeriods = value;
                NotifyPropertyChanged(AvailableTimePeriodsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AvailableTimePeriodsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.AvailableTimePeriods);
        CheckboxSettings _availableTimePeriods;

        int SelectedTimePeriodCount { get { return AvailableTimePeriods.Count(period => period.IsChecked); } }

        #endregion

        #region public bool ExportAnalysisPoints { get; set; }

        public bool ExportAnalysisPoints
        {
            get { return _exportAnalysisPoints; }
            set
            {
                if (_exportAnalysisPoints == value) return;
                _exportAnalysisPoints = value;
                NotifyPropertyChanged(ExportAnalysisPointsChangedEventArgs);
                SetHeaderText();
            }
        }

        static readonly PropertyChangedEventArgs ExportAnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportAnalysisPoints);
        bool _exportAnalysisPoints;

        #endregion

        #region public bool ExportCASSBathymetry { get; set; }

        public bool ExportCASSBathymetry
        {
            get { return _exportCASSBathymetry; }
            set
            {
                if (_exportCASSBathymetry == value) return;
                _exportCASSBathymetry = value;
                NotifyPropertyChanged(ExportCASSBathymetryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportCASSBathymetryChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportCASSBathymetry);
        bool _exportCASSBathymetry;

        #endregion

        #region public bool ExportCASSClimatology { get; set; }

        public bool ExportCASSClimatology
        {
            get { return _exportCASSClimatology; }
            set
            {
                if (_exportCASSClimatology == value) return;
                _exportCASSClimatology = value;
                NotifyPropertyChanged(ExportCassClimatologyChangedEventArgs);
                SetHeaderText();
            }
        }

        static readonly PropertyChangedEventArgs ExportCassClimatologyChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportCASSClimatology);
        bool _exportCASSClimatology;

        #endregion

        #region SelectAllPeriodsCommand

        SimpleCommand<object, object> _selectAllPeriods;

        public SimpleCommand<object, object> SelectAllPeriodsCommand
        {
            get { return _selectAllPeriods ?? (_selectAllPeriods = new SimpleCommand<object, object>(delegate { foreach (var period in AvailableTimePeriods) period.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllPeriodsCommand

        SimpleCommand<object, object> _unselectAllPeriods;

        public SimpleCommand<object, object> UnselectAllPeriodsCommand
        {
            get { return _unselectAllPeriods ?? (_unselectAllPeriods = new SimpleCommand<object, object>(delegate { foreach (var period in AvailableTimePeriods) period.IsChecked = false; })); }
        }

        #endregion

        #region public bool ProgressIsVisible { get; set; }

        public bool ProgressIsVisible
        {
            get { return _progressIsVisible; }
            set
            {
                if (_progressIsVisible == value) return;
                _progressIsVisible = value;
                NotifyPropertyChanged(ProgressIsVisibleChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ProgressIsVisibleChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ProgressIsVisible);
        private bool _progressIsVisible;
        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent == value) return;
                _progressPercent = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ProgressPercentChangedEventArgs));
            }
        }

        private static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ProgressPercent);
        private int _progressPercent;

        #endregion

        #region public bool NotExtractingData { get; set; }

        public bool NotExtractingData
        {
            get { return _notExtractingData; }
            set
            {
                if (_notExtractingData == value) return;
                _notExtractingData = value;
                NotifyPropertyChanged(NotExtractingDataChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs NotExtractingDataChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.NotExtractingData);
        private bool _notExtractingData;

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

        private static readonly PropertyChangedEventArgs ExtractButtonTextChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExtractButtonText);
        private string _extractButtonText = "Extract";

        #endregion

        #region public bool IsStarted { get; set; }

        public bool IsStarted
        {
            get { return _isStarted; }
            set
            {
                if (_isStarted == value) return;
                _isStarted = value;
                NotifyPropertyChanged(IsStartedChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.IsStarted);
        private bool _isStarted;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return OkIsEnabled; }, delegate
                {
                    SelectedTimePeriods = (from timePeriod in AvailableTimePeriods
                                           where timePeriod.IsChecked
                                           select (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), timePeriod.Caption)).ToList();
                    if (SelectedTimePeriods.Count > 0)
                    ExportDataInBackground(delegate
                    {
                        NotExtractingData = true;
                        ExtractButtonText = "Extract";
                        CommandManager.InvalidateRequerySuggested();
                        if (_extractionCanceled) return;
                        CloseActivePopUpCommand.Execute(true);
                    });
                    NotExtractingData = false;
                    ExtractButtonText = "Extracting...";
                    _extractionCanceled = false;
                }));
            }
        }

        private List<NAVOTimePeriod> SelectedTimePeriods { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(StatusChangedEventArgs));
            }
        }

        private string _status;
        private static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.Status);

        SimpleCommand<object, object> _ok;

        bool OkIsEnabled
        {
            get
            {
                if (ExportAnalysisPoints || ExportCASSClimatology)
                    return SelectedTimePeriodCount > 0;
                return ExportCASSBathymetry;
            }
        }

        bool _extractionCanceled;
        BackgroundWorker _backgroundWorker;
        public void ExportDataInBackground(RunWorkerCompletedEventHandler runWorkerCompletedEventHandler)
        {
            lock (this)
            {
                if (IsStarted) return;
                IsStarted = true;
                ProgressPercent = 0;
                _backgroundWorker = new BackgroundWorker
                {
                    WorkerSupportsCancellation = true,
                    WorkerReportsProgress = true,
                };
                _backgroundWorker.DoWork += ExportData;
                _backgroundWorker.RunWorkerCompleted += (s, e) =>
                {
                    IsStarted = false;
                    ProgressPercent = 100;
                    if (e.Cancelled) Status = "Canceled";
                    else if (e.Error != null)
                    {
                        Status = "Error";
                        throw new ApplicationException(string.Format("Error exporting environment data"), e.Error);
                    }
                };
                if (runWorkerCompletedEventHandler != null) _backgroundWorker.RunWorkerCompleted += runWorkerCompletedEventHandler;
                _backgroundWorker.RunWorkerAsync();
            }
        }

        int _currentExportStep;
        float _totalExportStepCount;

        void ExportData(object sender, DoWorkEventArgs args)
        {
            Delegates.Delegate<string> statusUpdateDelegate = delegate(string s)
            {
                Status = s;
                ProgressPercent = (int)((++_currentExportStep / _totalExportStepCount) * 100);
            };
            var backgroundWorker = (BackgroundWorker)sender;

            _totalExportStepCount = Environment.EnvironmentExportStepCount(SelectedTimePeriods);
            _totalExportStepCount += 2;
            statusUpdateDelegate("Initializing...");
            var environment = new Environment
            {
                Bathymetry = Bathymetry.FromYXZ(_experiment.BathymetryFileName, -1),
                Sediment = Sediment.Load(_experiment.SedimentFileName),
                SoundSpeed = SoundSpeed.Load(Path.Combine(_experiment.EnvironmentRoot, "soundspeed.xml")),
                Temperature = SoundSpeed.Load(Path.Combine(_experiment.EnvironmentRoot, "temperature.xml")),
                Salinity = SoundSpeed.Load(Path.Combine(_experiment.EnvironmentRoot, "salinity.xml")),
                Wind = Wind.Load(Path.Combine(_experiment.EnvironmentRoot, "wind.xml")),
            };
            environment.Export(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, _experiment.NemoFile.Scenario.SimAreaName), SelectedTimePeriods, null, statusUpdateDelegate, backgroundWorker);
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                {
                    if (!NotExtractingData)
                    {
                        CancelExtraction();
                        _extractionCanceled = true;
                    }
                    else
                    {
                        CloseActivePopUpCommand.Execute(false);
                    }
                }));
            }
        }

        SimpleCommand<object, object> _cancel;

        public void CancelExtraction()
        {
            if (!_backgroundWorker.IsBusy) return;
            _backgroundWorker.CancelAsync();
            Status = "Canceling, please wait...";
        }

        #endregion
    }
}