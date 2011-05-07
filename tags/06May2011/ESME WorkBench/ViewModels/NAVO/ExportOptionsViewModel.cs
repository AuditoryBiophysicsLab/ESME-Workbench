using System.ComponentModel;
using System.Linq;
using Cinch;
using ESMEWorkBench.Data;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class ExportOptionsViewModel : ViewModelBase
    {
        readonly Experiment _experiment;

        public ExportOptionsViewModel(Experiment experiment)
        {
            _experiment = experiment;
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

        static readonly PropertyChangedEventArgs ExperimentHasAnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExperimentHasAnalysisPoints);
        bool _experimentHasAnalysisPoints;

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

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.Status);
        string _status;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return OkIsEnabled; }, delegate { CloseActivePopUpCommand.Execute(true); })); }
        }

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

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(false); })); }
        }

        SimpleCommand<object, object> _cancel;

        #endregion
    }
}