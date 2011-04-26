using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using ESME.TransmissionLoss.CASS;
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
        }

        #region public bool ExperimentHasAnalysisPoints { get; set; }

        public bool ExperimentHasAnalysisPoints
        {
            get { return _experiment.AnalysisPoints.Count > 0; }
        }

        static readonly PropertyChangedEventArgs ExperimentHasAnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExperimentHasAnalysisPoints);
        bool _experimentHasAnalysisPoints;

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
            }
        }

        static readonly PropertyChangedEventArgs ExportAnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportAnalysisPoints);
        bool _exportAnalysisPoints;

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

        static readonly PropertyChangedEventArgs ExportCASSDataChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportCASSData);
        bool _exportCASSData;

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
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                       {
                                                                           CloseActivePopUpCommand.Execute(true);
                                                                       }));
            }
        }

        SimpleCommand<object, object> _ok;

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