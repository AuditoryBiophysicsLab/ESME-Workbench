using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Windows.Controls;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("EnvironmentBuilderViewModel")]
    class ExportOptionsViewModel : ViewModelBase
    {
        public ExportOptionsViewModel(List<NAVOTimePeriod> timePeriods)
        {
            AvailableClimatologyData = new ObservableCollection<CheckBox>();
            foreach (var timePeriod in timePeriods)
            {
                AvailableClimatologyData.Add(new CheckBox
                                             {
                                                 Content = timePeriod.ToString(),
                                                 IsChecked = false,
                                             });
            }
        }

        #region public ObservableCollection<CheckBox> AvailableClimatologyData { get; set; }

        public ObservableCollection<CheckBox> AvailableClimatologyData
        {
            get { return _availableClimatologyData ?? (_availableClimatologyData = new ObservableCollection<CheckBox>()); }
            set
            {
                if (_availableClimatologyData == value) return;
                if (_availableClimatologyData != null) _availableClimatologyData.CollectionChanged -= AvailableClimatologyDataCollectionChanged;
                _availableClimatologyData = value;
                if (_availableClimatologyData != null) _availableClimatologyData.CollectionChanged += AvailableClimatologyDataCollectionChanged;
                NotifyPropertyChanged(AvailableClimatologyDataChangedEventArgs);
            }
        }

        void AvailableClimatologyDataCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(AvailableClimatologyDataChangedEventArgs); }
        static readonly PropertyChangedEventArgs AvailableClimatologyDataChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.AvailableClimatologyData);
        ObservableCollection<CheckBox> _availableClimatologyData;

        #endregion

        #region public ExportOptions ExportOptions { get; set; }

        public ExportOptions ExportOptions
        {
            get { return _exportOptions; }
            set
            {
                if (_exportOptions == value) return;
                _exportOptions = value;
                NotifyPropertyChanged(ExportOptionsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportOptionsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptionsViewModel>(x => x.ExportOptions);
        ExportOptions _exportOptions;

        #endregion

        #region OkCommand

        SimpleCommand<object, object> _okCommand;

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(
                    delegate
                    { return IsOkCommandEnabled(); },
                    delegate
                    {
                        //close the view.
                        CloseActivePopUpCommand.Execute(true);
                    }));
            }
        }

        bool IsOkCommandEnabled()
        {
            if (!ExportOptions.ExportAnalysisPoints && !ExportOptions.ExportEnvironmentToCASS) return false;
            if (ExportOptions.ExportAnalysisPoints)
                if (!ExportOptions.ExportToESME && !ExportOptions.ExportToCASS) return false;
            if (ExportOptions.ExportEnvironmentToCASS)
            {
                if (!ExportOptions.ExportCASSBathymetry && !ExportOptions.ExportCASSClimatology) return false;
                if (ExportOptions.ExportCASSClimatology)
                    return AvailableClimatologyData.Any(curTimePeriod => curTimePeriod.IsChecked.HasValue && curTimePeriod.IsChecked.Value);
            }
            return true;
        }

        #endregion

        #region CancelCommand

        SimpleCommand<object, object> _cancel;

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(false); })); }
        }

        #endregion
    }

    public class ExportOptions : PropertyChangedBase
    {
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

        static readonly PropertyChangedEventArgs ExportAnalysisPointsChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportAnalysisPoints);
        bool _exportAnalysisPoints;

        #endregion

        #region public bool ExportToESME { get; set; }

        public bool ExportToESME
        {
            get { return _exportToESME; }
            set
            {
                if (_exportToESME == value) return;
                _exportToESME = value;
                NotifyPropertyChanged(ExportToESMEChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportToESMEChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportToESME);
        bool _exportToESME;

        #endregion

        #region public bool ExportToCASS { get; set; }

        public bool ExportToCASS
        {
            get { return _exportToCASS; }
            set
            {
                if (_exportToCASS == value) return;
                _exportToCASS = value;
                NotifyPropertyChanged(ExportToCASSChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportToCASSChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportToCASS);
        bool _exportToCASS;

        #endregion

        #region public bool ExportEnvironmentToCASS { get; set; }

        public bool ExportEnvironmentToCASS
        {
            get { return _exportEnvironmentToCASS; }
            set
            {
                if (_exportEnvironmentToCASS == value) return;
                _exportEnvironmentToCASS = value;
                NotifyPropertyChanged(ExportEnvironmentToCASSChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportEnvironmentToCASSChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportEnvironmentToCASS);
        bool _exportEnvironmentToCASS;

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

        static readonly PropertyChangedEventArgs ExportCASSBathymetryChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportCASSBathymetry);
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
                NotifyPropertyChanged(ExportCASSClimatologyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportCASSClimatologyChangedEventArgs = ObservableHelper.CreateArgs<ExportOptions>(x => x.ExportCASSClimatology);
        bool _exportCASSClimatology;

        #endregion
    }
}
