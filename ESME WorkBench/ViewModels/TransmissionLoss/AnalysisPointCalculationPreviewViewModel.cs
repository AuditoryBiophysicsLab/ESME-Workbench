using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointCalculationPreviewViewModel : ViewModelBase
    {
        #region public constructor

        public AnalysisPointCalculationPreviewViewModel()
        {
            TransmissionLossJobViewModels = new ObservableCollection<TransmissionLossJobViewModel>();
            BellhopRunFiles = new ObservableCollection<BellhopRunFile>();
        }

        #endregion

        #region public ObservableCollection<TransmissionLossJobViewModel> TransmissionLossJobViewModels { get; set; }

        public ObservableCollection<TransmissionLossJobViewModel> TransmissionLossJobViewModels
        {
            get { return _transmissionLossJobViewModels; }
            set
            {
                if (_transmissionLossJobViewModels == value) return;
                if (_transmissionLossJobViewModels != null) _transmissionLossJobViewModels.CollectionChanged -= TransmissionLossJobViewModelsCollectionChanged;
                _transmissionLossJobViewModels = value;
                if (_transmissionLossJobViewModels != null) _transmissionLossJobViewModels.CollectionChanged += TransmissionLossJobViewModelsCollectionChanged;
                NotifyPropertyChanged(TransmissionLossJobViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossJobViewModelsChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointCalculationPreviewViewModel>(x => x.TransmissionLossJobViewModels);
        ObservableCollection<TransmissionLossJobViewModel> _transmissionLossJobViewModels;

        void TransmissionLossJobViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null) foreach (var newItem in e.NewItems.Cast<TransmissionLossJobViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<TransmissionLossJobViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            NotifyPropertyChanged(TransmissionLossJobViewModelsChangedEventArgs);
        }

        #endregion

        #region public ObservableCollection<BellhopRunFile> BellhopRunFiles { get; set; }

        public ObservableCollection<BellhopRunFile> BellhopRunFiles
        {
            get { return _bellhopRunFiles; }
            set
            {
                if (_bellhopRunFiles == value) return;
                if (_bellhopRunFiles != null) _bellhopRunFiles.CollectionChanged -= BellhopRunFilesCollectionChanged;
                _bellhopRunFiles = value;
                if (_bellhopRunFiles != null) _bellhopRunFiles.CollectionChanged += BellhopRunFilesCollectionChanged;
                NotifyPropertyChanged(BellhopRunFilesChangedEventArgs);
            }
        }

        void BellhopRunFilesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BellhopRunFilesChangedEventArgs); }
        static readonly PropertyChangedEventArgs BellhopRunFilesChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointCalculationPreviewViewModel>(x => x.BellhopRunFiles);
        ObservableCollection<BellhopRunFile> _bellhopRunFiles;

        #endregion

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointCalculationPreviewViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        #region public bool IsValid { get; set; }

        public bool IsValid { get { return TransmissionLossJobViewModels.All(transmissionLossJobViewModel => transmissionLossJobViewModel.IsValid); } }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointCalculationPreviewViewModel>(x => x.IsValid);

        #endregion

        #region OKCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(delegate { return IsValid; }, delegate { CloseActivePopUpCommand.Execute(true); })); }
        }

        SimpleCommand<object, object> _okCommand;

        #endregion
    }
}
