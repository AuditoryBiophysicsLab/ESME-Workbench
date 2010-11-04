using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointViewModel : ViewModelBase 
    {
        public AnalysisPointViewModel(AnalysisPoint analysisPoint )
        {
            AnalysisPoint = analysisPoint;
            TransmissionLossFieldViewModels = new ObservableCollection<TransmissionLossFieldViewModel>();
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
                //populate the list 
                foreach (var field in _analysisPoint.TransmissionLossFields)
                    TransmissionLossFieldViewModels.Add(new TransmissionLossFieldViewModel(field, null));
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        #region public ObservableCollection<TransmissionLossFieldViewModel> TransmissionLossFieldViewModels { get; set; }

        public ObservableCollection<TransmissionLossFieldViewModel> TransmissionLossFieldViewModels
        {
            get { return _transmissionLossFieldViewModels; }
            set
            {
                if (_transmissionLossFieldViewModels == value) return;
                if (_transmissionLossFieldViewModels != null) _transmissionLossFieldViewModels.CollectionChanged -= TransmissionLossFieldViewModelsCollectionChanged;
                _transmissionLossFieldViewModels = value;
                if (_transmissionLossFieldViewModels != null) _transmissionLossFieldViewModels.CollectionChanged += TransmissionLossFieldViewModelsCollectionChanged;
                NotifyPropertyChanged(TransmissionLossFieldViewModelsChangedEventArgs);
            }
        }

        void TransmissionLossFieldViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(TransmissionLossFieldViewModelsChangedEventArgs); }
        static readonly PropertyChangedEventArgs TransmissionLossFieldViewModelsChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.TransmissionLossFieldViewModels);
        ObservableCollection<TransmissionLossFieldViewModel> _transmissionLossFieldViewModels;

        #endregion

        #region public TransmissionLossFieldViewModel SelectedItem { get; set; }

        public TransmissionLossFieldViewModel SelectedItem
        {
            get { return _selectedItem; }
            set
            {
                if (_selectedItem == value) return;
                _selectedItem = value;
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.SelectedItem);
        TransmissionLossFieldViewModel _selectedItem;

        #endregion


    }
}
