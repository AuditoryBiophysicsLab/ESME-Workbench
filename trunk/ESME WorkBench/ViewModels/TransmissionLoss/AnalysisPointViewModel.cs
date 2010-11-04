using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;

        public AnalysisPointViewModel(AnalysisPoint analysisPoint )
        {
            RegisterMediator();

            TransmissionLossFieldViewModels = new ObservableCollection<TransmissionLossFieldViewModel>();
            AnalysisPoint = analysisPoint;
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                //populate the list 
                foreach (var field in _analysisPoint.TransmissionLossFields)
                    TransmissionLossFieldViewModels.Add(new TransmissionLossFieldViewModel(field, null));
                if (TransmissionLossFieldViewModels.Count > 0)
                    SelectedItem = TransmissionLossFieldViewModels[0];
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
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
                if (_selectedItem != null) _selectedItem.TransmissionLossField.DiscardData();
                _selectedItem = value;
                if (_selectedItem != null)
                {
                    _selectedItem.TransmissionLossField.LoadData();
                    _selectedItem.SelectedRadial = 1;
                }
                NotifyPropertyChanged(SelectedItemChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedItemChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointViewModel>(x => x.SelectedItem);
        TransmissionLossFieldViewModel _selectedItem;

        #endregion
        
        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAnalysisPointViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            //_viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
        }
    }
}
