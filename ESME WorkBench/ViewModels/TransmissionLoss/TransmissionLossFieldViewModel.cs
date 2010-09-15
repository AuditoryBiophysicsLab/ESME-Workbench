using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossFieldViewModel : ViewModelBase
    {
        IViewAwareStatus _viewAwareStatusService;

        public TransmissionLossFieldViewModel(string fileName, IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatusService = viewAwareStatusService;
            TransmissionLossField = TransmissionLossField.Load(fileName);
            ColorMapViewModel = new ColorMapViewModel();

            TransmissionLossRadialViewModel = new TransmissionLossRadialViewModel(TransmissionLossField.Radials[0], ColorMapViewModel);
        }

        public TransmissionLossField TransmissionLossField { get; private set; }

        public ColorMapViewModel ColorMapViewModel { get; private set; }

        static readonly PropertyChangedEventArgs TransmissionLossRadialViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.TransmissionLossRadialViewModel);
        TransmissionLossRadialViewModel _transmissionLossRadialViewModel;
        public TransmissionLossRadialViewModel TransmissionLossRadialViewModel {
            get { return _transmissionLossRadialViewModel; } 
            private set
            {
                if (value == _transmissionLossRadialViewModel) return;
                _transmissionLossRadialViewModel = value;
                NotifyPropertyChanged(TransmissionLossRadialViewModelChangedEventArgs);
                ColorMapViewModel.MaxValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMin;
            }
        }
    }
}