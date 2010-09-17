using System;
using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossJobViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs TransmissionLossJobChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.TransmissionLossJob);

        TransmissionLossJob _transmissionLossJob;
        public TransmissionLossJobViewModel() { OkCommand = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(true); }); }

        public TransmissionLossJob TransmissionLossJob
        {
            get { return _transmissionLossJob; }
            set
            {
                if (value == _transmissionLossJob) return;
                _transmissionLossJob = value;
                NotifyPropertyChanged(TransmissionLossJobChangedEventArgs);
            }
        }

        public SimpleCommand<Object, Object> OkCommand { get; private set; }
    }
}