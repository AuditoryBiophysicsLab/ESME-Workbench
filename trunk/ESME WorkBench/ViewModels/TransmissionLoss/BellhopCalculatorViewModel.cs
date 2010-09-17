using System.ComponentModel;
using System.Threading;
using Cinch;
using ESME;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class BellhopCalculatorViewModel : ViewModelBase, IHasLog
    {
        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.TransmissionLossField);
        static readonly PropertyChangedEventArgs LogChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.Log);

        string _log;
        TransmissionLossField _transmissionLossField;

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            private set
            {
                if (value == _transmissionLossField) return;
                _transmissionLossField = value;
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
            }
        }

        #region IHasLog Members

        public string Log
        {
            get { return _log; }
            set
            {
                if (value == _log) return;
                _log = value;
                NotifyPropertyChanged(LogChangedEventArgs);
                System.Diagnostics.Debug.Write(_log);
            }
        }

        #endregion

        public void Calculate(BellhopRunFile bellhopRunFile)
        {
            TransmissionLossField = FieldCalculator.ComputeField(bellhopRunFile, this);
            Thread.Sleep(2000);
            CloseActivePopUpCommand.Execute(true);
        }
    }
}