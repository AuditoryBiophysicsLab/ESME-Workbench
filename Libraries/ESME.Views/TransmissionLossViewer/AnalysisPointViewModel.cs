using System.ComponentModel;
using System.Linq;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class AnalysisPointViewModel : ViewModelBase
    {
        public AnalysisPoint AnalysisPoint { get; private set; }
        public TransmissionLossViewModel TransmissionLossViewModel { get; set; }

        public AnalysisPointViewModel(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            AnalysisPoint.PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "IsDeleted")
                {
                    CloseDialog(null);
                }
            };
            TransmissionLossViewModel = new TransmissionLossViewModel { TransmissionLoss = analysisPoint.TransmissionLosses.First() };
            _oldTL = TransmissionLossViewModel.TransmissionLoss;
            _oldIndex = AnalysisPoint.TransmissionLosses.IndexOf(_oldTL);
            _oldTL.PropertyChanged += TransmissionLossChanged;
            TransmissionLossViewModel.PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "TransmissionLoss")
                {
                    if (_oldTL != null) _oldTL.PropertyChanged -= TransmissionLossChanged;
                    if (TransmissionLossViewModel.TransmissionLoss != null)
                    {
                        TransmissionLossViewModel.TransmissionLoss.PropertyChanged += TransmissionLossChanged;
                        _oldIndex = AnalysisPoint.TransmissionLosses.IndexOf(_oldTL);
                    }
                    _oldTL = TransmissionLossViewModel.TransmissionLoss;
                    
                }
            };
            

        }
        void TransmissionLossChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == "IsDeleted" && AnalysisPoint.TransmissionLosses.Count > 1)
                TransmissionLossViewModel.TransmissionLoss = AnalysisPoint.TransmissionLosses[_oldIndex % (AnalysisPoint.TransmissionLosses.Count-1)];
        }
        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _close;
        ESME.Scenarios.TransmissionLoss _oldTL;
        int _oldIndex;
        #endregion
    }
}