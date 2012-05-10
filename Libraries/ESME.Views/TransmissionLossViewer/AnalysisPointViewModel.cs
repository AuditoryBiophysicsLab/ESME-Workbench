using System.Linq;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class AnalysisPointViewModel : ViewModelBase
    {
        public AnalysisPoint AnalysisPoint { get; set; }
        public TransmissionLossViewModel TransmissionLossViewModel { get; set; }

        public AnalysisPointViewModel(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            TransmissionLossViewModel = new TransmissionLossViewModel {TransmissionLoss = analysisPoint.TransmissionLosses.First()};
        }

        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(o=>CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _close;

        #endregion
    }
}