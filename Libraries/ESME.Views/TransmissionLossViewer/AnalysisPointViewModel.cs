using System.Linq;
using ESME.Scenarios;
using HRC.ViewModels;

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
    }
}