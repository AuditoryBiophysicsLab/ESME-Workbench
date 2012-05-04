using System.Linq;
using System.Windows;
using ESME.Scenarios;
using HRC.ViewModels;

namespace ESME.Views.TransmissionLossViewer
{
    public class AnalysisPointViewModel
    {
        public AnalysisPoint AnalysisPoint { get; set; }
        public TransmissionLossViewModel TransmissionLossViewModel { get; set; }

        public AnalysisPointViewModel(AnalysisPoint analysisPoint)
        {
            AnalysisPoint = analysisPoint;
            TransmissionLossViewModel = new TransmissionLossViewModel { TransmissionLoss = analysisPoint.TransmissionLosses.First()};
        }
    }
}
