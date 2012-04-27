using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    [NotifyPropertyChanged]
    public class AnalysisPointsNode : ViewModelBase
    {
        public AnalysisPointsNode(Scenario scenario) { Scenario = scenario; }

        public Scenario Scenario { get; set; }
    }
}