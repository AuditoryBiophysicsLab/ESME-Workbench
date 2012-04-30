using ESME.Scenarios;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class AnalysisPointsNode : ViewModelBase
    {
        public AnalysisPointsNode(Scenario scenario) { Scenario = scenario; }

        public Scenario Scenario { get; set; }
    }
}