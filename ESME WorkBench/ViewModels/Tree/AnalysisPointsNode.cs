using ESME;
using ESME.Scenarios;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class AnalysisPointsNode : ViewModelBase, IMouseOverAware
    {
        public AnalysisPointsNode(Scenario scenario) { Scenario = scenario; }

        public Scenario Scenario { get; set; }

        public bool IsMouseOver
        {
            get { throw new System.NotImplementedException(); }
            set { throw new System.NotImplementedException(); }
        }
    }
}