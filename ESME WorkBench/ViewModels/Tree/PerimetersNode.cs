using ESME.Scenarios;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class PerimetersNode : TreeNodeBase
    {
        public PerimetersNode() {}
        public PerimetersNode(Scenario scenario) { Scenario = scenario; }
        public Scenario Scenario { get; set; }
    }
}