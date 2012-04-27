using ESME.Scenarios;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    [NotifyPropertyChanged]
    public class ScenarioNode : ViewModelBase
    {
        public Scenario Scenario { get; private set; }

        public ScenarioNode(Scenario scenario) 
        {
            Scenario = scenario;
            Children.Add(scenario);
            Children.Add(new AnalysisPointsNode(scenario));
            Children.Add(new PerimetersNode { Scenario = scenario });
            Children.Add(new SpeciesNode { Scenario = scenario });
            Children.Add(new EnvironmentNode(scenario));
        }

        [Initialize] public ObservableList<object> Children { get; set; }
    }
}