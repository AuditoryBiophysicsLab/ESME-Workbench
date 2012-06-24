using ESME.Scenarios;
using HRC.Utility;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class SpeciesNode : TreeNodeBase
    {
        public SpeciesNode() { }
        public SpeciesNode(Scenario scenario)
        {
            Scenario = scenario;
            SpeciesList = scenario.ScenarioSpecies;
        }

        public Scenario Scenario { get; set; }
        public ObservableList<ScenarioSpecies> SpeciesList { get; set; }
    }
}