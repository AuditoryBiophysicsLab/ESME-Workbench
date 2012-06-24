using ESME;
using ESME.Scenarios;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

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

        #region AddSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> AddSpeciesCommand { get { return _addSpecies ?? (_addSpecies = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddSpecies))); } }
        SimpleCommand<object, EventToCommandArgs> _addSpecies;
        #endregion
    }
}