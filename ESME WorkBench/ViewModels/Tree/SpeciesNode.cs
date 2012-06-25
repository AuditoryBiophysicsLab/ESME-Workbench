using System.Linq;
using ESME;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class SpeciesNode : TreeNodeBase
    {
        public SpeciesNode(Scenario scenario) { Scenario = scenario; }
        public Scenario Scenario { get; set; }

        #region AddSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> AddSpeciesCommand
        {
            get
            {
                return _addSpecies ?? (_addSpecies = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    MediatorMessage.Send(MediatorMessage.AddSpecies, Scenario);
                    OnPropertyChanged("AreAllSpeciesSeeedByESME");
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _addSpecies;
        #endregion
        #region DeleteAllSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteAllSpeciesCommand
        {
            get
            {
                return _deleteAllSpecies ?? (_deleteAllSpecies = new SimpleCommand<object, EventToCommandArgs>(o =>
                {
                    MediatorMessage.Send(MediatorMessage.DeleteAllSpecies, Scenario);
                    OnPropertyChanged("AreAllSpeciesSeeedByESME");
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _deleteAllSpecies;
        #endregion
        #region RepopulateAllSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> RepopulateAllSpeciesCommand { get { return _repopulateAllSpecies ?? (_repopulateAllSpecies = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RepopulateAllSpecies, Scenario))); } }
        SimpleCommand<object, EventToCommandArgs> _repopulateAllSpecies;
        #endregion

        public bool AreAllSpeciesSeededByESME { get { return Scenario.ScenarioSpecies.All(species => species.IsSeededByESME); } }
    }
}