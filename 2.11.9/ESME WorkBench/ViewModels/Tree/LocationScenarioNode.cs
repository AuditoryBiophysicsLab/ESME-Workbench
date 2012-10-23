using System.ComponentModel;
using System.Windows.Data;
using ESME;
using ESME.Locations;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class LocationScenarioNode : ViewModelBase
    {
        public LocationScenarioNode(Location location)
        {
            Location = location;
            ScenariosViewSource = new CollectionViewSource { Source = location.Scenarios };
            ScenariosViewSource.SortDescriptions.Add(new SortDescription("Name", ListSortDirection.Ascending));
            ScenariosViewSource.View.Refresh();
        }

        public Location Location { get; set; }
        public CollectionViewSource ScenariosViewSource { get; private set; }

        #region CreateScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> CreateScenarioCommand { get { return _createScenario ?? (_createScenario = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.CreateScenario, Location))); } }
        SimpleCommand<object, EventToCommandArgs> _createScenario;
        #endregion

        #region DeleteAllScenariosCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteAllScenariosCommand { get { return _deleteAllScenarios ?? (_deleteAllScenarios = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteAllScenarios, Location))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteAllScenarios;
        #endregion
    }
}