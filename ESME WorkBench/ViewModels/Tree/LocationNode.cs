using System.Collections.ObjectModel;
using ESME;
using ESME.Locations;
using HRC.Navigation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class LocationNode : ViewModelBase
    {
        public LocationNode(Location location) 
        {
            Location = location;
            Children = new ObservableCollection<object>
            {
                new LocationScenarioNode(location),
                new LocationEnvironmentNode { Location = Location }
            };
        }

        public Location Location { get; private set; }
        public ObservableCollection<object> Children { get; private set; }

        #region ZoomToLocationCommand
        public SimpleCommand<object, EventToCommandArgs> ZoomToLocationCommand { get { return _zoomToLocation ?? (_zoomToLocation = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SetMapExtent, (GeoRect)Location.GeoRect))); } }
        SimpleCommand<object, EventToCommandArgs> _zoomToLocation;
        #endregion

        #region DeleteLocationCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteLocationCommand { get { return _deleteLocation ?? (_deleteLocation = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteLocation, Location))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteLocation;
        #endregion
    }
}