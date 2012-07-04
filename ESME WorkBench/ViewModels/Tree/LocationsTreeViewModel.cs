using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Windows.Data;
using ESME;
using ESME.Locations;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class LocationsTreeViewModel : ViewModelBase
    {
        public LocationsTreeViewModel(ObservableCollection<Location> locations)
        {
            Locations = new ObservableList<LocationNode>();
            foreach (var location in locations) Locations.Add(new LocationNode(location));
            locations.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (Location location in e.NewItems) Locations.Add(new LocationNode(location));
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        foreach (Location location in e.OldItems)
                        {
                            var item = Locations.FirstOrDefault(l => l.Location.Guid == location.Guid);
                            if (item != null) Locations.Remove(item);
                        }
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        foreach (Location location in e.OldItems)
                        {
                            var item = Locations.FirstOrDefault(l => l.Location.Guid == location.Guid);
                            if (item != null) Locations.Remove(item);
                        }
                        foreach (Location location in e.NewItems) Locations.Add(new LocationNode(location));
                        break;
                    case NotifyCollectionChangedAction.Reset:
                        Locations.Clear();
                        foreach (Location location in e.NewItems) Locations.Add(new LocationNode(location));
                        break;
                }
            };
            LocationsTreeViewSource = new CollectionViewSource { Source = Locations };
            LocationsTreeViewSource.SortDescriptions.Add(new SortDescription("Location.Name", ListSortDirection.Ascending));
        }
        public CollectionViewSource LocationsTreeViewSource { get; private set; }
        public ObservableList<LocationNode> Locations { get; private set; }

        #region CreateLocationCommand
        public SimpleCommand<object, EventToCommandArgs> CreateLocationCommand { get { return _createLocation ?? (_createLocation = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.CreateLocation, true))); } }
        SimpleCommand<object, EventToCommandArgs> _createLocation;
        #endregion
    }
}
