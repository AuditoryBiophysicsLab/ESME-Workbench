using System.Collections.ObjectModel;
using ESME.Locations;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class LocationsTreeViewModel : ViewModelBase
    {
        public LocationsTreeViewModel(ObservableCollection<Location> locations)
        {
            Locations = locations;
        }
        public ObservableCollection<Location> Locations { get; private set; }

    }
}
