using System.Collections.ObjectModel;
using ESME.Locations;
using HRC.Aspects;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    [NotifyPropertyChanged]
    public class LocationsNode : ViewModelBase
    {
        public ObservableCollection<Location> Locations { get; set; }
    }
}