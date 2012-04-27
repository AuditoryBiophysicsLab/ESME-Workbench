using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    [NotifyPropertyChanged]
    public class SpeciesNode : ViewModelBase
    {
        public Scenario Scenario { get; set; }
    }
}