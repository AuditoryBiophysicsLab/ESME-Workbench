using ESME.Scenarios;
using HRC.Aspects;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    [NotifyPropertyChanged]
    public class PerimetersNode : ViewModelBase
    {
        public Scenario Scenario { get; set; }
    }
}