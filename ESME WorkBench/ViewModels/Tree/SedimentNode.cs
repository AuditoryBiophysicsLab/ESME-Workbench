using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class SedimentNode : ViewModelBase
    {
        public SedimentNode(EnvironmentalDataSet sediment)
        {
            Sediment = sediment;
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Ooze" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Slime" });
            SedimentTypes.Add(new SedimentTypeNode { SedimentType = "Atlantis" });
        }
        public bool IsChecked
        {
            get { return Sediment.LayerSettings.IsChecked; }
            set { Sediment.LayerSettings.IsChecked = value; }
        }
        public EnvironmentalDataSet Sediment { get; set; }
        [Initialize]
        public ObservableList<SedimentTypeNode> SedimentTypes { get; set; }
    }
}