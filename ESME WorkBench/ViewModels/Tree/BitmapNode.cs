using ESME.Locations;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class BitmapNode : ViewModelBase
    {
        public BitmapNode(string layerName, EnvironmentalDataSet bitmapData)
        {
            LayerName = layerName;
            BitmapData = bitmapData;
        }

        public string LayerName { get; private set; }
        public bool IsChecked
        {
            get { return BitmapData.LayerSettings.IsChecked; }
            set { BitmapData.LayerSettings.IsChecked = value; }
        }

        public EnvironmentalDataSet BitmapData { get; set; }
    }
}