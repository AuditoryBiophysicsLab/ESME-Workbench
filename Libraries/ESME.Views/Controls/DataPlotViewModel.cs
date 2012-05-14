using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.Controls
{
    class DataPlotViewModel : ViewModelBase
    {
        public double XMin { get; internal set; }
        public double XMax { get; internal set; }
        public double YMin { get; internal set; }
        public double YMax { get; internal set; }
        public string XAxisLabel { get; set; }
        public string YAxisLabel { get; set; }
        public ObservableList<double> XAxisMajorTicks { get; set; }
        public ObservableList<double> XAxisMinorTicks { get; set; }
        public ObservableList<double> YAxisMajorTicks { get; set; }
        public ObservableList<double> YAxisMinorTicks { get; set; }
    }
}
