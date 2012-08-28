using System.Collections.ObjectModel;
using System.Windows.Media;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public class TwoAxisSeriesViewModel : ViewModelBase
    {
        public TwoAxisSeriesViewModel() 
        {
            XAxis.Label = "X Axis";
            YAxis.Label = "Y Axis";
            XAxis.Autorange = true;
            YAxis.Autorange = true;
        }
        [Initialize] public DataAxisViewModel XAxis { get; set; }
        [Initialize] public DataAxisViewModel YAxis { get; set; }
        public bool ShowXAxisMajorTicks { get { return XAxis.ShowMajorTicks; } set { XAxis.ShowMajorTicks = value; } }
        public bool ShowXAxisMinorTicks { get { return XAxis.ShowMinorTicks; } set { XAxis.ShowMinorTicks = value; } }
        public bool ShowYAxisMajorTicks { get { return YAxis.ShowMajorTicks; } set { YAxis.ShowMajorTicks = value; } }
        public bool ShowYAxisMinorTicks { get { return YAxis.ShowMinorTicks; } set { YAxis.ShowMinorTicks = value; } }
        public ObservableCollection<ISeries> SeriesSource { get; set; }
        public Color MajorTickLineColor { get; set; }
        public Color MinorTickLineColor { get; set; }
    }

    public class DataAxisViewModel : ViewModelBase
    {
        public string Label { get; set; }
        public AxisType AxisType { get; set; }
        public double StartValue { get; set; }
        public double EndValue { get; set; }
        public bool ShowMajorTicks { get; set; }
        public bool ShowMinorTicks { get; set; }
        ObservableList<AxisTick> _majorTicks;
        public ObservableList<AxisTick> MajorTicks { get { return ShowMajorTicks ? _majorTicks : null; } set { _majorTicks = value; } }
        ObservableList<AxisTick> _minorTicks;
        public ObservableList<AxisTick> MinorTicks { get { return ShowMinorTicks ? _minorTicks : null; } set { _minorTicks = value; } }
        public string TickValueFormat { get; set; }
        public bool Autorange { get; set; }
    }
}
