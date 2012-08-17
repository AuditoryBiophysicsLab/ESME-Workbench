using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Controls
{
    class DataPlotViewModel : ViewModelBase
    {
        public DataPlotViewModel() 
        {
            Render();    
        }
        [Initialize(100)] 
        public double XMin { get; internal set; }

        [Initialize(200)] 
        public double XMax { get; internal set; }
        
        [Initialize(1)] 
        public double YMin { get; internal set; }
        
        [Initialize(10000)] 
        public double YMax { get; internal set; }
        
        [Initialize(AxisType.Logarithmic)] 
        public AxisType YAxisType { get; set; }
        
        [Initialize(AxisType.Linear)]
        public AxisType XAxisType { get; set; }
        
        public string XAxisLabel { get; set; }
        public string YAxisLabel { get; set; }
        
        public ObservableList<AxisTick> XAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> XAxisMinorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> YAxisMinorTicks { get; set; }

        public DataPlotView View { get; set; }
        [Initialize("M 0,0")]
        public string MajorGrid { get; private set; }
        [Initialize("M 0,0")]
        public string MinorGrid { get; private set; }
        void Render()
        {
            var height = View.PlotSurface.ActualHeight;
            var width = View.PlotSurface.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (height == 0 || width == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            MajorGrid = DataAxis.GetGrid(YAxisMajorTicks, XAxisMajorTicks, 1, height, width);
            MinorGrid = DataAxis.GetGrid(YAxisMinorTicks, XAxisMinorTicks, 0, height, width);
        }
    }
}
