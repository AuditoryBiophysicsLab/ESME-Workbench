using System;
using System.Collections.Specialized;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Utility;
using HRC.ViewModels;

namespace ESME.Views.Controls
{
    class DataPlotViewModel<T> : ViewModelBase
    {
        public DataPlotViewModel() 
        {
            PropertyChanged += (s, e) =>
            {
                if (e.PropertyName == "Data" && Data != null)
                {
                    ProcessData();
                    Data.CollectionChanged += DataCollectionChanged;
                }
            };
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

        void ProcessData()
        {
            foreach (var item in Data) UpdateRanges(item);
            Render();
        }

        void UpdateRanges(T item)
        {
            if (Filter != null && !Filter(item)) return;
            var xValue = XValue(item);
            XMin = Math.Min(XMin, xValue);
            XMax = Math.Max(XMax, xValue);

            var yValue = YValue(item);
            YMin = Math.Min(YMin, yValue);
            YMax = Math.Max(YMax, yValue);
        }

        public ObservableList<T> Data { get; set; }

        /// <summary>
        /// This function should return TRUE if the item being filtered should be included in the plot
        /// </summary>
        public Func<T, bool> Filter { get; set; }
        public Func<T, double> XValue { get; set; }
        public Func<T, double> YValue { get; set; }

        void DataCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (T item in args.NewItems) UpdateRanges(item);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
            }
            Render();
        }

        class DecoratedItem<TItem>
        {
            public TItem Item { get; set; }
            public double X { get; set; }
            public double Y { get; set; }
            public bool IsPlottable { get; set; }
        }
    }
}
