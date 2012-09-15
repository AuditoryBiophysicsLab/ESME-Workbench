using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Windows;
using HRC.Aspects;
using HRC.Plotting.AxisLabeling.Layout;
using HRC.ViewModels;

namespace HRC.Plotting
{
    public class DataAxisViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _propertyObserver;
        public DataAxisViewModel()
        {
            _propertyObserver = new PropertyObserver<DataAxisViewModel>(this)
                .RegisterHandler(d => d.VisibleRange, () => Debug.WriteLine(string.Format("{0} VisibleRange changed, new value: {1}", Label, VisibleRange == null ? "(null)" : VisibleRange.ToString())))
                .RegisterHandler(d => d.ActualHeight, () => Debug.WriteLine(string.Format("{0} ActualHeight changed, new value: {1}", Label, ActualHeight)))
                .RegisterHandler(d => d.ActualHeight, () => Debug.WriteLine(string.Format("{0} ActualWidth changed, new value: {1}", Label, ActualWidth)));
            VisibleRange.RangeChanged += (sender, args) => Debug.WriteLine(string.Format("{0} VisibleRange RangeChanged, new value: {1}", Label, VisibleRange == null ? "(null)" : VisibleRange.ToString()));
        }
        [Initialize("Axis")] public string Label { get; set; }
        [Initialize(AxisLayoutAlgorithm.ExtendedWilkinson)] public AxisLayoutAlgorithm AxisLayoutAlgorithm { get; set; }
        [Initialize] public ObservableCollection<NewDataAxisTick> AxisTicks { get; set; }
        [Initialize(AxisType.Linear)] public AxisType AxisType { get; set; }
        [Initialize] public RangeCollection DataRange { get; set; }
        public bool IsInverted { get; set; }
        /// <summary>
        /// Length of a major tick, in pixels at screen resolution
        /// </summary>
        [Initialize(6.0)] public double MajorTickLength { get; set; }
        /// <summary>
        /// Desired number of major ticks per inch.  This is only used as a guideline for the axis 
        /// layout algorithm and the actual result may differ from this value
        /// </summary>
        [Initialize(1.0)] public double MajorTicksPerInch { get; set; }
        /// <summary>
        /// Length of a minor tick, in pixels at screen resolution
        /// </summary>
        [Initialize(3.0)] public double MinorTickLength { get; set; }
        /// <summary>
        /// Desired number of minor ticks per inch.  This is only used as a guideline for the axis 
        /// layout algorithm and the actual result may differ from this value.
        /// This value is only used when AxisType == AxisType.Linear.
        /// When AxisType == AxisType.Logarithmic, minor ticks will always be created at the usual
        /// places (2, 3, 4, 5, 6, 7, 8, and 9), and also at any power of 10 that does not already have
        /// a major tick
        /// </summary>
        [Initialize(4.0)] public double MinorTicksPerInch { get; set; }
        public Func<double, double> PositionToValue { get; set; }
        public Func<double, double> ValueToPosition { get; set; }
        [Initialize] public Range VisibleRange { get; set; }
        public double ActualWidth { get; set; }
        public double ActualHeight { get; set; }

        public Visibility Visibility { get; set; }

        [Initialize(true)] public bool Autorange { get; set; }
        public double MouseDataLocation { get; set; }
    }
}