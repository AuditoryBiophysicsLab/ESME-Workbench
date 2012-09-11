using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using System.Windows.Media;
using DavesWPFTester.AxisLabeling.Layout;
using ESME.Views.Controls;
using HRC;
using HRC.Aspects;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public class FourAxisSeriesViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<FourAxisSeriesViewModel> _propertyObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver, _yAxisObserver;
        [UsedImplicitly] CollectionObserver _dataSeriesCollectionObserver;
        public FourAxisSeriesViewModel()
        {
            XAxis = BottomAxis;
            YAxis = LeftAxis;
            TopAxis.Visibility = Visibility.Collapsed;
            RightAxis.Visibility = Visibility.Collapsed;
            TopAxis.Label = "Top Axis";
            BottomAxis.Label = "Bottom Axis";
            LeftAxis.Label = "Left Axis";
            RightAxis.Label = "Right Axis";
            XAxis.Autorange = true;
            YAxis.Autorange = true;
            XAxisTicks = XAxis.AxisTicks;
            YAxisTicks = YAxis.AxisTicks;

            _propertyObserver = new PropertyObserver<FourAxisSeriesViewModel>(this)
                .RegisterHandler(d => d.DataSeriesCollection, DataSeriesCollectionPropertyChanged);

            if (DataSeriesCollection != null) DataSeriesCollectionPropertyChanged();
            MajorTickLineColor = Colors.Black;
            MinorTickLineColor = Colors.LightGray;
        }

        [Initialize] public DataAxisViewModel TopAxis { get; set; }
        [Initialize] public DataAxisViewModel BottomAxis { get; set; }
        [Initialize] public DataAxisViewModel LeftAxis { get; set; }
        [Initialize] public DataAxisViewModel RightAxis { get; set; }
        [Initialize] public ObservableCollection<ISeries> DataSeriesCollection { get; set; }

        public DataAxisViewModel XAxis { get; set; }
        public DataAxisViewModel YAxis { get; set; }
        public Color MajorTickLineColor { get; set; }
        public Color MinorTickLineColor { get; set; }
        public ObservableCollection<NewDataAxisTick> XAxisTicks { get; set; }
        public ObservableCollection<NewDataAxisTick> YAxisTicks { get; set; }

        void DataSeriesCollectionPropertyChanged()
        {
            if (_dataSeriesCollectionObserver != null) _dataSeriesCollectionObserver.UnregisterHandler(DataSeriesCollectionChanged);
            if (DataSeriesCollection == null) return;
            if (_dataSeriesCollectionObserver == null) _dataSeriesCollectionObserver = new CollectionObserver(DataSeriesCollection);
            _dataSeriesCollectionObserver.RegisterHandler(DataSeriesCollectionChanged);
        }
        void DataSeriesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        if (dataSeries.XAxis == null) dataSeries.XAxis = XAxis;
                        if (dataSeries.YAxis == null) dataSeries.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        if (dataSeries.XAxis == null) dataSeries.XAxis = XAxis;
                        if (dataSeries.YAxis == null) dataSeries.YAxis = YAxis;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException("Clear");
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
        }
    }

    public class DataAxisViewModel : ViewModelBase
    {
        public DataAxisViewModel()
        {
            DataRange = new Range(0.1, 10);
            VisibleRange = DataRange.Expand(0);
            AxisTicks = new ObservableCollection<NewDataAxisTick>();
        }
        [Initialize("Axis")]
        public string Label { get; set; }
        [Initialize(AxisLayoutAlgorithm.ExtendedWilkinson)] 
        public AxisLayoutAlgorithm AxisLayoutAlgorithm { get; set; }
        [Initialize]
        public ObservableCollection<NewDataAxisTick> AxisTicks { get; set; }
        [Initialize(AxisType.Linear)]
        public AxisType AxisType { get; set; }
        public Range DataRange { get; set; }
        public bool IsInverted { get; set; }
        /// <summary>
        /// Length of a major tick, in pixels at screen resolution
        /// </summary>
        [Initialize(6.0)] 
        public double MajorTickLength { get; set; }
        /// <summary>
        /// Desired number of major ticks per inch.  This is only used as a guideline for the axis 
        /// layout algorithm and the actual result may differ from this value
        /// </summary>
        [Initialize(1.0)] 
        public double MajorTicksPerInch { get; set; }
        /// <summary>
        /// Length of a minor tick, in pixels at screen resolution
        /// </summary>
        [Initialize(3.0)]
        public double MinorTickLength { get; set; }
        /// <summary>
        /// Desired number of minor ticks per inch.  This is only used as a guideline for the axis 
        /// layout algorithm and the actual result may differ from this value.
        /// This value is only used when AxisType == AxisType.Linear.
        /// When AxisType == AxisType.Logarithmic, minor ticks will always be created at the usual
        /// places (2, 3, 4, 5, 6, 7, 8, and 9), and also at any power of 10 that does not already have
        /// a major tick
        /// </summary>
        [Initialize(4.0)]
        public double MinorTicksPerInch { get; set; }
        public Func<double, double> PositionToValue { get; set; }
        public Func<double, double> ValueToPosition { get; set; }
        public Range VisibleRange { get; set; }

        public Visibility Visibility { get; set; }

        [Initialize(true)] public bool Autorange { get; set; }
    }
}
