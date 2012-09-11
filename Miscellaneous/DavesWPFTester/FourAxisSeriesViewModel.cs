using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Windows;
using System.Windows.Media;
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
            XRange.RangeChanged += XRangeChanged;
            YRange.RangeChanged += YRangeChanged;
            XAxisTicks = XAxis.AxisTicks;
            YAxisTicks = YAxis.AxisTicks;

            _propertyObserver = new PropertyObserver<FourAxisSeriesViewModel>(this)
                .RegisterHandler(d => d.DataSeriesCollection, DataSeriesCollectionPropertyChanged)
                .RegisterHandler(d => XAxis, XAxisPropertyChanged)
                .RegisterHandler(d => YAxis, YAxisPropertyChanged);

            if (DataSeriesCollection != null) DataSeriesCollectionPropertyChanged();
            XAxisPropertyChanged();
            YAxisPropertyChanged();
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
        [Initialize, UsedImplicitly] public Range XRange { get; set; }
        [Initialize, UsedImplicitly] public Range YRange { get; set; }
        public ObservableCollection<NewDataAxisTick> XAxisTicks { get; set; }
        public ObservableCollection<NewDataAxisTick> YAxisTicks { get; set; }

        void XAxisPropertyChanged()
        {
            AxisMappingFunctionChanged(XAxis, ref _xAxisObserver, XAxisChanged);
            AxisMappingFunctionChanged(XAxis, ref _xAxisObserver, RenderAllSeries);
        }
        void YAxisPropertyChanged()
        {
            AxisMappingFunctionChanged(YAxis, ref _yAxisObserver, YAxisChanged);
            AxisMappingFunctionChanged(XAxis, ref _xAxisObserver, RenderAllSeries);
        }

        static void AxisMappingFunctionChanged(DataAxisViewModel dataAxis, ref PropertyObserver<DataAxisViewModel> observer, Action action)
        {
            if (observer != null) observer.UnregisterHandler(d => d.MappingFunction);
            if (dataAxis == null) return;
            if (observer == null) observer = new PropertyObserver<DataAxisViewModel>(dataAxis);
            observer.RegisterHandler(d => d.MappingFunction, action);
        }
        void XAxisChanged() { foreach (var item in DataSeriesCollection) item.XAxisMappingFunction = XAxis.MappingFunction; }
        void YAxisChanged() { foreach (var item in DataSeriesCollection) item.YAxisMappingFunction = YAxis.MappingFunction; }
        void RenderAllSeries()
        {
            //Debug.WriteLine("Re-rendering all series");
            foreach (var item in DataSeriesCollection) item.RenderShapes();
        }
        void XRangeChanged(object sender, NotifyRangeChangedEventArgs args = null)
        {
            if (XAxis != null && XAxis.Autorange)
            {
                //Debug.WriteLine(string.Format("Updating X Axis min/max. Old range: {0} ... {1} New range: {2} ... {3}", XAxis.StartValue, XAxis.EndValue, XMin, XMax));
                XAxis.DataRange.Add(XRange);
            }
        }

        void YRangeChanged(object sender, NotifyRangeChangedEventArgs args = null)
        {
            if (YAxis != null && YAxis.Autorange)
            {
                //Debug.WriteLine(string.Format("Updating X Axis min/max. Old range: {0} ... {1} New range: {2} ... {3}", XAxis.StartValue, XAxis.EndValue, XMin, XMax));
                YAxis.DataRange.Add(YRange);
            }
        }

        void DataSeriesCollectionPropertyChanged()
        {
            if (_dataSeriesCollectionObserver != null) _dataSeriesCollectionObserver.UnregisterHandler(DataSeriesCollectionChanged);
            if (DataSeriesCollection == null) return;
            if (_dataSeriesCollectionObserver == null) _dataSeriesCollectionObserver = new CollectionObserver(DataSeriesCollection);
            _dataSeriesCollectionObserver.RegisterHandler(DataSeriesCollectionChanged);
            foreach (var series in DataSeriesCollection)
            {
                ExpandXRange(series.XRange);
                ExpandYRange(series.YRange);
            }
        }
        void DataSeriesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        dataSeries.XAxis = XAxis;
                        dataSeries.YAxis = YAxis;
                        dataSeries.XRange.RangeChanged += ExpandXRange;
                        dataSeries.YRange.RangeChanged += ExpandYRange;
                        ExpandXRange(dataSeries.XRange);
                        ExpandYRange(dataSeries.YRange);
                        if (dataSeries.XAxisMappingFunction == null) dataSeries.XAxisMappingFunction = XAxis.MappingFunction;
                        if (dataSeries.YAxisMappingFunction == null) dataSeries.YAxisMappingFunction = YAxis.MappingFunction;
                        Debug.WriteLine(string.Format("Adding DataSeries: {0}", dataSeries));
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (SeriesViewModelBase dataSeries in args.OldItems)
                    {
                        dataSeries.XRange.RangeChanged -= ExpandXRange;
                        dataSeries.YRange.RangeChanged -= ExpandYRange;
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (SeriesViewModelBase dataSeries in args.OldItems)
                    {
                        dataSeries.XRange.RangeChanged -= ExpandXRange;
                        dataSeries.YRange.RangeChanged -= ExpandYRange;
                    }
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        dataSeries.XRange.RangeChanged += ExpandXRange;
                        dataSeries.YRange.RangeChanged += ExpandYRange;
                        ExpandXRange(dataSeries.XRange);
                        ExpandYRange(dataSeries.YRange);
                        if (dataSeries.XAxisMappingFunction == null) dataSeries.XAxisMappingFunction = XAxis.MappingFunction;
                        if (dataSeries.YAxisMappingFunction == null) dataSeries.YAxisMappingFunction = YAxis.MappingFunction;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException("Clear");
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
        }

        void ExpandXRange(object sender, NotifyRangeChangedEventArgs args = null) { XRange.Add((Range)sender); }
        void ExpandYRange(object sender, NotifyRangeChangedEventArgs args = null) { YRange.Add((Range)sender); }
    }

    public class DataAxisViewModel : ViewModelBase
    {
        public DataAxisViewModel()
        {
            DataRange = new Range(0.1, 10);
            VisibleRange = DataRange.Expand(0);
            AxisTicks = new ObservableCollection<NewDataAxisTick>();
        }
        public string Label { get; set; }
        public AxisType AxisType { get; set; }

        public Visibility Visibility { get; set; }
        public Range DataRange { get; set; }
        public Range VisibleRange { get; set; }
        public ObservableCollection<NewDataAxisTick> AxisTicks { get; set; }

        [Initialize(true)] public bool Autorange { get; set; }
        public bool IsInverted { get; set; }
        public Func<double, double> MappingFunction { get; set; }
    }
}
