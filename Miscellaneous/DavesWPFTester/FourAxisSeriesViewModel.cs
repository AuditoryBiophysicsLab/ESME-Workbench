using System;
using System.Collections.Generic;
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
            TopAxis.Label = "Top Axis";
            BottomAxis.Label = "Bottom Axis";
            LeftAxis.Label = "Left Axis";
            RightAxis.Label = "Right Axis";
            XMin = XMax = YMin = YMax = double.NaN;
            XAxis.Autorange = true;
            YAxis.Autorange = true;

            _propertyObserver = new PropertyObserver<FourAxisSeriesViewModel>(this)
                .RegisterHandler(d => d.DataSeriesCollection, DataSeriesCollectionPropertyChanged)
                .RegisterHandler(d => XMin, XMinMaxPropertiesChanged)
                .RegisterHandler(d => XMax, XMinMaxPropertiesChanged)
                .RegisterHandler(d => YMin, YMinMaxPropertiesChanged)
                .RegisterHandler(d => YMax, YMinMaxPropertiesChanged)
                .RegisterHandler(d => XAxis, XAxisPropertyChanged)
                .RegisterHandler(d => YAxis, YAxisPropertyChanged);

            if (DataSeriesCollection != null) DataSeriesCollectionPropertyChanged();
            XAxisPropertyChanged();
            YAxisPropertyChanged();
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
        public double XMin { get; set; }
        public double XMax { get; set; }
        public double YMin { get; set; }
        public double YMax { get; set; }

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

        void XMinMaxPropertiesChanged()
        {
            if (XAxis != null && XAxis.Autorange)
            {
                //Debug.WriteLine(string.Format("Updating X Axis min/max. Old range: {0} ... {1} New range: {2} ... {3}", XAxis.StartValue, XAxis.EndValue, XMin, XMax));
                XAxis.Range.Update(XMin, XMax);
            }
        }

        void YMinMaxPropertiesChanged()
        {
            if (YAxis != null && YAxis.Autorange)
            {
                //Debug.WriteLine(string.Format("Updating Y Axis min/max. Old range: {0} ... {1} New range: {2} ... {3}", YAxis.StartValue, YAxis.EndValue, YMin, YMax));
                YAxis.Range.Update(YMin, YMax);
            }
        }

        void DataSeriesCollectionPropertyChanged()
        {
            if (_dataSeriesCollectionObserver != null) _dataSeriesCollectionObserver.UnregisterHandler(DataSeriesCollectionChanged);
            if (DataSeriesCollection == null) return;
            if (_dataSeriesCollectionObserver == null) _dataSeriesCollectionObserver = new CollectionObserver(DataSeriesCollection);
            _dataSeriesCollectionObserver.RegisterHandler(DataSeriesCollectionChanged);
            UpdateMinMaxForAllSeries();
        }
        readonly Dictionary<SeriesViewModelBase, PropertyObserver<SeriesViewModelBase>> _seriesObservers = new Dictionary<SeriesViewModelBase, PropertyObserver<SeriesViewModelBase>>();
        void DataSeriesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        var observer = new PropertyObserver<SeriesViewModelBase>(dataSeries)
                            .RegisterHandler(d => d.XMin, UpdateMinMax)
                            .RegisterHandler(d => d.XMax, UpdateMinMax)
                            .RegisterHandler(d => d.YMin, UpdateMinMax)
                            .RegisterHandler(d => d.YMax, UpdateMinMax);
                        _seriesObservers.Add(dataSeries, observer);
                        UpdateMinMax(dataSeries);
                        if (dataSeries.XAxisMappingFunction == null) dataSeries.XAxisMappingFunction = XAxis.MappingFunction;
                        if (dataSeries.YAxisMappingFunction == null) dataSeries.YAxisMappingFunction = YAxis.MappingFunction;
                        Debug.WriteLine(string.Format("Adding DataSeries: {0}", dataSeries));
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (SeriesViewModelBase dataSeries in args.OldItems)
                        if (_seriesObservers.ContainsKey(dataSeries))
                        {
                            _seriesObservers[dataSeries]
                                .UnregisterHandler(d => d.XMin)
                                .UnregisterHandler(d => d.XMax)
                                .UnregisterHandler(d => d.YMin)
                                .UnregisterHandler(d => d.YMax);
                            _seriesObservers.Remove(dataSeries);
                        }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (SeriesViewModelBase dataSeries in args.OldItems)
                        if (_seriesObservers.ContainsKey(dataSeries))
                        {
                            _seriesObservers[dataSeries]
                                .UnregisterHandler(d => d.XMin)
                                .UnregisterHandler(d => d.XMax)
                                .UnregisterHandler(d => d.YMin)
                                .UnregisterHandler(d => d.YMax);
                            _seriesObservers.Remove(dataSeries);
                        }
                    foreach (SeriesViewModelBase dataSeries in args.NewItems)
                    {
                        var observer = new PropertyObserver<SeriesViewModelBase>(dataSeries)
                            .RegisterHandler(d => d.XMin, UpdateMinMax)
                            .RegisterHandler(d => d.XMax, UpdateMinMax)
                            .RegisterHandler(d => d.YMin, UpdateMinMax)
                            .RegisterHandler(d => d.YMax, UpdateMinMax);
                        _seriesObservers.Add(dataSeries, observer);
                        UpdateMinMax(dataSeries);
                        dataSeries.XAxisMappingFunction = XAxis.MappingFunction;
                        dataSeries.YAxisMappingFunction = YAxis.MappingFunction;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    _seriesObservers.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
        }

        void UpdateMinMax(ISeries dataSeries)
        {
            if (double.IsNaN(XMin)) XMin = dataSeries.XMin;
            if (double.IsNaN(XMax)) XMax = dataSeries.XMax;
            if (double.IsNaN(YMin)) YMin = dataSeries.YMin;
            if (double.IsNaN(YMax)) YMax = dataSeries.YMax;
            XMin = Math.Min(XMin, dataSeries.XMin);
            XMax = Math.Max(XMax, dataSeries.XMax);
            YMin = Math.Min(YMin, dataSeries.YMin);
            YMax = Math.Max(YMax, dataSeries.YMax);
        }

        void UpdateMinMaxForAllSeries()
        {
            foreach (var dataSeries in DataSeriesCollection) UpdateMinMax(dataSeries);
        }
    }

    public class DataAxisViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _propertyObserver;

        public DataAxisViewModel()
        {
            Range = new Range(0.1, 10);
            Range.RangeChanged += (s, e) =>
            {
                OnPropertyChanged("StartValue");
                OnPropertyChanged("EndValue");
            };
        }
        public string Label { get; set; }
        public AxisType AxisType { get; set; }

        public double StartValue
        {
            get { return IsInverted ? Range.Max : Range.Min; }
            set
            {
                if (IsInverted) Range.Max = value;
                else Range.Min = value;
            }
        }

        public double EndValue
        {
            get { return IsInverted ? Range.Min : Range.Max; }
            set
            {
                if (IsInverted) Range.Min = value;
                else Range.Max = value;
            }
        }

        public Visibility Visibility { get; set; }
        public Range Range { get; private set; }
        public bool ShowMajorTicks { get; set; }
        public bool ShowMinorTicks { get; set; }

        [Initialize("0.##")] public string TickValueFormat { get; set; }
        public bool Autorange { get; set; }
        public bool IsInverted { get; set; }
        public Func<double, double> MappingFunction { get; set; }
    }
}
