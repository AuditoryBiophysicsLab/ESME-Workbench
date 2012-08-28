using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Windows.Media;
using ESME.Views.Controls;
using HRC;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;

namespace DavesWPFTester
{
    public class TwoAxisSeriesViewModel : ViewModelBase
    {
        [UsedImplicitly] PropertyObserver<TwoAxisSeriesViewModel> _propertyObserver;
        [UsedImplicitly] PropertyObserver<DataAxisViewModel> _xAxisObserver, _yAxisObserver;
        [UsedImplicitly] CollectionObserver _dataSeriesCollectionObserver;
        public TwoAxisSeriesViewModel()
        {
            XMin = XMax = YMin = YMax = double.NaN;
            XAxis.Label = "X Axis";
            YAxis.Label = "Y Axis";
            XAxis.Autorange = true;
            YAxis.Autorange = true;

            _propertyObserver = new PropertyObserver<TwoAxisSeriesViewModel>(this)
                .RegisterHandler(d => d.DataSeriesCollection, DataSeriesCollectionPropertyChanged)
                .RegisterHandler(d => XMin, MinMaxPropertiesChanged)
                .RegisterHandler(d => XMax, MinMaxPropertiesChanged)
                .RegisterHandler(d => YMin, MinMaxPropertiesChanged)
                .RegisterHandler(d => YMax, MinMaxPropertiesChanged)
                .RegisterHandler(d => XAxis, XAxisPropertyChanged)
                .RegisterHandler(d => YAxis, YAxisPropertyChanged);

            if (DataSeriesCollection != null) DataSeriesCollectionPropertyChanged();
            XAxisPropertyChanged();
            YAxisPropertyChanged();
        }
        [Initialize] public DataAxisViewModel XAxis { get; set; }
        [Initialize] public DataAxisViewModel YAxis { get; set; }
        public bool ShowXAxisMajorTicks { get { return XAxis.ShowMajorTicks; } set { XAxis.ShowMajorTicks = value; } }
        public bool ShowXAxisMinorTicks { get { return XAxis.ShowMinorTicks; } set { XAxis.ShowMinorTicks = value; } }
        public bool ShowYAxisMajorTicks { get { return YAxis.ShowMajorTicks; } set { YAxis.ShowMajorTicks = value; } }
        public bool ShowYAxisMinorTicks { get { return YAxis.ShowMinorTicks; } set { YAxis.ShowMinorTicks = value; } }
        [Initialize] public ObservableCollection<ISeries> DataSeriesCollection { get; set; }
        public Color MajorTickLineColor { get; set; }
        public Color MinorTickLineColor { get; set; }
        public double XMin { get; set; }
        public double XMax { get; set; }
        public double YMin { get; set; }
        public double YMax { get; set; }

        void XAxisPropertyChanged() { AxisPropertyChanged(XAxis, ref _xAxisObserver, XAxisChanged); }
        void YAxisPropertyChanged() { AxisPropertyChanged(YAxis, ref _yAxisObserver, YAxisChanged); }

        void AxisPropertyChanged(DataAxisViewModel dataAxis, ref PropertyObserver<DataAxisViewModel> observer, Action action)
        {
            if (observer != null) observer.UnregisterHandler(d => d.MappingFunction).UnregisterHandler(d => d.StartValue).UnregisterHandler(d => d.EndValue);
            if (dataAxis == null) return;
            if (observer == null) observer = new PropertyObserver<DataAxisViewModel>(XAxis);
            observer.RegisterHandler(d => d.MappingFunction, action)
                .RegisterHandler(d => d.StartValue, RenderAllSeries)
                .RegisterHandler(d => d.EndValue, RenderAllSeries);
        }
        void XAxisChanged() { foreach (var item in DataSeriesCollection) item.XAxisMappingFunction = XAxis.MappingFunction; }
        void YAxisChanged() { foreach (var item in DataSeriesCollection) item.YAxisMappingFunction = YAxis.MappingFunction; }
        void RenderAllSeries() { foreach (var item in DataSeriesCollection) item.RenderShapes(); }

        void MinMaxPropertiesChanged()
        {
            if (XAxis != null && XAxis.Autorange) XAxis.UpdateRange(XMin, XMax);
            if (YAxis != null && YAxis.Autorange) YAxis.UpdateRange(YMin, YMax);
        }
        void DataSeriesCollectionPropertyChanged()
        {
            if (_dataSeriesCollectionObserver != null) _dataSeriesCollectionObserver.UnregisterHandler(DataSeriesCollectionChanged);
            if (DataSeriesCollection == null) return;
            if (_dataSeriesCollectionObserver == null) _dataSeriesCollectionObserver = new CollectionObserver(DataSeriesCollection);
            _dataSeriesCollectionObserver.RegisterHandler(DataSeriesCollectionChanged);
            UpdateMinMaxForAllSeries();
        }
        readonly Dictionary<DataSeriesViewModel, PropertyObserver<DataSeriesViewModel>> _seriesObservers = new Dictionary<DataSeriesViewModel, PropertyObserver<DataSeriesViewModel>>();
        void DataSeriesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (DataSeriesViewModel dataSeries in args.NewItems)
                    {
                        var observer = new PropertyObserver<DataSeriesViewModel>(dataSeries)
                            .RegisterHandler(d => d.XMin, UpdateMinMax)
                            .RegisterHandler(d => d.XMax, UpdateMinMax)
                            .RegisterHandler(d => d.YMin, UpdateMinMax)
                            .RegisterHandler(d => d.YMax, UpdateMinMax);
                        _seriesObservers.Add(dataSeries, observer);
                        UpdateMinMax(dataSeries);
                        dataSeries.XAxisMappingFunction = XAxis.MappingFunction;
                        dataSeries.YAxisMappingFunction = YAxis.MappingFunction;
                        Debug.WriteLine(string.Format("Adding DataSeries: {0}", dataSeries));
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (DataSeriesViewModel dataSeries in args.OldItems)
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
                    foreach (DataSeriesViewModel dataSeries in args.OldItems)
                        if (_seriesObservers.ContainsKey(dataSeries))
                        {
                            _seriesObservers[dataSeries]
                                .UnregisterHandler(d => d.XMin)
                                .UnregisterHandler(d => d.XMax)
                                .UnregisterHandler(d => d.YMin)
                                .UnregisterHandler(d => d.YMax);
                            _seriesObservers.Remove(dataSeries);
                        }
                    foreach (DataSeriesViewModel dataSeries in args.NewItems)
                    {
                        var observer = new PropertyObserver<DataSeriesViewModel>(dataSeries)
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
            _propertyObserver = new PropertyObserver<DataAxisViewModel>(this)
                .RegisterHandler(d => d.IsInverted, IsInvertedChanged);
        }
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
        [Initialize("0.##")]public string TickValueFormat { get; set; }
        public bool Autorange { get; set; }
        public bool IsInverted { get; set; }
        public Func<double, double> MappingFunction { get; set; }
        void IsInvertedChanged()
        {
            var startValue = StartValue;
            var endValue = EndValue;
            StartValue = IsInverted ? endValue : startValue;
            EndValue = IsInverted ? startValue : endValue;
        }
        public void UpdateRange(double min, double max)
        {
            if (double.IsNaN(min) || double.IsNaN(max)) return;
            StartValue = IsInverted ? max : min;
            EndValue = IsInverted ? min : max;
        }
    }
}
