using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using System.Windows.Media;
using HRC.Aspects;
using HRC.ViewModels;

namespace HRC.Plotting
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
                .RegisterHandler(d => d.DataSeriesCollection, DataSeriesCollectionPropertyChanged)
                .RegisterHandler(d => d.MouseLocation, () =>
                {
                    TopAxis.MouseDataLocation = IsMouseOver && TopAxis.Visibility == Visibility.Visible && TopAxis.PositionToValue != null ? TopAxis.PositionToValue(MouseLocation.X) : double.NaN;
                    BottomAxis.MouseDataLocation = IsMouseOver && BottomAxis.Visibility == Visibility.Visible && BottomAxis.PositionToValue != null ? BottomAxis.PositionToValue(MouseLocation.X) : double.NaN;
                    LeftAxis.MouseDataLocation = IsMouseOver && LeftAxis.Visibility == Visibility.Visible && LeftAxis.PositionToValue != null ? LeftAxis.PositionToValue(MouseLocation.Y) : double.NaN;
                    RightAxis.MouseDataLocation = IsMouseOver && RightAxis.Visibility == Visibility.Visible && RightAxis.PositionToValue != null ? RightAxis.PositionToValue(MouseLocation.Y) : double.NaN;
                    //Debug.WriteLine("Mouse data locations: Bottom: {0}, Left: {1}, Top: {2}, Right: {3}", BottomAxis.MouseDataLocation, LeftAxis.MouseDataLocation, TopAxis.MouseDataLocation, RightAxis.MouseDataLocation);
                });

            if (DataSeriesCollection != null) DataSeriesCollectionPropertyChanged();
            MajorTickLineColor = Colors.Black;
            MinorTickLineColor = Colors.LightGray;
        }

        [Initialize] public DataAxisViewModel TopAxis { get; set; }
        [Initialize] public DataAxisViewModel BottomAxis { get; set; }
        [Initialize] public DataAxisViewModel LeftAxis { get; set; }
        [Initialize] public DataAxisViewModel RightAxis { get; set; }
        [Initialize, UsedImplicitly] public ObservableCollection<ISeries> DataSeriesCollection { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<LegendItemViewModel> LegendItems { get; private set; }

        public DataAxisViewModel XAxis { get; set; }
        public DataAxisViewModel YAxis { get; set; }
        public Color MajorTickLineColor { get; set; }
        public Color MinorTickLineColor { get; set; }
        public ObservableCollection<NewDataAxisTick> XAxisTicks { get; set; }
        public ObservableCollection<NewDataAxisTick> YAxisTicks { get; set; }
        public double ActualWidth { get; set; }
        public double ActualHeight { get; set; }
        public bool IsMouseOver { get; set; }
        public Point MouseLocation { get; set; }

        void DataSeriesCollectionPropertyChanged()
        {
            if (_dataSeriesCollectionObserver != null) _dataSeriesCollectionObserver.UnregisterHandler(DataSeriesCollectionChanged);
            if (DataSeriesCollection == null) return;
            if (_dataSeriesCollectionObserver == null) _dataSeriesCollectionObserver = new CollectionObserver(DataSeriesCollection);
            _dataSeriesCollectionObserver.RegisterHandler(DataSeriesCollectionChanged);
        }
        readonly Dictionary<SeriesViewModelBase,CollectionObserver> _seriesLegendItemCollectionObservers = new Dictionary<SeriesViewModelBase, CollectionObserver>();
        void DataSeriesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (SeriesViewModelBase newItem in args.NewItems) 
                    {
                        if (newItem.XAxis == null) newItem.XAxis = XAxis;
                        if (newItem.YAxis == null) newItem.YAxis = YAxis;
                        foreach (var legendItem in newItem.LegendItems) LegendItems.Add(legendItem);
                        _seriesLegendItemCollectionObservers.Add(newItem, new CollectionObserver(newItem.LegendItems).RegisterHandler(DataSeriesLegendItemsCollectionChanged));
                    }
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (SeriesViewModelBase oldItem in args.OldItems)
                    {
                        oldItem.XAxis = null;
                        oldItem.YAxis = null;
                        foreach (var legendItem in oldItem.LegendItems) LegendItems.Remove(legendItem);
                        _seriesLegendItemCollectionObservers.Remove(oldItem);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (SeriesViewModelBase oldItem in args.OldItems)
                    {
                        oldItem.XAxis = null;
                        oldItem.YAxis = null;
                        foreach (var legendItem in oldItem.LegendItems) LegendItems.Remove(legendItem);
                        _seriesLegendItemCollectionObservers.Remove(oldItem);
                    }
                    foreach (SeriesViewModelBase newItem in args.NewItems) 
                    {
                        if (newItem.XAxis == null) newItem.XAxis = XAxis;
                        if (newItem.YAxis == null) newItem.YAxis = YAxis;
                        foreach (var legendItem in newItem.LegendItems) LegendItems.Add(legendItem);
                        _seriesLegendItemCollectionObservers.Add(newItem, new CollectionObserver(newItem.LegendItems).RegisterHandler(DataSeriesLegendItemsCollectionChanged));
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    LegendItems.Clear();
                    break;
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
        }

        void DataSeriesLegendItemsCollectionChanged(INotifyCollectionChanged sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (LegendItemViewModel legendItem in args.NewItems) LegendItems.Add(legendItem);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (LegendItemViewModel legendItem in args.OldItems) LegendItems.Remove(legendItem);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    for (var i = 0; i < args.OldItems.Count; i++)
                    {
                        var newItem = (LegendItemViewModel)args.NewItems[i];
                        var oldItemIndex = args.OldStartingIndex + i;
                        LegendItems[oldItemIndex] = newItem;
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException("Reset");
                case NotifyCollectionChangedAction.Move:
                    throw new NotImplementedException("Move");
            }
        }
    }
}
