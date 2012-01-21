using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Windows.Data;
using ESME.Environment.Descriptors;

namespace ESME.Views.EnvironmentBuilder
{
    public class RangeComplexTreeViewModel : RangeComplexTreeItem
    {
        readonly RangeComplex _rangeComplex;
        readonly List<RangeComplexTreeItem> _treeItems = new List<RangeComplexTreeItem>();
        bool _once = true;
        public RangeComplexTreeViewModel(RangeComplex rangeComplex) 
        {
            _rangeComplex = rangeComplex;
            ResetTree();
            _rangeComplex.EnvironmentFiles.CollectionChanged += EnvironmentFileCollectionChanged;
            _rangeComplex.AreaCollection.CollectionChanged += (s, e) =>
            {
                switch (e.Action)
                {
                    case NotifyCollectionChangedAction.Add:
                        foreach (KeyValuePair<string, RangeComplexArea> newItem in e.NewItems)
                        {
                            newItem.Value.BathymetryFiles.CollectionChanged += EnvironmentFileCollectionChanged;
                            AddEnvironmentFiles(newItem.Value.BathymetryFiles);
                        }
                        break;
                    case NotifyCollectionChangedAction.Remove:
                        foreach (KeyValuePair<string, RangeComplexArea> oldItem in e.OldItems)
                        {
                            oldItem.Value.BathymetryFiles.CollectionChanged -= EnvironmentFileCollectionChanged;
                            this["Areas"].Children.RemoveAll(area => area.Name == oldItem.Key);
                        }
                        break;
                    case NotifyCollectionChangedAction.Replace:
                        foreach (KeyValuePair<string, RangeComplexArea> oldItem in e.OldItems)
                        {
                            oldItem.Value.BathymetryFiles.CollectionChanged -= EnvironmentFileCollectionChanged;
                            this["Areas"].Children.RemoveAll(area => area.Name == oldItem.Key);
                        }
                        foreach (KeyValuePair<string, RangeComplexArea> newItem in e.NewItems)
                        {
                            newItem.Value.BathymetryFiles.CollectionChanged += EnvironmentFileCollectionChanged;
                            AddEnvironmentFiles(newItem.Value.BathymetryFiles);
                        }
                        break;
                    case NotifyCollectionChangedAction.Reset:
                        ResetTree();
                        break;
                }
            };
        }

        void ResetTree()
        {
            if (!_once) Debugger.Break();
            _once = true;
            Children.Clear();
            AddEnvironmentFiles(_rangeComplex.EnvironmentFiles);
            foreach (var area in _rangeComplex.AreaCollection) AddEnvironmentFiles(area.Value.BathymetryFiles);
        }

        void EnvironmentFileCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (KeyValuePair<string, EnvironmentFile> newItem in e.NewItems) AddEnvironmentFile(newItem.Value);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (KeyValuePair<string, EnvironmentFile> oldItem in e.OldItems)
                    {
                        var oldTreeItem = _treeItems.Where(item => item.EnvironmentFile == oldItem.Value).Single();
                        if (oldTreeItem.Parent != null) oldTreeItem.Parent.Children.Remove(oldTreeItem);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (KeyValuePair<string, EnvironmentFile> oldItem in e.OldItems)
                    {
                        var oldTreeItem = _treeItems.Where(item => item.EnvironmentFile == oldItem.Value).Single();
                        if (oldTreeItem.Parent != null) oldTreeItem.Parent.Children.Remove(oldTreeItem);
                    }
                    foreach (KeyValuePair<string, EnvironmentFile> newItem in e.NewItems) AddEnvironmentFile(newItem.Value);
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException();
            }
        }

        void AddEnvironmentFiles(IEnumerable<KeyValuePair<string, EnvironmentFile>> envFiles)
        {
            foreach (var envFile in envFiles) AddEnvironmentFile(envFile.Value);
        }

        void AddEnvironmentFile(EnvironmentFile envFile)
        {
            if (Children.Where(item => item.Name == "Areas").Count() == 0) Children.Add(new RangeComplexTreeItem { Name = "Areas" });
            if (Children.Where(item => item.Name == "Environment").Count() == 0)
            {
                Children.Add(new RangeComplexTreeItem { Name = "Environment" });
                this["Environment"].Children.Add(new RangeComplexTreeItem { Name = "Salinity" });
                this["Environment"].Children.Add(new RangeComplexTreeItem { Name = "Temperature" });
            }

            RangeComplexTreeItem newItem;
            switch (envFile.DataType)
            {
                case EnvironmentDataType.Bathymetry:
                    newItem = new EnvironmentFileTreeItem
                    {
                        Name = Path.GetFileNameWithoutExtension(envFile.FileName),
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    var areaName = Path.GetDirectoryName(envFile.FileName);
                    var areas = this["Areas"];
                    if (!areas.Children.Any(area => area.Name == areaName))
                    {
                        areas.Children.Add(new RangeComplexTreeItem { Name = areaName });
                        areas.ToolTip = "{0} areas defined";
                    }
                    areas[areaName].ToolTip = "{0} resolutions available";
                    areas[areaName].Children.Add(newItem);
                    break;
                case EnvironmentDataType.BottomLoss:
                    newItem = new EnvironmentFileTreeItem
                    {
                        Name = "Bottom Loss",
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    this["Environment"].Children.Add(newItem);
                    break;
                case EnvironmentDataType.Salinity:
                    newItem = new TimePeriodEnvironmentFileTreeItem
                    {
                        Name = envFile.TimePeriod.ToString(),
                        TimePeriod = envFile.TimePeriod,
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    var salinityRoot = this["Environment"]["Salinity"];
                    salinityRoot.Children.Add(newItem);
                    salinityRoot.ToolTip = "{0} time periods available";
                    break;
                case EnvironmentDataType.Sediment:
                    newItem = new EnvironmentFileTreeItem
                    {
                        Name = "Sediment",
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    this["Environment"].Children.Add(newItem);
                    break;
                case EnvironmentDataType.Temperature:
                    newItem = new TimePeriodEnvironmentFileTreeItem
                    {
                        Name = envFile.TimePeriod.ToString(),
                        TimePeriod = envFile.TimePeriod,
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    var temperatureRoot = this["Environment"]["Temperature"];
                    temperatureRoot.Children.Add(newItem);
                    temperatureRoot.ToolTip = "{0} time periods available";
                    break;
                case EnvironmentDataType.Wind:
                    newItem = new EnvironmentFileTreeItem
                    {
                        Name = "Wind",
                        SampleCount = envFile.SampleCount,
                        GeoRect = envFile.GeoRect,
                        FileSize = envFile.FileSize,
                    };
                    this["Environment"].Children.Add(newItem);
                    break;
                default:
                    throw new ApplicationException(string.Format("Unknown environment data type: {0}", envFile.DataType));
            }
            newItem.EnvironmentFile = envFile;
            newItem.IsAvailable = true;
            newItem.IsInitializing = false;
            newItem.IsEnabled = true;
            if (newItem.ToolTip == null)
                newItem.ToolTip = string.Format("Sample points: {0:#,#}\r\nCenter: ({1:0.####}, {2:0.####})\r\nAverage width: {3:0.##} km\r\nHeight: {4:0.##} km\r\nFile size: {5:#,#} bytes",
                                                envFile.SampleCount, envFile.GeoRect.Center.Latitude, envFile.GeoRect.Center.Longitude, envFile.GeoRect.AverageWidthKm, envFile.GeoRect.HeightKm,
                                                envFile.FileSize);

            _treeItems.Add(newItem);
        }
    }

    [ValueConversion(typeof (System.Collections.IList), typeof (System.Collections.IEnumerable))]
    public class CollectionViewSortConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var collection = value as System.Collections.IList;
            if (collection == null) return null;
            var view = new ListCollectionView(collection);
            var sort = new SortDescription(parameter.ToString(), ListSortDirection.Ascending);
            view.SortDescriptions.Add(sort);
            return view;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) { throw new NotImplementedException(); }
    }
}
