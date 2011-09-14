using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using ESME.Environment.Descriptors;

namespace ESME.Views.EnvironmentBuilder
{
    public class RangeComplexViewModel : RangeComplexTreeItem
    {
        readonly NewRangeComplex _rangeComplex;
        readonly List<RangeComplexTreeItem> _treeItems = new List<RangeComplexTreeItem>();
        public RangeComplexViewModel(NewRangeComplex rangeComplex) 
        {
            _rangeComplex = rangeComplex;
            Children.Add(new RangeComplexTreeItem { Name = "Areas" });
            Children.Add(new RangeComplexTreeItem { Name = "Environment" });
            this["Environment"].Children.Add(new RangeComplexTreeItem { Name = "Salinity" });
            this["Environment"].Children.Add(new RangeComplexTreeItem { Name = "Temperature" });
            _rangeComplex.EnvironmentFileCollection.CollectionChanged += EnvironmentFileCollectionChanged;
            AddEnvironmentFiles(_rangeComplex.EnvironmentFileCollection);
        }

        void EnvironmentFileCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (EnvironmentFile item in e.NewItems) AddEnvironmentFile(item);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (EnvironmentFile oldItem in e.OldItems)
                    {
                        var oldTreeItem = _treeItems.Where(item => item.EnvironmentFile == oldItem).Single();
                        if (oldTreeItem.Parent != null) oldTreeItem.Parent.Children.Remove(oldTreeItem);
                    }
                    break;
                case NotifyCollectionChangedAction.Replace:
                    foreach (EnvironmentFile oldItem in e.OldItems)
                    {
                        var oldTreeItem = _treeItems.Where(item => item.EnvironmentFile == oldItem).Single();
                        if (oldTreeItem.Parent != null)
                        {
                            var parent = oldTreeItem.Parent;
                            var siblings = parent.Children;
                            throw new NotImplementedException();
                        }
                    }
                    break;
                case NotifyCollectionChangedAction.Reset:
                    throw new NotImplementedException();
                    break;
            }
        }

        void AddEnvironmentFiles(IEnumerable<EnvironmentFile> fileCollection) { foreach (var envFile in fileCollection) AddEnvironmentFile(envFile); }
        void AddEnvironmentFile(EnvironmentFile envFile)
        {
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
                    if (!areas.Children.Any(area => area.Name == areaName)) areas.Children.Add(new RangeComplexTreeItem {Name = areaName});
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
                    this["Environment"]["Salinity"].Children.Add(newItem);
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
                    this["Environment"]["Temperature"].Children.Add(newItem);
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
            _treeItems.Add(newItem);
        }
    }
}
