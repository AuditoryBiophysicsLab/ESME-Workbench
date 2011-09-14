using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Views.EnvironmentBuilder
{
    public class RangeComplexTreeItem : ViewModelBase
    {
        public RangeComplexTreeItem() 
        {
            Children = new ObservableList<RangeComplexTreeItem>();
            Children.CollectionChanged += (s, e) =>
            {
                if (e.Action == NotifyCollectionChangedAction.Add)
                    foreach (RangeComplexTreeItem item in e.NewItems) item.Parent = this;
            };
        }

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.Name);
        string _name;

        #endregion

        #region public string ToolTip { get; set; }

        public string ToolTip
        {
            get { return _toolTip; }
            set
            {
                if (_toolTip == value) return;
                _toolTip = value;
                NotifyPropertyChanged(ToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.ToolTip);
        string _toolTip;

        #endregion

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        #region public bool IsAvailable { get; set; }

        public bool IsAvailable
        {
            get { return _isAvailable; }
            set
            {
                if (_isAvailable == value) return;
                _isAvailable = value;
                NotifyPropertyChanged(IsAvailableChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsAvailableChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.IsAvailable);
        bool _isAvailable;

        #endregion

        #region public bool IsInitializing { get; set; }

        public bool IsInitializing
        {
            get { return _isInitializing; }
            set
            {
                if (_isInitializing == value) return;
                _isInitializing = value;
                NotifyPropertyChanged(IsInitializingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsInitializingChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.IsInitializing);
        bool _isInitializing;

        #endregion

        #region public ObservableList<RangeComplexTreeItem> Children { get; set; }

        public ObservableList<RangeComplexTreeItem> Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexTreeItem>(x => x.Children);
        ObservableList<RangeComplexTreeItem> _children;

        #endregion

        public EnvironmentFile EnvironmentFile { get; set; }

        public RangeComplexTreeItem Parent { get; set; }

        public RangeComplexTreeItem this[string childName] { get { return Children.Where(child => child.Name == childName).Single(); } }
    }

    public class EnvironmentFileTreeItem : RangeComplexTreeItem
    {
        #region public uint SampleCount { get; set; }

        public uint SampleCount
        {
            get { return _sampleCount; }
            set
            {
                if (_sampleCount == value) return;
                _sampleCount = value;
                NotifyPropertyChanged(SampleCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SampleCountChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFileTreeItem>(x => x.SampleCount);
        uint _sampleCount;

        #endregion

        #region public GeoRect GeoRect { get; set; }

        public GeoRect GeoRect
        {
            get { return _geoRect; }
            set
            {
                if (_geoRect == value) return;
                _geoRect = value;
                NotifyPropertyChanged(GeoRectChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GeoRectChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFileTreeItem>(x => x.GeoRect);
        GeoRect _geoRect;

        #endregion

        #region public long FileSize { get; set; }

        public long FileSize
        {
            get { return _fileSize; }
            set
            {
                if (_fileSize == value) return;
                _fileSize = value;
                NotifyPropertyChanged(FileSizeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FileSizeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentFileTreeItem>(x => x.FileSize);
        long _fileSize;

        #endregion
    }

    public class TimePeriodEnvironmentFileTreeItem : EnvironmentFileTreeItem
    {
        #region public NAVOTimePeriod TimePeriod { get; set; }

        public NAVOTimePeriod TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodEnvironmentFileTreeItem>(x => x.TimePeriod);
        NAVOTimePeriod _timePeriod;

        #endregion
    }
}