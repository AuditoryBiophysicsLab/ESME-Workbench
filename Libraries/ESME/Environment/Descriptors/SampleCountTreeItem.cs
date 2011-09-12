using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    public class EnvironmentDataTree : EnvironmentTreeItem {}

    // Used by all environmental data types
    public class SampleCountTreeItem : EnvironmentTreeItem
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

        static readonly PropertyChangedEventArgs SampleCountChangedEventArgs = ObservableHelper.CreateArgs<SampleCountTreeItem>(x => x.SampleCount);
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

        static readonly PropertyChangedEventArgs GeoRectChangedEventArgs = ObservableHelper.CreateArgs<SampleCountTreeItem>(x => x.GeoRect);
        GeoRect _geoRect;

        #endregion

        #region public bool IsDataAvailable { get; set; }

        public bool IsDataAvailable
        {
            get { return _isDataAvailable; }
            set
            {
                if (_isDataAvailable == value) return;
                _isDataAvailable = value;
                NotifyPropertyChanged(IsDataAvailableChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDataAvailableChangedEventArgs = ObservableHelper.CreateArgs<SampleCountTreeItem>(x => x.IsDataAvailable);
        bool _isDataAvailable;

        #endregion

    }

    // Used by temperature, salinity, soundspeed and wind
    public class TimePeriodEnvironmentTreeItem : SampleCountTreeItem
    {
        #region public float MinimumValue { get; set; }

        public float MinimumValue
        {
            get { return _minimumValue; }
            set
            {
                _minimumValue = value;
                NotifyPropertyChanged(MinimumValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MinimumValueChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodEnvironmentTreeItem>(x => x.MinimumValue);
        float _minimumValue;

        #endregion

        #region public float MaximumValue { get; set; }

        public float MaximumValue
        {
            get { return _maximumValue; }
            set
            {
                if (_maximumValue == value) return;
                _maximumValue = value;
                NotifyPropertyChanged(MaximumValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaximumValueChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodEnvironmentTreeItem>(x => x.MaximumValue);
        float _maximumValue;

        #endregion
    }

    // Used by temperature, salinity and soundspeed
    public class SoundSpeedTreeItem : TimePeriodEnvironmentTreeItem
    {
    }

    // Used by sediment
    public class SedimentTreeItem : EnvironmentTreeItem
    {
        #region public List<EnvironmentTreeItem> SedimentTypes { get; set; }

        public List<SampleCountTreeItem> SedimentTypes
        {
            get { return _sedimentTypes; }
            set
            {
                if (_sedimentTypes == value) return;
                _sedimentTypes = value;
                NotifyPropertyChanged(SedimentTypesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SedimentTypesChangedEventArgs = ObservableHelper.CreateArgs<SedimentTreeItem>(x => x.SedimentTypes);
        List<SampleCountTreeItem> _sedimentTypes;

        #endregion
    }
}