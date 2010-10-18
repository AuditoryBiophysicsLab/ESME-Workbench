using System;
using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss;
using HRC.Navigation;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossJobViewModel : EditableValidatingViewModelBase
    {
        #region public DataWrapper<double> Latitude { get; private set; }

        public DataWrapper<double> Latitude
        {
            get { return _latitude; }
            private set
            {
                if (_latitude == value) return;
                _latitude = value;
                NotifyPropertyChanged(LatitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LatitudeChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Latitude);
        DataWrapper<double> _latitude;

        #endregion

        #region public DataWrapper<double> Longitude { get; private set; }

        public DataWrapper<double> Longitude
        {
            get { return _longitude; }
            private set
            {
                if (_longitude == value) return;
                _longitude = value;
                NotifyPropertyChanged(LongitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Longitude);
        DataWrapper<double> _longitude;

        #endregion

        #region public DataWrapper<float> Radius { get; private set; }

        public DataWrapper<float> Radius
        {
            get { return _radius; }
            private set
            {
                if (_radius == value) return;
                _radius = value;
                NotifyPropertyChanged(RadiusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RadiusChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.Radius);
        DataWrapper<float> _radius;

        #endregion

        #region public DataWrapper<float> SourceDepth { get; private set; }

        public DataWrapper<float> SourceDepth
        {
            get { return _sourceDepth; }
            private set
            {
                if (_sourceDepth == value) return;
                _sourceDepth = value;
                NotifyPropertyChanged(SourceDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SourceDepthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.SourceDepth);
        DataWrapper<float> _sourceDepth;

        #endregion

        #region public DataWrapper<float> LowFrequency { get; private set; }

        public DataWrapper<float> LowFrequency
        {
            get { return _lowFrequency; }
            private set
            {
                if (_lowFrequency == value) return;
                _lowFrequency = value;
                NotifyPropertyChanged(LowFrequencyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LowFrequencyChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.LowFrequency);
        DataWrapper<float> _lowFrequency;

        #endregion

        #region public DataWrapper<float> HighFrequency { get; private set; }

        public DataWrapper<float> HighFrequency
        {
            get { return _highFrequency; }
            private set
            {
                if (_highFrequency == value) return;
                _highFrequency = value;
                NotifyPropertyChanged(HighFrequencyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HighFrequencyChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.HighFrequency);
        DataWrapper<float> _highFrequency;

        #endregion

        #region public DataWrapper<float> VerticalBeamWidth { get; private set; }

        public DataWrapper<float> VerticalBeamWidth
        {
            get { return _verticalBeamWidth; }
            private set
            {
                if (_verticalBeamWidth == value) return;
                _verticalBeamWidth = value;
                NotifyPropertyChanged(VerticalBeamWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs VerticalBeamWidthChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.VerticalBeamWidth);
        DataWrapper<float> _verticalBeamWidth;

        #endregion

        #region public DataWrapper<float> DepressionElevationAngle { get; private set; }

        public DataWrapper<float> DepressionElevationAngle
        {
            get { return _depressionElevationAngle; }
            private set
            {
                if (_depressionElevationAngle == value) return;
                _depressionElevationAngle = value;
                NotifyPropertyChanged(DepressionElevationAngleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepressionElevationAngleChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.DepressionElevationAngle);
        DataWrapper<float> _depressionElevationAngle;

        #endregion

        #region public DataWrapper<float> RadialBearing { get; private set; }

        public DataWrapper<float> RadialBearing
        {
            get { return _radialBearing; }
            private set
            {
                if (_radialBearing == value) return;
                _radialBearing = value;
                NotifyPropertyChanged(RadialBearingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RadialBearingChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.RadialBearing);
        DataWrapper<float> _radialBearing;

        #endregion

        #region public DataWrapper<int> RadialCount { get; private set; }

        public DataWrapper<int> RadialCount
        {
            get { return _radialCount; }
            private set
            {
                if (_radialCount == value) return;
                _radialCount = value;
                NotifyPropertyChanged(RadialCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RadialCountChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.RadialCount);
        DataWrapper<int> _radialCount;

        #endregion

        readonly IEnumerable<DataWrapperBase> _cachedListOfDataWrappers;
        readonly int _maxCalculationDepth;
        public TransmissionLossJobViewModel(EarthCoordinate location, float platformDepth, NemoMode nemoMode, int radialCount, int maxCalculationDepth)
        {
            #region Create DataWrappers

            Latitude = new DataWrapper<double>(this, LatitudeChangedEventArgs);
            Longitude = new DataWrapper<double>(this, LongitudeChangedEventArgs);
            Radius = new DataWrapper<float>(this, RadiusChangedEventArgs);
            RadialBearing = new DataWrapper<float>(this, RadialBearingChangedEventArgs);
            SourceDepth = new DataWrapper<float>(this, SourceDepthChangedEventArgs);
            RadialCount = new DataWrapper<int>(this, RadialCountChangedEventArgs);
            LowFrequency = new DataWrapper<float>(this, LowFrequencyChangedEventArgs);
            HighFrequency = new DataWrapper<float>(this, HighFrequencyChangedEventArgs);
            VerticalBeamWidth = new DataWrapper<float>(this, VerticalBeamWidthChangedEventArgs);
            DepressionElevationAngle = new DataWrapper<float>(this, DepressionElevationAngleChangedEventArgs);

            #endregion

            Latitude.DataValue = location.Latitude_degrees;
            Longitude.DataValue = location.Longitude_degrees;
            Radius.DataValue = nemoMode.Radius;
            SourceDepth.DataValue = platformDepth + nemoMode.DepthOffset;
            LowFrequency.DataValue = nemoMode.LowFrequency;
            HighFrequency.DataValue = nemoMode.HighFrequency;
            VerticalBeamWidth.DataValue = nemoMode.VerticalBeamWidth;
            DepressionElevationAngle.DataValue = nemoMode.DepressionElevationAngle;
            RadialBearing.DataValue = 0;
            RadialCount.DataValue = radialCount;
            _maxCalculationDepth = maxCalculationDepth;

            _cachedListOfDataWrappers = DataWrapperHelper.GetWrapperProperties(this);
            foreach (var wrapper in _cachedListOfDataWrappers) wrapper.IsEditable = false;

            #region Create validation rules

            Latitude.AddRule(new SimpleRule("DataValue", "Latitude must be in the range -90 to +90", domObj =>
            {
                var obj = (DataWrapper<double>)domObj;
                return ((obj.DataValue < -90) || (90 < obj.DataValue));
            }));
            Longitude.AddRule(new SimpleRule("DataValue", "Longitude must be in the range -180 to +180", domObj =>
            {
                var obj = (DataWrapper<double>)domObj;
                return ((obj.DataValue < -180) || (180 < obj.DataValue));
            }));
            Radius.AddRule(new SimpleRule("DataValue", "Radius must be greater than zero", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue <= 0);
            }));
            RadialBearing.AddRule(new SimpleRule("DataValue", "RadialBearing must be in the range -180 to +180", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return ((obj.DataValue < -180) || (180 < obj.DataValue));
            }));
            SourceDepth.AddRule(new SimpleRule("DataValue", "SourceDepth must be greater than zero", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue <= 0);
            }));
            RadialCount.AddRule(new SimpleRule("DataValue", "RadialCount must be greater than zero", domObj =>
            {
                var obj = (DataWrapper<int>)domObj;
                return (obj.DataValue <= 0);
            }));
            LowFrequency.AddRule(new SimpleRule("DataValue", "LowFrequency must be greater than zero", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue <= 0);
            }));
            LowFrequency.AddRule(new SimpleRule("DataValue", "LowFrequency must be less than HighFrequency", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue >= HighFrequency.DataValue);
            }));
            HighFrequency.AddRule(new SimpleRule("DataValue", "HighFrequency must be greater than zero", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue <= 0);
            }));
            HighFrequency.AddRule(new SimpleRule("DataValue", "HighFrequency must be greater than LowFrequency", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return (obj.DataValue <= LowFrequency.DataValue);
            }));
            VerticalBeamWidth.AddRule(new SimpleRule("DataValue", "VerticalBeamWidth must be between zero and +180", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return ((0 <= obj.DataValue) && (180 < obj.DataValue));
            }));
            DepressionElevationAngle.AddRule(new SimpleRule("DataValue", "DepressionElevationAngle must be in the range -90 to +90", domObj =>
            {
                var obj = (DataWrapper<float>)domObj;
                return ((obj.DataValue < -90) || (90 < obj.DataValue));
            }));

            #endregion

            OkCommand = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(true); });
        }

        public TransmissionLossJob TransmissionLossJob
        {
            get
            {
                return new TransmissionLossJob
                {
                    AcousticProperties = new AcousticProperties
                    {
                        SourceDepth = SourceDepth.DataValue,
                        VerticalBeamWidth = VerticalBeamWidth.DataValue,
                        DepressionElevationAngle = DepressionElevationAngle.DataValue,
                        LowFrequency = LowFrequency.DataValue,
                        HighFrequency = HighFrequency.DataValue,
                    },
                    AnalysisPoint = new AnalysisPoint
                    {
                        IDField = 1,
                        Location = new EarthCoordinate(Latitude.DataValue, Longitude.DataValue),
                        RadialBearing = RadialBearing.DataValue,
                        RadialCount = RadialCount.DataValue,
                    },
                    Radius = (int)Radius.DataValue,
                    MaxDepth = _maxCalculationDepth,
                };
            }
        }

        public SimpleCommand<Object, Object> OkCommand { get; private set; }
    }
}