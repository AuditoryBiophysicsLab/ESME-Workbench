using System;
using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossJobViewModel : EditableValidatingViewModelBase
    {
        #region public DataWrapper<double> Latitude { get; set; }

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

        #region public DataWrapper<double> Longitude { get; set; }

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

        #region public DataWrapper<int> Radius { get; set; }

        public DataWrapper<int> Radius
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
        DataWrapper<int> _radius;

        #endregion

        #region public DataWrapper<float> RadialBearing { get; set; }

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

        #region public DataWrapper<float> SourceDepth { get; set; }

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

        #region public DataWrapper<int> RadialCount { get; set; }

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

        #region public DataWrapper<float> LowFrequency { get; set; }

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

        #region public DataWrapper<float> HighFrequency { get; set; }

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

        #region public DataWrapper<float> VerticalBeamWidth { get; set; }

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

        #region public DataWrapper<float> DepressionElevationAngle { get; set; }

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

        static readonly PropertyChangedEventArgs TransmissionLossJobChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossJobViewModel>(x => x.TransmissionLossJob);

        TransmissionLossJob _transmissionLossJob;
        public TransmissionLossJobViewModel()
        {
            #region Create DataWrappers

            Latitude = new DataWrapper<double>(this, LatitudeChangedEventArgs);
            Longitude = new DataWrapper<double>(this, LongitudeChangedEventArgs);
            Radius = new DataWrapper<int>(this, RadiusChangedEventArgs);
            RadialBearing = new DataWrapper<float>(this, RadialBearingChangedEventArgs);
            SourceDepth = new DataWrapper<float>(this, SourceDepthChangedEventArgs);
            RadialCount = new DataWrapper<int>(this, RadialCountChangedEventArgs);
            LowFrequency = new DataWrapper<float>(this, LowFrequencyChangedEventArgs);
            HighFrequency = new DataWrapper<float>(this, HighFrequencyChangedEventArgs);
            VerticalBeamWidth = new DataWrapper<float>(this, VerticalBeamWidthChangedEventArgs);
            DepressionElevationAngle = new DataWrapper<float>(this, DepressionElevationAngleChangedEventArgs);

            #endregion

            OkCommand = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(true); });
        }

        public TransmissionLossJob TransmissionLossJob
        {
            get { return _transmissionLossJob; }
            set
            {
                if (value == _transmissionLossJob) return;
                _transmissionLossJob = value;
                NotifyPropertyChanged(TransmissionLossJobChangedEventArgs);
            }
        }

        public SimpleCommand<Object, Object> OkCommand { get; private set; }
    }
}