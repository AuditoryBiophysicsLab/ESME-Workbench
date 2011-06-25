using System.ComponentModel;
using System.Threading;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class GDEMBackgroundExtractor : NAVOBackgroundExtractor
    {
        #region public SoundSpeedField TemperatureField { get; set; }

        public SoundSpeedField TemperatureField
        {
            get { return _temperatureField; }
            set
            {
                if (_temperatureField == value) return;
                _temperatureField = value;
                NotifyPropertyChanged(TemperatureFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TemperatureFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.TemperatureField);
        SoundSpeedField _temperatureField;

        #endregion

        #region public SoundSpeedField SalinityField { get; set; }

        public SoundSpeedField SalinityField
        {
            get { return _salinityField; }
            set
            {
                if (_salinityField == value) return;
                _salinityField = value;
                NotifyPropertyChanged(SalinityFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SalinityFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.SalinityField);
        SoundSpeedField _salinityField;

        #endregion

        #region public EarthCoordinate<float> MaxDepth { get; set; }

        public EarthCoordinate<float> MaxDepth
        {
            get { return _maxDepth; }
            set
            {
                if (_maxDepth == value) return;
                _maxDepth = value;
                NotifyPropertyChanged(MaxDepthChangedEventArgs);
                if (_maxDepth != null) _maxDepthMutex.ReleaseMutex();
            }
        }

        static readonly PropertyChangedEventArgs MaxDepthChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.MaxDepth);
        EarthCoordinate<float> _maxDepth;

        #endregion

        #region public SoundSpeedField ExtendedSoundSpeedField { get; set; }

        public SoundSpeedField ExtendedSoundSpeedField
        {
            get { return _extendedSoundSpeedField; }
            set
            {
                if (_extendedSoundSpeedField == value) return;
                _extendedSoundSpeedField = value;
                NotifyPropertyChanged(ExtendedSoundSpeedFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtendedSoundSpeedFieldChangedEventArgs = ObservableHelper.CreateArgs<GDEMBackgroundExtractor>(x => x.ExtendedSoundSpeedField);
        SoundSpeedField _extendedSoundSpeedField;

        #endregion

        readonly Mutex _maxDepthMutex = new Mutex(true);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            RunState = "Running";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            TaskName = "Temperature and salinity data extraction for " + backgroundExtractor.TimePeriod;
            backgroundExtractor.Maximum = 5;
            GeneralizedDigitalEnvironmentModelDatabase.ExtractArea(backgroundExtractor, out _temperatureField, out _salinityField);
            TaskName = "Soundspeed calculation for " + TimePeriod;
            RunState = "Waiting for bathymetry";
            _maxDepthMutex.WaitOne();
            RunState = "Running";
            backgroundExtractor.Status = "Creating soundspeed profile for " + TimePeriod;

            var temperature = new SoundSpeed();
            temperature.SoundSpeedFields.Add(_temperatureField);
            
            var salinity = new SoundSpeed();
            salinity.SoundSpeedFields.Add(_salinityField);
            
            var soundSpeed = SoundSpeed.Create(temperature, salinity);
            backgroundExtractor.Value++;

            backgroundExtractor.Status = "Extending soundspeed profile for " + TimePeriod;
            soundSpeed.SoundSpeedFields[0].Extend(_temperatureField, _salinityField, MaxDepth, backgroundExtractor.ExtractionArea);
            ExtendedSoundSpeedField = soundSpeed.SoundSpeedFields[0];
            backgroundExtractor.Value++;
            _maxDepthMutex.ReleaseMutex();
        }
    }
}