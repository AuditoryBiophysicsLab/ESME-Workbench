using System.ComponentModel;
using System.Threading;
using Cinch;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class GDEMBackgroundExtractor : NAVOBackgroundExtractor
    {
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
            SoundSpeedField temperatureField;
            SoundSpeedField salinityField;
            RunState = "Running";
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;
            TaskName = "Temperature and salinity data extraction for " + backgroundExtractor.TimePeriod;
            backgroundExtractor.Maximum = 5;
            GeneralizedDigitalEnvironmentModelDatabase.ExtractArea(backgroundExtractor, out temperatureField, out salinityField);
            TaskName = "Soundspeed calculation for " + TimePeriod;
            RunState = "Waiting for bathymetry";
            _maxDepthMutex.WaitOne();
            RunState = "Running";
            backgroundExtractor.Status = "Creating soundspeed profile for " + TimePeriod;

            var temperature = new SoundSpeed();
            temperature.SoundSpeedFields.Add(temperatureField);
            
            var salinity = new SoundSpeed();
            salinity.SoundSpeedFields.Add(salinityField);
            
            var soundSpeed = SoundSpeed.Create(temperature, salinity);
            backgroundExtractor.Value++;

            backgroundExtractor.Status = "Extending soundspeed profile for " + TimePeriod;
            soundSpeed.SoundSpeedFields[0].Extend(temperatureField, salinityField, MaxDepth, backgroundExtractor.ExtractionArea);
            ExtendedSoundSpeedField = soundSpeed.SoundSpeedFields[0];
            backgroundExtractor.Value++;
            _maxDepthMutex.ReleaseMutex();
        }
    }
}