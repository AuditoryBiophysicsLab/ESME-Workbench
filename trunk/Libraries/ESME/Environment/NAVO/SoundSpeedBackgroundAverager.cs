using System.ComponentModel;
using System.Threading;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class SoundSpeedBackgroundAverager : NAVOBackgroundExtractor
    {
        #region public SoundSpeed ExtendedMonthlySoundSpeeds { get; set; }

        public SoundSpeed ExtendedMonthlySoundSpeeds
        {
            get { return _extendedMonthlySoundSpeeds; }
            set
            {
                if (_extendedMonthlySoundSpeeds == value) return;
                _extendedMonthlySoundSpeeds = value;
                NotifyPropertyChanged(MonthlySoundSpeedsChangedEventArgs);
                if (_extendedMonthlySoundSpeeds != null) _mutex.ReleaseMutex();
            }
        }

        static readonly PropertyChangedEventArgs MonthlySoundSpeedsChangedEventArgs = ObservableHelper.CreateArgs<SoundSpeedBackgroundAverager>(x => x.ExtendedMonthlySoundSpeeds);
        SoundSpeed _extendedMonthlySoundSpeeds;

        #endregion

        #region public SoundSpeedField ExtendedAverageSoundSpeedField { get; set; }

        public SoundSpeedField ExtendedAverageSoundSpeedField
        {
            get { return _extendedAverageSoundSpeedField; }
            set
            {
                if (_extendedAverageSoundSpeedField == value) return;
                _extendedAverageSoundSpeedField = value;
                NotifyPropertyChanged(ExtendedAverageSoundSpeedFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtendedAverageSoundSpeedFieldChangedEventArgs = ObservableHelper.CreateArgs<SoundSpeedBackgroundAverager>(x => x.ExtendedAverageSoundSpeedField);
        SoundSpeedField _extendedAverageSoundSpeedField;

        #endregion

        readonly Mutex _mutex = new Mutex(true);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;

            RunState = "Waiting for monthly soundspeeds";
            TaskName = "Soundspeed averaging for " + backgroundExtractor.TimePeriod;

            _mutex.WaitOne();

            RunState = "Running";
            if (UniqueMonths.Count <= 1)
            {
                backgroundExtractor.Maximum = 1;
                ExtendedAverageSoundSpeedField = ExtendedMonthlySoundSpeeds[backgroundExtractor.TimePeriod];
                backgroundExtractor.Value = 1;
                return;
            }

            ExtendedAverageSoundSpeedField = SoundSpeed.Average(ExtendedMonthlySoundSpeeds, backgroundExtractor.TimePeriod, this);
            _mutex.ReleaseMutex();
        }
    }
}