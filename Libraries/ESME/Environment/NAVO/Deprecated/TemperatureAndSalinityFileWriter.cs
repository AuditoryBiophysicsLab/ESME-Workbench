using System.ComponentModel;
using System.IO;
using System.Threading;
using Cinch;

namespace ESME.Environment.NAVO
{
#if false
    public class TemperatureAndSalinityFileWriter : NAVOBackgroundExtractor
    {
        #region public SoundSpeed Temperature { get; set; }

        public SoundSpeed Temperature
        {
            get { return _temperature; }
            set
            {
                if (_temperature == value) return;
                _temperature = value;
                NotifyPropertyChanged(TemperatureChangedEventArgs);
                if ((Temperature != null) && (Salinity != null)) WaitSemaphore.Release();
            }
        }

        static readonly PropertyChangedEventArgs TemperatureChangedEventArgs = ObservableHelper.CreateArgs<TemperatureAndSalinityFileWriter>(x => x.Temperature);
        SoundSpeed _temperature;

        #endregion

        #region public SoundSpeed Salinity { get; set; }

        public SoundSpeed Salinity
        {
            get { return _salinity; }
            set
            {
                if (_salinity == value) return;
                _salinity = value;
                NotifyPropertyChanged(SalinityChangedEventArgs);
                if ((Temperature != null) && (Salinity != null)) WaitSemaphore.Release();
            }
        }

        static readonly PropertyChangedEventArgs SalinityChangedEventArgs = ObservableHelper.CreateArgs<TemperatureAndSalinityFileWriter>(x => x.Salinity);
        SoundSpeed _salinity;

        #endregion

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundTask = (TemperatureAndSalinityFileWriter)e.Argument;
            backgroundTask.Maximum = 2;
            TaskName = "Save soundspeed data";
            RunState = "Waiting for calculations to complete";
            WaitSemaphore.WaitOne();
            RunState = "Saving temperature data";
            Temperature.Save(Path.Combine(backgroundTask.DestinationPath, "temperature.xml"));
            backgroundTask.Value++;
            RunState = "Saving salinity data";
            Salinity.Save(Path.Combine(backgroundTask.DestinationPath, "salinity.xml"));
            backgroundTask.Value++;
        }
    }
#endif
}
