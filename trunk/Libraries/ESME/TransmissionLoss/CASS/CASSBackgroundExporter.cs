using System.ComponentModel;
using System.Text;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;

namespace ESME.TransmissionLoss.CASS
{
    public class CASSBackgroundExporter : NAVOBackgroundExtractor
    {
        #region public Bathymetry Bathymetry { get; set; }

        public Bathymetry Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
                //Console.WriteLine("CASS Exporter got bathymetry...");
                CheckForMutexRelease();
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        #endregion

        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
                CheckForMutexRelease();
            }
        }

        static readonly PropertyChangedEventArgs SedimentChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Sediment);
        Sediment _sediment;

        #endregion

        #region public SoundSpeed ExtendedAndAveragedSoundSpeeds { get; set; }

        public SoundSpeed ExtendedAndAveragedSoundSpeeds
        {
            get { return _extendedAndAveragedSoundSpeeds; }
            set
            {
                _extendedAndAveragedSoundSpeeds = value;
                NotifyPropertyChanged(ExtendedAndAveragedSoundSpeedsChangedEventArgs);
                CheckForMutexRelease();
            }
        }

        static readonly PropertyChangedEventArgs ExtendedAndAveragedSoundSpeedsChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.ExtendedAndAveragedSoundSpeeds);
        SoundSpeed _extendedAndAveragedSoundSpeeds;

        #endregion

        #region public Wind Wind { get; set; }

        public Wind Wind
        {
            get { return _wind; }
            set
            {
                _wind = value;
                NotifyPropertyChanged(WindChangedEventArgs);
                CheckForMutexRelease();
            }
        }

        static readonly PropertyChangedEventArgs WindChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Wind);
        Wind _wind;

        #endregion

        void CheckForMutexRelease()
        {
            var sb = new StringBuilder();
            sb.Append("Waiting for ");
            if (Sediment == null) sb.Append("sediment, ");
            if (Wind == null) sb.Append("wind, ");
            if (ExtendedAndAveragedSoundSpeeds == null) sb.Append("soundspeed, ");
            sb.Remove(sb.Length - 2, 2);
            sb.Append(" data");
            RunState = sb.ToString();
            if ((Bathymetry != null) || ((Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null)))
                WaitSemaphore.Release();
        }

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundExtractor = (CASSBackgroundExporter)e.Argument;

            RunState = "Waiting for extended sediment, wind, soundspeed data";
            TaskName = "Exporting NAEMO data";
            WaitSemaphore.WaitOne();
            RunState = "Running";

            if (Bathymetry != null)
            {
                TaskName = "Exporting NAEMO bathymetry";
                Bathymetry.ToYXZ(backgroundExtractor.DestinationPath, -1);
            }
            else
            {
                TaskName = "Exporting NAEMO environment for " + TimePeriod;
                var environmentFileName = backgroundExtractor.DestinationPath;

                CASSFiles.WriteEnvironmentFile(environmentFileName, ExtractionArea, Sediment, ExtendedAndAveragedSoundSpeeds[TimePeriod], Wind[TimePeriod], backgroundExtractor);
            }

            backgroundExtractor.Value++;
        }
    }
}
