using System.ComponentModel;
using System.Text;
using System.Threading;
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
                CheckForSemaphoreRelease();
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        #endregion

        #region public string BathymetryFileName { get; set; }

        public string BathymetryFileName
        {
            get { return _bathymetryFileName; }
            set
            {
                if (_bathymetryFileName == value) return;
                _bathymetryFileName = value;
                NotifyPropertyChanged(BathymetryFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryFileNameChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.BathymetryFileName);
        string _bathymetryFileName;

        #endregion

        #region public string OverlayFileName { get; set; }

        public string OverlayFileName
        {
            get { return _overlayFileName; }
            set
            {
                if (_overlayFileName == value) return;
                _overlayFileName = value;
                NotifyPropertyChanged(OverlayFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayFileNameChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.OverlayFileName);
        string _overlayFileName;

        #endregion

        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
                CheckForSemaphoreRelease();
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
                CheckForSemaphoreRelease();
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
                CheckForSemaphoreRelease();
            }
        }

        static readonly PropertyChangedEventArgs WindChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Wind);
        Wind _wind;

        #endregion

        readonly Mutex _mutex = new Mutex();

        void CheckForSemaphoreRelease()
        {
            _mutex.WaitOne();
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
            _mutex.ReleaseMutex();
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
                while ((ExtendedAndAveragedSoundSpeeds[TimePeriod] == null) || (Wind[TimePeriod] == null) || (ExtractionArea == null))
                {
                    var sb = new StringBuilder();
                    sb.Append("Still waiting on ");
                    if (ExtendedAndAveragedSoundSpeeds[TimePeriod] == null) sb.Append("sound speed, ");
                    if (Wind[TimePeriod] == null) sb.Append("wind, ");
                    if (ExtractionArea == null) sb.Append("extraction area, ");
                    sb.Remove(sb.Length - 2, 2);
                    TaskName = sb.ToString();
                    Thread.Sleep(100);
                }
                TaskName = "Exporting NAEMO environment for " + TimePeriod;
                var environmentFileName = backgroundExtractor.DestinationPath;
                CASSFiles.WriteEnvironmentFile(environmentFileName, ExtractionArea, Sediment, ExtendedAndAveragedSoundSpeeds[TimePeriod], Wind[TimePeriod], backgroundExtractor, BathymetryFileName, OverlayFileName);
            }

            backgroundExtractor.Value++;
        }
    }
}
