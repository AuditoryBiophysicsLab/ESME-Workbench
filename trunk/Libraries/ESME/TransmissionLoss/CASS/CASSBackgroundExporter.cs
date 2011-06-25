using System.ComponentModel;
using System.IO;
using System.Threading;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;

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
                if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null))) _mutex.ReleaseMutex();
            }
        }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Bathymetry);
        Bathymetry _bathymetry;

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
                if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null))) _mutex.ReleaseMutex();
            }
        }

        static readonly PropertyChangedEventArgs GeoRectChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.GeoRect);
        GeoRect _geoRect;

        #endregion
        
        #region public Sediment Sediment { get; set; }

        public Sediment Sediment
        {
            get { return _sediment; }
            set
            {
                if (_sediment == value) return;
                _sediment = value;
                NotifyPropertyChanged(SedimentChangedEventArgs);
                if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null))) _mutex.ReleaseMutex();
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
                if (_extendedAndAveragedSoundSpeeds == value) return;
                _extendedAndAveragedSoundSpeeds = value;
                NotifyPropertyChanged(ExtendedAndAveragedSoundSpeedsChangedEventArgs);
                if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null))) _mutex.ReleaseMutex();
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
                if (_wind == value) return;
                _wind = value;
                NotifyPropertyChanged(WindChangedEventArgs);
                if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null))) _mutex.ReleaseMutex();
            }
        }

        static readonly PropertyChangedEventArgs WindChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.Wind);
        Wind _wind;

        #endregion

        readonly Mutex _mutex = new Mutex(true);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundExtractor = (NAVOBackgroundExtractor)e.Argument;

            backgroundExtractor.Maximum = 1;
            RunState = "Waiting for extended soundspeeds";
            TaskName = "Exporting NAEMO data";

            _mutex.WaitOne();
            RunState = "Running";

            if (Bathymetry != null)
            {
                TaskName = "Exporting NAEMO bathymetry";
                Bathymetry.ToYXZ(Path.Combine(backgroundExtractor.DestinationPath, "Bathymetry", "bathymetry.txt"), -1);
            }
            else
            {
                TaskName = "Exporting NAEMO environment for " + TimePeriod;
                var environmentFileName = Path.Combine(backgroundExtractor.DestinationPath, "Environment", "env_" + TimePeriod.ToString().ToLower() + ".dat");

                CASSFiles.WriteEnvironmentFile(environmentFileName, GeoRect, Sediment, ExtendedAndAveragedSoundSpeeds[TimePeriod], Wind[TimePeriod]);
            }

            backgroundExtractor.Value++;
            _mutex.ReleaseMutex();
        }
    }
}
