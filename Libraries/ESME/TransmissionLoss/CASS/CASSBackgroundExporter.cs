using System;
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
                Console.WriteLine("CASS Exporter got bathymetry...");
                CheckForMutexRelease();
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
                CheckForMutexRelease();
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
                if (_extendedAndAveragedSoundSpeeds == value) return;
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
                if (_wind == value) return;
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
            if ((Bathymetry != null) || ((GeoRect != null) && (Sediment != null) && (ExtendedAndAveragedSoundSpeeds != null) && (Wind != null)))
                _semaphore.Release();
        }

        readonly Semaphore _semaphore = new Semaphore(0, 1);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundExtractor = (CASSBackgroundExporter)e.Argument;

            RunState = "Waiting for extended soundspeeds";
            TaskName = "Exporting NAEMO data";
            Console.WriteLine("CASS Exporter about to wait.");
            _semaphore.WaitOne();
            RunState = "Running";

            if (Bathymetry != null)
            {
                Console.WriteLine("CASS Exporter about export bathymetry...");
                TaskName = "Exporting NAEMO bathymetry";
                Bathymetry.ToYXZ(Path.Combine(backgroundExtractor.DestinationPath, "Bathymetry", "bathymetry.txt"), -1);
            }
            else
            {
                Console.WriteLine("CASS Exporter about export environment data for {0}", TimePeriod);
                TaskName = "Exporting NAEMO environment for " + TimePeriod;
                var environmentFileName = Path.Combine(backgroundExtractor.DestinationPath, "Environment", "env_" + TimePeriod.ToString().ToLower() + ".dat");

                CASSFiles.WriteEnvironmentFile(environmentFileName, GeoRect, Sediment, ExtendedAndAveragedSoundSpeeds[TimePeriod], Wind[TimePeriod], backgroundExtractor);
                Console.WriteLine("CASS Exporter finished exporting environment data for {0}", TimePeriod);
            }

            backgroundExtractor.Value++;
        }
    }
}
