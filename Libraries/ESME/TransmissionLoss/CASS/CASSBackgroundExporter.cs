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

        #region public EnvironmentData<BottomLossSample> BottomLossSample { get; set; }

        public EnvironmentData<BottomLossSample> BottomLossSample
        {
            get { return _bottomLossSample; }
            set
            {
                if (_bottomLossSample == value) return;
                _bottomLossSample = value;
                NotifyPropertyChanged(BottomLossDataChangedEventArgs);
                CheckForSemaphoreRelease();
            }
        }

        static readonly PropertyChangedEventArgs BottomLossDataChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.BottomLossSample);
        EnvironmentData<BottomLossSample> _bottomLossSample;

        #endregion

        #region public bool ExportHFEVA { get; set; }

        public bool ExportHFEVA
        {
            get { return _exportHFEVA; }
            set
            {
                if (_exportHFEVA == value) return;
                _exportHFEVA = value;
                NotifyPropertyChanged(ExportHFEVAChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportHFEVAChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.ExportHFEVA);
        bool _exportHFEVA;

        #endregion

        #region public bool ExportHFBL { get; set; }

        public bool ExportHFBL
        {
            get { return _exportHFBL; }
            set
            {
                if (_exportHFBL == value) return;
                _exportHFBL = value;
                NotifyPropertyChanged(ExportHFBLChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportHFBLChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.ExportHFBL);
        bool _exportHFBL;

        #endregion

        #region public bool ExportLFBLHFB { get; set; }

        public bool ExportLFBLHFB
        {
            get { return _exportLFBLHFB; }
            set
            {
                if (_exportLFBLHFB == value) return;
                _exportLFBLHFB = value;
                NotifyPropertyChanged(ExportLFBLHFBChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportLFBLHFBChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.ExportLFBLHFB);
        bool _exportLFBLHFB;

        #endregion

        #region public bool ExportLFBLPE { get; set; }

        public bool ExportLFBLPE
        {
            get { return _exportLFBLPE; }
            set
            {
                if (_exportLFBLPE == value) return;
                _exportLFBLPE = value;
                NotifyPropertyChanged(ExportLFBLPEChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportLFBLPEChangedEventArgs = ObservableHelper.CreateArgs<CASSBackgroundExporter>(x => x.ExportLFBLPE);
        bool _exportLFBLPE;

        #endregion

        readonly Mutex _mutex = new Mutex();

        void CheckForSemaphoreRelease()
        {
            try
            {
                _mutex.WaitOne();
                var sb = new StringBuilder();
                sb.Append("Waiting for ");
                if (ExportHFEVA && (Sediment == null)) sb.Append("sediment, ");
                if ((ExportHFBL || ExportLFBLHFB || ExportLFBLPE) && (BottomLossSample == null)) sb.Append("bottom loss, ");
                if (Wind == null) sb.Append("wind, ");
                if (ExtendedAndAveragedSoundSpeeds == null) sb.Append("soundspeed, ");
                sb.Remove(sb.Length - 2, 2);
                sb.Append(" data");
                RunState = sb.ToString();
                if (ExportHFEVA && Sediment == null) return;
                if ((ExportHFBL || ExportLFBLHFB || ExportLFBLPE) && (BottomLossSample == null)) return;
                if (Wind == null) return;
                if (ExtendedAndAveragedSoundSpeeds == null) return;
                WaitSemaphore.Release();
            }
            finally
            {
                _mutex.ReleaseMutex();
            }
        }

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundExtractor = (CASSBackgroundExporter)e.Argument;

            RunState = "Waiting for environment data";
            TaskName = "Exporting NAEMO data";
            WaitSemaphore.WaitOne();
            RunState = "Running";
            while ((ExtendedAndAveragedSoundSpeeds[TimePeriod] == null) || (Wind[TimePeriod] == null) || (ExtractionArea == null))
            {
                var sb = new StringBuilder();
                sb.Append("Still waiting on ");
                if (ExtendedAndAveragedSoundSpeeds[TimePeriod] == null) sb.Append("sound speed, ");
                if (Wind[TimePeriod] == null) sb.Append("wind, ");
                if (ExtractionArea == null) sb.Append("extraction area, ");
                if ((ExportHFBL || ExportLFBLHFB || ExportLFBLPE) && (BottomLossSample == null)) sb.Append("bottom loss, ");
                sb.Remove(sb.Length - 2, 2);
                TaskName = sb.ToString();
                Thread.Sleep(100);
            }
            TaskName = "Exporting NAEMO environment for " + TimePeriod;
            var environmentFileName = backgroundExtractor.DestinationPath;
            CASSFiles.WriteEnvironmentFiles(environmentFileName, ExtractionArea, Sediment, ExtendedAndAveragedSoundSpeeds[TimePeriod], Wind[TimePeriod], BathymetryFileName, OverlayFileName, ExportHFEVA, ExportHFBL, ExportLFBLHFB, ExportLFBLPE, BottomLossSample, backgroundExtractor);

            backgroundExtractor.Value++;
        }
    }
}
