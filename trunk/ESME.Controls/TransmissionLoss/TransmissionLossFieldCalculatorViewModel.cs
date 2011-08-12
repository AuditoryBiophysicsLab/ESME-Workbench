using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Windows.Threading;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.CASS;
using ESME.TransmissionLoss.RAM;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossFieldCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossFieldCalculatorViewModel(string runfileName, Dispatcher dispatcher)
        {
            Status = "Loading";
            RunfileName = runfileName;
            _dispatcher = dispatcher;
            TransmissionLossRunFile = TransmissionLossRunFile.Load(RunfileName);
            Status = "Waiting";
        }

        #endregion

        public void PrepareRadials()
        {
            if (Status != "Waiting") return;
            Status = "Loading environment";
            LoadEnvironment();
            Status = "Preparing radial calculation threads";
            SetupRadialViewModels();
            Status = "Ready";
        }

        #region public string RunfileName { get; set; }

        public string RunfileName
        {
            get { return _runfileName; }
            set
            {
                if (_runfileName == value) return;
                _runfileName = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(RunfileFilenameChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs RunfileFilenameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.RunfileName);
        string _runfileName;

        #endregion

        #region public Dispatcher Dispatcher { get; set; }

        public Dispatcher Dispatcher
        {
            get { return _dispatcher; }
            set
            {
                if (_dispatcher == value) return;
                _dispatcher = value;
                foreach (var radial in RadialCalculatorViewModels) radial.Dispatcher = _dispatcher;
            }
        }

        Dispatcher _dispatcher;

        #endregion

        #region public TransmissionLossField TransmissionLossField { get; set; }

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            set
            {
                if (_transmissionLossField == value) return;
                _transmissionLossField = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TransmissionLossField);
        TransmissionLossField _transmissionLossField;

        #endregion

        #region public float TotalProgress { get; set; }

        public float TotalProgress
        {
            get { return _totalProgress; }
            set
            {
                if (_totalProgress >= value) return;
                _totalProgress = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() =>
                {
                    NotifyPropertyChanged(TotalProgressChangedEventArgs);
                    CommandManager.InvalidateRequerySuggested();
                });
            }
        }

        static readonly PropertyChangedEventArgs TotalProgressChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TotalProgress);
        float _totalProgress;

        #endregion

        #region public int RadialCount { get; set; }

        public int RadialCount
        {
            get { return _radialCount; }
            set
            {
                if (_radialCount == value) return;
                _radialCount = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(RadialCountChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs RadialCountChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.RadialCount);
        int _radialCount;

        #endregion

        #region public TransmissionLossRunFile TransmissionLossRunFile { get; set; }

        public TransmissionLossRunFile TransmissionLossRunFile
        {
            get { return _runFile; }
            set
            {
                if (_runFile == value) return;
                _runFile = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossRunFileChangedEventArgs));
                if (_runFile == null) return;
                RadialCount = _runFile.TransmissionLossJob.SoundSource.RadialBearings.Count;
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRunFileChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TransmissionLossRunFile);
        TransmissionLossRunFile _runFile;
        NAEMOEnvironmentFile _environment;

        void LoadEnvironment()
        {
            var bathymetryPath = Path.Combine(TransmissionLossRunFile.ScenarioDataDirectory, TransmissionLossRunFile.RangeComplexName, "Bathymetry", TransmissionLossRunFile.BathymetryName + ".txt");
            var environmentPath = Path.Combine(TransmissionLossRunFile.ScenarioDataDirectory, TransmissionLossRunFile.RangeComplexName, "Environment", TransmissionLossRunFile.EnvironmentName + ".dat");
            _environment = NAEMOEnvironmentFile.Load(environmentPath);
            _environment.EnvironmentInformation.Bathymetry = Bathymetry.FromYXZ(bathymetryPath, -1);
        }

        void SetupRadialViewModels()
        {
            var rangeCellCount = 0;
            BellhopSettings bellhopSettings;
            RAMSettings ramSettings;
            var transmissionLossJob = TransmissionLossRunFile.TransmissionLossJob;
            float depthCellSize = 0;
            switch (TransmissionLossRunFile.TransmissionLossAlgorithm)
            {
                case TransmissionLossAlgorithm.Bellhop:
                    bellhopSettings = ((BellhopRunFile)TransmissionLossRunFile).BellhopSettings;
                    depthCellSize = bellhopSettings.DepthCellSize;
                    rangeCellCount =
                            (int)
                            Math.Round((transmissionLossJob.SoundSource.Radius / bellhopSettings.RangeCellSize)) + 1;
                    break;
                case TransmissionLossAlgorithm.RAMGEO:
                    ramSettings = ((RamRunFile)TransmissionLossRunFile).RAMSettings;
                    depthCellSize = ramSettings.DepthStepSize;
                    break;
                default:
                    break;
            }
            TransmissionLossField = new TransmissionLossField(TransmissionLossRunFile);
            var maxCalculationDepthMeters = float.MinValue;
            var radialCount = transmissionLossJob.SoundSource.RadialBearings.Count;
            var bottomProfiles = new BottomProfile[radialCount];
            var soundSpeedProfiles = new SoundSpeedProfile[radialCount];
            var windSpeeds = new float[radialCount];
            var useWindSpeed = false;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var curTransect = new Transect(null, transmissionLossJob.SoundSource, radialBearing,
                                               transmissionLossJob.SoundSource.Radius);
                bottomProfiles[bearingIndex] = new BottomProfile(rangeCellCount, curTransect, _environment.EnvironmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float)bottomProfiles[bearingIndex].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[bearingIndex] = _environment.EnvironmentInformation.SoundSpeedField.EnvironmentData[curTransect.MidPoint];
                windSpeeds[bearingIndex] = _environment.EnvironmentInformation.Wind.TimePeriods[0].EnvironmentData[curTransect.MidPoint].Data;
            }
            maxCalculationDepthMeters *= 1.1f;
            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters / depthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = _environment.EnvironmentInformation.Sediment.Samples[transmissionLossJob.SoundSource];
                TransmissionLossRadialCalculatorViewModel radialViewModel;
                switch (TransmissionLossRunFile.TransmissionLossAlgorithm)
                {
                    case TransmissionLossAlgorithm.Bellhop:
                        var bellhopConfig = Bellhop.GetRadialConfiguration(transmissionLossJob,
                                                                           soundSpeedProfiles[bearingIndex],
                                                                           sedimentType, maxCalculationDepthMeters,
                                                                           rangeCellCount, depthCellCount, false,
                                                                           false, false, 1500);
                        TransmissionLossRunFile.TransmissionLossRunFileRadials.Add(new BellhopRunFileRadial
                        {
                                BearingFromSourceDegrees = radialBearing,
                                Configuration = bellhopConfig,
                                BottomProfile = bottomProfiles[bearingIndex].ToBellhopString()
                        });
                        // todo: Once we know how to calculate the Top Reflection Coefficients for Bellhop, put them in here
                        // 
                        radialViewModel =
                                new BellhopRadialCalculatorViewModel(
                                        (BellhopRunFileRadial)_runFile.TransmissionLossRunFileRadials[bearingIndex],
                                        bearingIndex, _dispatcher);
                        radialViewModel.PropertyChanged += (s, e) =>
                        {
                            if (_dispatcher != null) ((BellhopRadialCalculatorViewModel)s).Dispatcher = _dispatcher;
                            if (e.PropertyName != "ProgressPercent") return;
                            //float radialCount = RadialCalculatorViewModels.Count;
                            var progress =
                                    RadialCalculatorViewModels.Sum(radial => radial.ProgressPercent / radialCount);
                            TotalProgress = progress;
                        };
                        radialViewModel.CalculationCompleted += (s, e) => RadialCount = RadialCalculatorViewModels.Count(radial => radial.ProgressPercent < 100);
                        if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => RadialCalculatorViewModels.Add(radialViewModel));
                        else RadialCalculatorViewModels.Add(radialViewModel);
                        break;
                    case TransmissionLossAlgorithm.RAMGEO:
                        var ramConfig = Ram.GetRadialConfiguration(transmissionLossJob,
                                                                   soundSpeedProfiles[bearingIndex],
                                                                   bottomProfiles[bearingIndex], sedimentType,
                                                                   maxCalculationDepthMeters, rangeCellCount,
                                                                   depthCellCount);
                        TransmissionLossRunFile.TransmissionLossRunFileRadials.Add(new RamRunFileRadial
                        {BearingFromSourceDegrees = radialBearing, Configuration = ramConfig});
                        break;
                    default:
                        break;
                }
            }
            _environment = null;
        }

        public void Start(RunWorkerCompletedEventHandler runWorkerCompletedEventHandler)
        {
            PrepareRadials();
            while (Status != "Ready")
                Thread.Sleep(100);
            lock (this)
            {
                if (IsStarted) return;
                IsStarted = true;
                var bw = new BackgroundWorker();
                bw.DoWork += Calculate;
                bw.RunWorkerCompleted += delegate { IsCompleted = true; };
                if (runWorkerCompletedEventHandler != null) bw.RunWorkerCompleted += runWorkerCompletedEventHandler;
                bw.RunWorkerAsync(_runFile);
            }
        }
        #endregion

        #region public bool IsStarted { get; set; }

        public bool IsStarted
        {
            get { return _isStarted; }
            set
            {
                if (_isStarted == value) return;
                _isStarted = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(IsStartedChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.IsStarted);
        bool _isStarted;

        #endregion

        #region public bool IsCompleted { get; set; }

        public bool IsCompleted
        {
            get { return _isCompleted; }
            set
            {
                if (_isCompleted == value) return;
                _isCompleted = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(IsCompletedChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs IsCompletedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.IsCompleted);
        bool _isCompleted;

        #endregion

        #region public ObservableCollection<TransmissionLossRadialCalculatorViewModel> RadialCalculatorViewModels { get; set; }

        public ObservableCollection<TransmissionLossRadialCalculatorViewModel> RadialCalculatorViewModels
        {
            get { return _radialCalculatorViewModels; }
            set
            {
                if (_radialCalculatorViewModels == value) return;
                if (_radialCalculatorViewModels != null) _radialCalculatorViewModels.CollectionChanged -= RadialCalculatorViewModelsCollectionChanged;
                _radialCalculatorViewModels = value;
                if (_radialCalculatorViewModels != null) _radialCalculatorViewModels.CollectionChanged += RadialCalculatorViewModelsCollectionChanged;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs));
            }
        }

        void RadialCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs); }
        static readonly PropertyChangedEventArgs BellhopRadialCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.RadialCalculatorViewModels);
        ObservableCollection<TransmissionLossRadialCalculatorViewModel> _radialCalculatorViewModels = new ObservableCollection<TransmissionLossRadialCalculatorViewModel>();

        #endregion

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(StatusChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.Status);
        string _status;

        #endregion

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ??
                       (_cancel =
                        new SimpleCommand<object, object>(delegate { return IsCancelCommandEnabled; },
                                                          delegate { CancelHandler(); }));
            }
        }

        SimpleCommand<object, object> _cancel;

        bool IsCancelCommandEnabled
        {
            get { return (!CancelRequested) && (TotalProgress < 100); }
        }

        void CancelHandler() { CancelRequested = true; }
        #endregion

        #region public bool CancelRequested { get; set; }

        public bool CancelRequested
        {
            get { return _cancelRequested; }
            set
            {
                if (_cancelRequested == value) return;
                _cancelRequested = value;
                foreach (var radial in RadialCalculatorViewModels) radial.CancelRequested = _cancelRequested;
                NotifyPropertyChanged(CancelRequestedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CancelRequestedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.CancelRequested);
        bool _cancelRequested;

        #endregion

        #region BackgroundWorker thread that computes the TransmissionLossField in parallel

        void Calculate(object sender, DoWorkEventArgs args)
        {
            PrepareRadials();
            //if (TransmissionLossField.Radials == null) throw new ApplicationException("Radials are null");
            var runFile = (TransmissionLossRunFile) args.Argument;
            var radialNum = 0;
            var radialProgress = 100f/runFile.TransmissionLossRunFileRadials.Count;
            TotalProgress = 0f;

            Status = "Calculating";

            Parallel.ForEach<TransmissionLossRunFileRadial, float>(runFile.TransmissionLossRunFileRadials, () => 0, (radial, loopstate, progress) =>
            {
                var localRadialNum = Interlocked.Increment(ref radialNum);
                var radialViewModel = RadialCalculatorViewModels[localRadialNum - 1];
                radialViewModel.Start();
                return radialProgress;
            }, finalResult => TotalProgress += finalResult);

            if (CancelRequested)
            {
                Status = "Canceling";
                return;
            }

            Status = "Finishing";
            foreach (var radial in RadialCalculatorViewModels) if (radial.TransmissionLossRadial != null) TransmissionLossField.AddRadial(radial.TransmissionLossRadial);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs));
        }

        #endregion
    }
}