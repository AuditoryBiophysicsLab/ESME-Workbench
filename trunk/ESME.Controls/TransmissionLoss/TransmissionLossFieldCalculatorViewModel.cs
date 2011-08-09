using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
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
            RunfileName = runfileName;
            _dispatcher = dispatcher;
            TransmissionLossRunFile = TransmissionLossRunFile.Load(runfileName);
        }

        #endregion

        #region public string RunfileName { get; set; }

        public string RunfileName
        {
            get { return _runfileName; }
            set
            {
                if (_runfileName == value) return;
                _runfileName = value;
                NotifyPropertyChanged(RunfileFilenameChangedEventArgs);
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
                NotifyPropertyChanged(DispatcherChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DispatcherChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.Dispatcher);
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
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
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
                if (_totalProgress == value) return;
                _totalProgress = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TotalProgressChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs TotalProgressChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TotalProgress);
        float _totalProgress;

        #endregion

        #region public TransmissionLossRunFile TransmissionLossRunFile { get; set; }

        public TransmissionLossRunFile TransmissionLossRunFile
        {
            get { return _runFile; }
            set
            {
                if (_runFile == value) return;
                _runFile = value;
                NotifyPropertyChanged(TransmissionLossRunFileChangedEventArgs);
                if (_runFile == null) return;
                LoadEnvironment();
                SetupRadialViewModels();
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRunFileChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TransmissionLossRunFile);
        TransmissionLossRunFile _runFile;
        NAEMOEnvironmentFile _environment;

        void LoadEnvironment()
        {
            var bathymetryPath = Path.Combine(TransmissionLossRunFile.ScenarioDataDirectory, TransmissionLossRunFile.RangeComplexName, "Bathymetry", TransmissionLossRunFile.BathymetryName + ".txt");
            var environmentPath = Path.Combine(TransmissionLossRunFile.ScenarioDataDirectory, TransmissionLossRunFile.RangeComplexName, "Environment", TransmissionLossRunFile.EnvironmentName + ".dat");
            Status = "Loading environment";
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
                    rangeCellCount = (int)Math.Round((transmissionLossJob.SoundSource.Radius / bellhopSettings.RangeCellSize)) + 1;
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
                var curTransect = new Transect(null, transmissionLossJob.SoundSource, radialBearing, transmissionLossJob.SoundSource.Radius);
                bottomProfiles[bearingIndex] = new BottomProfile(rangeCellCount, curTransect, _environment.EnvironmentInformation.Bathymetry);
                maxCalculationDepthMeters = Math.Max((float)bottomProfiles[bearingIndex].MaxDepth, maxCalculationDepthMeters);
                soundSpeedProfiles[bearingIndex] = _environment.EnvironmentInformation.SoundSpeedField.EnvironmentData[curTransect.MidPoint];
                windSpeeds[bearingIndex] = _environment.EnvironmentInformation.Wind.TimePeriods[0].EnvironmentData[curTransect.MidPoint].Data;
            }

            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters /depthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = _environment.EnvironmentInformation.Sediment.Samples[transmissionLossJob.SoundSource];
                TransmissionLossRadialCalculatorViewModel radialViewModel;
                switch (TransmissionLossRunFile.TransmissionLossAlgorithm)
                {
                    case TransmissionLossAlgorithm.Bellhop:
                        var bellhopConfig = Bellhop.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[bearingIndex], sedimentType, maxCalculationDepthMeters, rangeCellCount, depthCellCount, false, false, false, 1500);
                        TransmissionLossRunFile.TransmissionLossRunFileRadials.Add(new BellhopRunFileRadial {BearingFromSourceDegrees = radialBearing, Configuration = bellhopConfig, BottomProfile = bottomProfiles[bearingIndex].ToBellhopString()});
                        // todo: Once we know how to calculate the Top Reflection Coefficients for Bellhop, put them in here
                        // 
                        radialViewModel = new BellhopRadialCalculatorViewModel((BellhopRunFileRadial)_runFile.TransmissionLossRunFileRadials[bearingIndex], bearingIndex, _dispatcher);
                        radialViewModel.PropertyChanged += (s, e) =>
                                                           {
                                                               if (_dispatcher != null) ((BellhopRadialCalculatorViewModel)s).Dispatcher = _dispatcher;
                                                               if (e.PropertyName != "ProgressPercent") return;
                                                               //float radialCount = RadialCalculatorViewModels.Count;
                                                               var progress = RadialCalculatorViewModels.Sum(radial => radial.ProgressPercent/radialCount);
                                                               TotalProgress = progress;
                                                           };
                        RadialCalculatorViewModels.Add(radialViewModel);
                        break;
                    case TransmissionLossAlgorithm.RAMGEO:
                        var ramConfig = Ram.GetRadialConfiguration(transmissionLossJob, soundSpeedProfiles[bearingIndex], bottomProfiles[bearingIndex], sedimentType, maxCalculationDepthMeters, rangeCellCount, depthCellCount);
                        TransmissionLossRunFile.TransmissionLossRunFileRadials.Add(new RamRunFileRadial { BearingFromSourceDegrees = radialBearing, Configuration = ramConfig });
                        break;
                    default:
                        break;
                }
            }
        }

        public void Start(RunWorkerCompletedEventHandler runWorkerCompletedEventHandler)
        {
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

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(NameChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.Name);
        string _name;

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

        #region BackgroundWorker thread that computes the TransmissionLossField in parallel

        void Calculate(object sender, DoWorkEventArgs args)
        {
            //if (TransmissionLossField.Radials == null) throw new ApplicationException("Radials are null");

            var runFile = (TransmissionLossRunFile) args.Argument;
            var radialNum = 0;
            var radialProgress = 100f/runFile.TransmissionLossRunFileRadials.Count;
            TotalProgress = 0f;

            Parallel.ForEach<TransmissionLossRunFileRadial, float>(runFile.TransmissionLossRunFileRadials, () => 0, (radial, loopstate, progress) =>
            {
                var localRadialNum = Interlocked.Increment(ref radialNum);
                var radialViewModel = RadialCalculatorViewModels[localRadialNum - 1];
                radialViewModel.Start();
                return radialProgress;
            }, finalResult => TotalProgress += finalResult);
            foreach (var radial in RadialCalculatorViewModels) if (radial.TransmissionLossRadial != null) TransmissionLossField.AddRadial(radial.TransmissionLossRadial);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            if (_dispatcher != null) _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs));
        }

        #endregion
    }
}