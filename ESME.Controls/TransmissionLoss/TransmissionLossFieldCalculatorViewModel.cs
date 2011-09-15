using System;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using Cinch;
using ESME.Data;
using ESME.Environment;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.CASS;
using ESME.TransmissionLoss.RAM;
using HRC.Utility;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossFieldCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossFieldCalculatorViewModel(string runfileName)
        {
            Status = "Loading";
            RunfileName = runfileName;
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
                NotifyPropertyChanged(RunfileFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RunfileFilenameChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.RunfileName);
        string _runfileName;

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
                NotifyPropertyChanged(TotalProgressChangedEventArgs);
                CommandManager.InvalidateRequerySuggested();
            }
        }

        static readonly PropertyChangedEventArgs TotalProgressChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.TotalProgress);
        float _totalProgress;

        #endregion

        #region public float MaxTotalProgressValue { get; set; }

        public float MaxTotalProgressValue
        {
            get { return _maxTotalProgressValue; }
            set
            {
                if (_maxTotalProgressValue == value) return;
                _maxTotalProgressValue = value;
                NotifyPropertyChanged(MaxTotalProgressValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MaxTotalProgressValueChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.MaxTotalProgressValue);
        float _maxTotalProgressValue;

        #endregion

        #region public int RadialCount { get; set; }

        public int RadialCount
        {
            get { return _radialCount; }
            set
            {
                if (_radialCount == value) return;
                _radialCount = value;
                NotifyPropertyChanged(RadialCountChangedEventArgs);
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
                NotifyPropertyChanged(TransmissionLossRunFileChangedEventArgs);
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
            //maxCalculationDepthMeters = 2000;
            var depthCellCount = (int)Math.Round((maxCalculationDepthMeters / depthCellSize)) + 1;
            for (var bearingIndex = 0; bearingIndex < radialCount; bearingIndex++)
            {
                var radialBearing = transmissionLossJob.SoundSource.RadialBearings[bearingIndex];
                var sedimentType = _environment.EnvironmentInformation.Sediment.Samples[transmissionLossJob.SoundSource];
                TransmissionLossRadialCalculatorViewModel radialViewModel;
                double[,] topReflectionCoefficient = null;
                if (!float.IsNaN(windSpeeds[bearingIndex])) topReflectionCoefficient = Bellhop.GenerateReflectionCoefficients(windSpeeds[bearingIndex], transmissionLossJob.SoundSource.AcousticProperties.Frequency);
                switch (TransmissionLossRunFile.TransmissionLossAlgorithm)
                {
                    case TransmissionLossAlgorithm.BellhopNL:
                         var bellhopNLConfig = Bellhop.GetRadialConfiguration(transmissionLossJob,
                                                                           soundSpeedProfiles[bearingIndex],
                                                                           sedimentType, maxCalculationDepthMeters,
                                                                           rangeCellCount, depthCellCount, false,
                                                                           false, true, 1500);
                        TransmissionLossRunFile.TransmissionLossRunFileRadials.Add(new BellhopRunFileRadial
                        {
                                BearingFromSourceDegrees = radialBearing,
                                Configuration = bellhopNLConfig,
                                BottomProfile = bottomProfiles[bearingIndex].ToBellhopString(),
                                TopReflectionCoefficient = topReflectionCoefficient,
                        });
                        radialViewModel = new BellhopRadialCalculatorViewModel((BellhopRunFileRadial)_runFile.TransmissionLossRunFileRadials[bearingIndex], bearingIndex);
                        var NLLogDirectory = Path.Combine(Path.GetDirectoryName(_runFile.Filename), "Transmission Loss Calculator logs");
                        Directory.CreateDirectory(NLLogDirectory);
                        //radialViewModel.LogPath = Path.Combine(logDirectory, Path.GetFileNameWithoutExtension(_runFile.Filename) + string.Format("-bearing-{0:0.##}-", radialBearing));
                        radialViewModel.PropertyChanged += (s, e) =>
                        {
                            if (e.PropertyName != "ProgressPercent") return;
                            //float radialCount = RadialCalculatorViewModels.Count;
                            TotalProgress = RadialCalculatorViewModels.Sum(radial => radial.ProgressPercent);
                        };
                        radialViewModel.CalculationCompleted += (s, e) => RadialCount = RadialCalculatorViewModels.Count(radial => !radial.IsCompleted);
                        RadialCalculatorViewModels.Add(radialViewModel);
                        break;
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
                                BottomProfile = bottomProfiles[bearingIndex].ToBellhopString(),
                                TopReflectionCoefficient = topReflectionCoefficient,
                        });
                        radialViewModel = new BellhopRadialCalculatorViewModel((BellhopRunFileRadial)_runFile.TransmissionLossRunFileRadials[bearingIndex], bearingIndex);
                        var logDirectory = Path.Combine(Path.GetDirectoryName(_runFile.Filename), "Transmission Loss Calculator logs");
                        Directory.CreateDirectory(logDirectory);
                        //radialViewModel.LogPath = Path.Combine(logDirectory, Path.GetFileNameWithoutExtension(_runFile.Filename) + string.Format("-bearing-{0:0.##}-", radialBearing));
                        radialViewModel.PropertyChanged += (s, e) =>
                        {
                            if (e.PropertyName != "ProgressPercent") return;
                            //float radialCount = RadialCalculatorViewModels.Count;
                            TotalProgress = RadialCalculatorViewModels.Sum(radial => radial.ProgressPercent);
                        };
                        radialViewModel.CalculationCompleted += (s, e) => RadialCount = RadialCalculatorViewModels.Count(radial => !radial.IsCompleted);
                        RadialCalculatorViewModels.Add(radialViewModel);
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

        #endregion

        #region public bool IsStarted { get; set; }

        public bool IsStarted
        {
            get { return _isStarted; }
            set
            {
                if (_isStarted == value) return;
                _isStarted = value;
                NotifyPropertyChanged(IsStartedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.IsStarted);
        bool _isStarted;

        #endregion

        #region public ObservableList<TransmissionLossRadialCalculatorViewModel> RadialCalculatorViewModels { get; set; }

        public ObservableList<TransmissionLossRadialCalculatorViewModel> RadialCalculatorViewModels
        {
            get { return _radialCalculatorViewModels; }
            set
            {
                if (_radialCalculatorViewModels == value) return;
                _radialCalculatorViewModels = value;
                NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BellhopRadialCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldCalculatorViewModel>(x => x.RadialCalculatorViewModels);
        ObservableList<TransmissionLossRadialCalculatorViewModel> _radialCalculatorViewModels = new ObservableList<TransmissionLossRadialCalculatorViewModel>();

        #endregion

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
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
            get { return (!CancelRequested) && (TotalProgress < MaxTotalProgressValue); }
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
            var runFile = (TransmissionLossRunFile)args.Argument;
            var radialNum = 0;
            TotalProgress = 0f;
            MaxTotalProgressValue = 100 * runFile.TransmissionLossRunFileRadials.Count;

            Status = "Calculating";

            Parallel.ForEach<TransmissionLossRunFileRadial, float>(runFile.TransmissionLossRunFileRadials, () => 0, (radial, loopstate, progress) =>
            {
                var localRadialNum = Interlocked.Increment(ref radialNum);
                var radialViewModel = RadialCalculatorViewModels[localRadialNum - 1];
                radialViewModel.Start();
                return 100;
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
            NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
        }

        public bool Calculate()
        {
            PrepareRadials();
            var radialNum = 0;
            TotalProgress = 0f;
            MaxTotalProgressValue = 100 * _runFile.TransmissionLossRunFileRadials.Count;

            Status = "Calculating";

            Parallel.ForEach<TransmissionLossRunFileRadial, float>(_runFile.TransmissionLossRunFileRadials, () => 0, (radial, loopstate, progress) =>
            {
                var localRadialNum = Interlocked.Increment(ref radialNum);
                var radialViewModel = RadialCalculatorViewModels[localRadialNum - 1];
                radialViewModel.Start();
                return 100;
            }, finalResult => TotalProgress += finalResult);

            if (CancelRequested)
            {
                Status = "Canceling";
                return false;
            }

            Status = "Finishing";
            foreach (var radial in RadialCalculatorViewModels) if (radial.TransmissionLossRadial != null) TransmissionLossField.AddRadial(radial.TransmissionLossRadial);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
            return true;
        }

        #endregion
    }
}