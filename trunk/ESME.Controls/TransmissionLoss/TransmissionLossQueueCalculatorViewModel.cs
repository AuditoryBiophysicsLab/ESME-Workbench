using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Threading.Tasks;
using Cinch;
using ESME.TransmissionLoss.CASS;
using HRC.Utility;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossQueueCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossQueueCalculatorViewModel()
        {
            FieldCalculatorViewModels = new ObservableList<TransmissionLossFieldCalculatorViewModel>();
        }

        public uint PhysicalCores { get; set; }
        public uint LogicalCores { get; set; }
        public string CpuDescription { get; set; }
        #endregion

        #region public string WindowTitle { get; set; }

        public string WindowTitle
        {
            get
            {
                if (!string.IsNullOrEmpty(_windowTitle)) return _windowTitle;
                _windowTitle = GetCurrentWindowTitleString();
                NotifyPropertyChanged(WindowTitleChangedEventArgs);
                return _windowTitle;
            }
            set
            {
                if (_windowTitle == value) return;
                _windowTitle = value;
                NotifyPropertyChanged(WindowTitleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindowTitleChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.WindowTitle);
        string _windowTitle;
        const string BaseWindowName = "ESME Transmission Loss Calculator : ";
        string GetCurrentWindowTitleString()
        {
            var itemCount = FieldCalculatorViewModels.Count;
            switch (itemCount)
            {
                case 0:
                    return IsPaused ? BaseWindowName + "Paused (queue empty)" : BaseWindowName + "Idle";
                case 1:
                    return IsPaused ? BaseWindowName + "Paused (1 item queued)" : BaseWindowName + "1 item in queue";
                default:
                    return IsPaused ? BaseWindowName + "Paused (" + itemCount + " items queued)" : BaseWindowName + itemCount + " items in queue";
            }
        }

        #endregion

        #region public bool IsPaused { get; set; }

        public bool IsPaused
        {
            get { return _isPaused; }
            set
            {
                if (_isPaused == value) return;
                _isPaused = value;
                NotifyPropertyChanged(IsPausedChangedEventArgs);
                WindowTitle = GetCurrentWindowTitleString();
                if (!_isPaused) StartWorkIfNeeded();
            }
        }

        static readonly PropertyChangedEventArgs IsPausedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.IsPaused);
        bool _isPaused;

        #endregion

        public void CancelActiveCalculation()
        {
            if ((FieldCalculatorViewModels != null) && (FieldCalculatorViewModels.Count > 0)) FieldCalculatorViewModels[0].CancelRequested = true;
        }

        #region public ObservableList<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels { get; set; }

        public ObservableList<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels
        {
            get { return _fieldCalculatorViewModels; }
            set
            {
                if (_fieldCalculatorViewModels == value) return;
                _fieldCalculatorViewModels = value;
                NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.FieldCalculatorViewModels);
        ObservableList<TransmissionLossFieldCalculatorViewModel> _fieldCalculatorViewModels;

        void StartWorkIfNeeded()
        {
            WindowTitle = GetCurrentWindowTitleString();
            if (FieldCalculatorViewModels.Count > 0)
                Task.Factory.StartNew(() => FieldCalculatorViewModels[0].Start(delegate { HandleCompletedQueueItem(); }));
            if (FieldCalculatorViewModels.Count > 1) Task.Factory.StartNew(() => FieldCalculatorViewModels[1].PrepareRadials());
            //if (FieldCalculatorViewModels.Count > 2) Task.Factory.StartNew(() => FieldCalculatorViewModels[2].PrepareRadials());
        }

        void HandleCompletedQueueItem()
        {
            while ((FieldCalculatorViewModels.Count > 0) && (FieldCalculatorViewModels[0].IsCompleted))
            {
                if (FieldCalculatorViewModels[0].CancelRequested)
                {
                    FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]);
                    continue;
                }
                var runfileName = FieldCalculatorViewModels[0].RunfileName;
                var runfilePath = Path.GetDirectoryName(runfileName);
                //var outputFileName = Path.Combine(runfilePath, Path.GetFileNameWithoutExtension(runfileName) + ".bin");
                var runFile = FieldCalculatorViewModels[0].TransmissionLossRunFile;
                var outputFileName = Path.Combine(runfilePath, Path.GetFileNameWithoutExtension(runFile.Filename) + ".bin");
                var output = new CASSOutput
                {
                    RunDateTime = DateTime.Now.ToString(),
                    OperatingSystemName = System.Environment.OSVersion.VersionString,
                    SystemNodeName = System.Environment.MachineName,
                    OperatingSystemRelease = System.Environment.OSVersion.ServicePack,
                    OperatingSystemVersion = System.Environment.OSVersion.Platform.ToString(),
                    MachineType = string.Format("{0}/{1} phys/logical core(s)", PhysicalCores, LogicalCores),
                    ProcessorType = CpuDescription,
                    Title = Path.GetFileNameWithoutExtension(runfileName),
                    SiteName = FieldCalculatorViewModels[0].TransmissionLossRunFile.RangeComplexName,
                    SiteRefLatLocation = (float)runFile.ReferenceLocation.Latitude,
                    SiteRefLonLocation = (float)runFile.ReferenceLocation.Longitude,
                    SourceRefLatLocation = (float)runFile.TransmissionLossJob.SoundSource.Latitude,
                    SourceRefLonLocation = (float)runFile.TransmissionLossJob.SoundSource.Longitude,
                    PlatformName = runFile.TransmissionLossJob.PlatformName,
                    SourceName = runFile.TransmissionLossJob.SourceName,
                    ModeName = runFile.TransmissionLossJob.ModeName,
                    Frequency = runFile.TransmissionLossJob.SoundSource.AcousticProperties.Frequency,
                    DepressionElevationAngle = runFile.TransmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle,
                    VerticalBeamPattern = runFile.TransmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth,
                    SourceDepth = runFile.TransmissionLossJob.SoundSource.AcousticProperties.SourceDepth,
                    SourceLevel = runFile.TransmissionLossJob.SoundSource.SourceLevel,
                    MinWaterDepth = 0,  // Seems to be always zero
                    MaxWaterDepth = runFile.TransmissionLossJob.MaxDepth,
                    WaterDepthIncrement = runFile.WaterDepthIncrement,
                    MinRangeDistance = 0,
                    MaxRangeDistance = runFile.TransmissionLossJob.SoundSource.Radius,
                    RangeDistanceIncrement = runFile.RangeDistanceIncrement,
                    BottomType = runFile.TransmissionLossJob.BottomTypeName,
                    Season = runFile.TransmissionLossJob.TimePeriodName,
                    WindSpeed = runFile.TransmissionLossJob.WindSpeed,
                    CASSLevel = 1,
                    RadialCount = FieldCalculatorViewModels[0].RadialCalculatorViewModels.Count,
                    RadialBearings = runFile.TransmissionLossJob.SoundSource.RadialBearings.ToArray(),
                    RangeCellCount = FieldCalculatorViewModels[0].RadialCalculatorViewModels[0].TransmissionLossRadial.Ranges.Count,
                    RangeCells = FieldCalculatorViewModels[0].RadialCalculatorViewModels[0].TransmissionLossRadial.Ranges.ToArray(),
                    DepthCellCount = FieldCalculatorViewModels[0].RadialCalculatorViewModels[0].TransmissionLossRadial.Depths.Count,
                    DepthCells = FieldCalculatorViewModels[0].RadialCalculatorViewModels[0].TransmissionLossRadial.Depths.ToArray(),
                    Pressures = new List<float[,]>(),
                    Filename = outputFileName,
                };
                foreach (var radialViewModel in FieldCalculatorViewModels[0].RadialCalculatorViewModels)
                {
                    var radial = radialViewModel.TransmissionLossRadial;
                    var buffer = new float[radial.Depths.Count, radial.Ranges.Count];
                    for (var rangeIndex = 0; rangeIndex < radial.Ranges.Count; rangeIndex++)
                        for (var depthIndex = 0; depthIndex < radial.Depths.Count; depthIndex++)
                        {
                            var spl = runFile.TransmissionLossJob.SoundSource.SourceLevel -
                                                             radial[depthIndex, rangeIndex];
                            //if (spl > runFile.TransmissionLossJob.SoundSource.SourceLevel) Debugger.Break();
                            buffer[depthIndex, rangeIndex] = spl;
                        }
                    output.Pressures.Add(buffer);
                }
                output.ToBinaryFile();
                FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]);
            }
            if (!IsPaused) StartWorkIfNeeded();
        }

        #endregion
    }
}