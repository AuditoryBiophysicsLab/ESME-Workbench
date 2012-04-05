using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Threading;
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
            Task.Factory.StartNew(ProcessWorkQueue);
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
                //if (!_isPaused) StartWorkIfNeeded();
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
                //if (_fieldCalculatorViewModels != null) _fieldCalculatorViewModels.CollectionChanged += (s, e) => StartWorkIfNeeded();
            }
        }

        static readonly PropertyChangedEventArgs FieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.FieldCalculatorViewModels);
        ObservableList<TransmissionLossFieldCalculatorViewModel> _fieldCalculatorViewModels;

        void ProcessWorkQueue()
        {
            Task calculationTask = null;
            Task preparationTask = null;
            while (true)
            {
                while (IsPaused) Thread.Sleep(200);
                if ((FieldCalculatorViewModels.Count > 0) && (calculationTask == null)) calculationTask = Task.Factory.StartNew(Calculate, FieldCalculatorViewModels[0]);
                if ((FieldCalculatorViewModels.Count > 1) && (preparationTask == null) && (FieldCalculatorViewModels[1].Status != "Ready")) preparationTask = Task.Factory.StartNew(Prepare, FieldCalculatorViewModels[1]);
                try
                {
                    if (calculationTask != null)
                    {
                        calculationTask.Wait(2000);
                        if (calculationTask.IsCompleted) calculationTask = null;
                    }
                }
                catch (AggregateException ae)
                {
                    foreach (var e in ae.InnerExceptions) Debug.WriteLine("{0}: Calculation task threw exception: {1}", DateTime.Now, e.Message);
                    if (calculationTask != null)
                    {
                        Debug.WriteLine("{0}: Removing calculation item {1}", DateTime.Now, Path.GetFileNameWithoutExtension(((TransmissionLossFieldCalculatorViewModel)calculationTask.AsyncState).RunfileName));
                        FieldCalculatorViewModels.Remove((TransmissionLossFieldCalculatorViewModel)calculationTask.AsyncState);
                        calculationTask = null;
                    }
                }
                try
                {
                    if (preparationTask != null)
                    {
                        preparationTask.Wait(2000);
                        if (preparationTask.IsCompleted) preparationTask = null;
                    }
                }
                catch (AggregateException ae)
                {
                    foreach (var e in ae.InnerExceptions) Debug.WriteLine("{0}: Preparation task threw exception: {1}", DateTime.Now, e.Message);
                    if (preparationTask != null)
                    {
                        Debug.WriteLine("{0}: Removing preparation item {1}", DateTime.Now, Path.GetFileNameWithoutExtension(((TransmissionLossFieldCalculatorViewModel)preparationTask.AsyncState).RunfileName));
                        FieldCalculatorViewModels.Remove((TransmissionLossFieldCalculatorViewModel)preparationTask.AsyncState);
                        preparationTask = null;
                    }
                }
                if (calculationTask == null && preparationTask == null) Thread.Sleep(500);
            }
        }

        void Calculate(object state)
        {
            var field = (TransmissionLossFieldCalculatorViewModel)state;
            Debug.WriteLine("{0}: Starting {1}", DateTime.Now, Path.GetFileNameWithoutExtension(field.RunfileName));
            if (field.Calculate()) CalculationCompleteHandler(field);
            else FieldCalculatorViewModels.Remove(field);
        }

        static void Prepare(object state)
        {
            var field = (TransmissionLossFieldCalculatorViewModel)state;
            Debug.WriteLine("{0}: Preparing {1}", DateTime.Now, Path.GetFileNameWithoutExtension(field.RunfileName));
            field.PrepareRadials();
        }

        void CalculationCompleteHandler(TransmissionLossFieldCalculatorViewModel completedCalculation)
        {
            var runfileName = completedCalculation.RunfileName;
            var runfilePath = Path.GetDirectoryName(runfileName);
            var runFile = completedCalculation.TransmissionLossRunFile;
            var outputFileName = Path.Combine(runfilePath, Path.GetFileNameWithoutExtension(runFile.Filename) + ".bin");
            var output = new CASSOutput
            {
                RunDateTime = DateTime.Now.ToString(),
                OperatingSystemName = System.Environment.OSVersion.VersionString ?? "",
                SystemNodeName = System.Environment.MachineName,
                OperatingSystemRelease = System.Environment.OSVersion.ServicePack,
                OperatingSystemVersion = System.Environment.OSVersion.Platform.ToString(),
                MachineType = string.Format("{0}/{1} phys/logical core(s)", PhysicalCores, LogicalCores),
                ProcessorType = CpuDescription ?? "",
                Title = Path.GetFileNameWithoutExtension(runfileName) ?? "",
                SiteName = completedCalculation.TransmissionLossRunFile.RangeComplexName ?? "",
                SiteRefLatLocation = (float)runFile.ReferenceLocation.Latitude,
                SiteRefLonLocation = (float)runFile.ReferenceLocation.Longitude,
                SourceRefLatLocation = (float)runFile.TransmissionLossJob.SoundSource.Geo.Latitude,
                SourceRefLonLocation = (float)runFile.TransmissionLossJob.SoundSource.Geo.Longitude,
                PlatformName = runFile.TransmissionLossJob.PlatformName ?? "",
                SourceName = runFile.TransmissionLossJob.SourceName ?? "",
                ModeName = runFile.TransmissionLossJob.ModeName ?? "",
                Frequency = runFile.TransmissionLossJob.SoundSource.AcousticProperties.Frequency,
                DepressionElevationAngle = runFile.TransmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle,
                VerticalBeamPattern = runFile.TransmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth,
                SourceDepth = runFile.TransmissionLossJob.SoundSource.AcousticProperties.SourceDepth,
                SourceLevel = runFile.TransmissionLossJob.SoundSource.SourceLevel,
                MinWaterDepth = 0, // Seems to be always zero
                MaxWaterDepth = runFile.TransmissionLossJob.MaxDepth,
                WaterDepthIncrement = runFile.WaterDepthIncrement,
                MinRangeDistance = 0,
                MaxRangeDistance = runFile.TransmissionLossJob.SoundSource.Radius,
                RangeDistanceIncrement = runFile.RangeDistanceIncrement,
                BottomType = runFile.TransmissionLossJob.BottomTypeName ?? "",
                Season = runFile.TransmissionLossJob.TimePeriodName ?? "",
                WindSpeed = runFile.TransmissionLossJob.WindSpeed,
                CASSLevel = 1,
                RadialCount = completedCalculation.RadialCalculatorViewModels.Count,
                RadialBearings = runFile.TransmissionLossJob.SoundSource.RadialBearings.ToArray(),
                RangeCellCount = completedCalculation.RadialCalculatorViewModels[0].TransmissionLossRadial.Ranges.Count,
                RangeCells = completedCalculation.RadialCalculatorViewModels[0].TransmissionLossRadial.Ranges.ToArray(),
                DepthCellCount = completedCalculation.RadialCalculatorViewModels[0].TransmissionLossRadial.Depths.Count,
                DepthCells = completedCalculation.RadialCalculatorViewModels[0].TransmissionLossRadial.Depths.ToArray(),
                Pressures = new List<float[,]>(),
                Filename = outputFileName,
            };
            foreach (var radialViewModel in completedCalculation.RadialCalculatorViewModels)
            {
                var radial = radialViewModel.TransmissionLossRadial;
                var buffer = new float[radial.Depths.Count,radial.Ranges.Count];
                for (var rangeIndex = 0; rangeIndex < radial.Ranges.Count; rangeIndex++)
                    for (var depthIndex = 0; depthIndex < radial.Depths.Count; depthIndex++)
                    {
                        var spl = runFile.TransmissionLossJob.SoundSource.SourceLevel -
                                  radial[depthIndex, rangeIndex];
                        buffer[depthIndex, rangeIndex] = spl;
                    }
                output.Pressures.Add(buffer);
            }
            output.ToBinaryFile();
            FieldCalculatorViewModels.Remove(completedCalculation);
        }

        #endregion
    }
}