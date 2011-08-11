using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Threading.Tasks;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss.CASS;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossQueueCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossQueueCalculatorViewModel()
        {
            FieldCalculatorViewModels = new ObservableCollection<TransmissionLossFieldCalculatorViewModel>();
            Dispatcher = Dispatcher.CurrentDispatcher;
        }

        #endregion

        #region public Dispatcher Dispatcher { get; set; }

        public Dispatcher Dispatcher
        {
            get { return _dispatcher; }
            set
            {
                if (_dispatcher == value) return;
                _dispatcher = value;
                foreach (var field in FieldCalculatorViewModels) field.Dispatcher = _dispatcher;
            }
        }

        Dispatcher _dispatcher;

        #endregion

        #region public string WindowTitle { get; set; }

        public string WindowTitle
        {
            get { return _windowTitle; }
            set
            {
                if (_windowTitle == value) return;
                _windowTitle = value;
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(WindowTitleChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs WindowTitleChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.WindowTitle);
        string _windowTitle;
        const string BaseWindowName = "ESME Transmission Loss Calculator : Work queue : ";

        #endregion

        #region public bool CancelRequested { get; set; }

        public bool CancelRequested
        {
            get { return _cancelRequested; }
            set
            {
                if (_cancelRequested == value) return;
                _cancelRequested = value;
                foreach (var field in FieldCalculatorViewModels) field.CancelRequested = _cancelRequested;
                NotifyPropertyChanged(CancelRequestedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CancelRequestedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.CancelRequested);
        bool _cancelRequested;

        #endregion

        #region public ObservableCollection<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels { get; set; }

        public ObservableCollection<TransmissionLossFieldCalculatorViewModel> FieldCalculatorViewModels
        {
            get { return _fieldCalculatorViewModels; }
            set
            {
                if (_fieldCalculatorViewModels == value) return;
                if (_fieldCalculatorViewModels != null) _fieldCalculatorViewModels.CollectionChanged -= FieldCalculatorViewModelsCollectionChanged;
                _fieldCalculatorViewModels = value;
                if (_fieldCalculatorViewModels != null) _fieldCalculatorViewModels.CollectionChanged += FieldCalculatorViewModelsCollectionChanged;
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs FieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.FieldCalculatorViewModels);
        ObservableCollection<TransmissionLossFieldCalculatorViewModel> _fieldCalculatorViewModels;

        void FieldCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    //if (e.NewItems != null)
                    //    foreach (var newItem in e.NewItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    //if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            StartWorkIfNeeded();
            var itemCount = FieldCalculatorViewModels.Count;
            switch (itemCount)
            {
                case 0:
                    WindowTitle = BaseWindowName + "EMPTY";
                    break;
                case 1:
                    WindowTitle = BaseWindowName + "1 item";
                    break;
                default:
                    WindowTitle = BaseWindowName + itemCount + " items";
                    break;
            }

            if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs));
        }

        void StartWorkIfNeeded()
        {
            if (FieldCalculatorViewModels.Count > 0)
                Task.Factory.StartNew(() => FieldCalculatorViewModels[0].Start(delegate { HandleCompletedQueueItem(); }));
            if (FieldCalculatorViewModels.Count > 1) Task.Factory.StartNew(() => FieldCalculatorViewModels[1].PrepareRadials());
            //if (FieldCalculatorViewModels.Count > 2) Task.Factory.StartNew(() => FieldCalculatorViewModels[2].PrepareRadials());
        }

        void HandleCompletedQueueItem()
        {
            while ((FieldCalculatorViewModels.Count > 0) && (FieldCalculatorViewModels[0].IsCompleted))
            {
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
                    MachineType = System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"),
                    ProcessorType = System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE"),
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
                            buffer[depthIndex, rangeIndex] = runFile.TransmissionLossJob.SoundSource.SourceLevel - radial[depthIndex, rangeIndex];
                    output.Pressures.Add(buffer);
                }
                output.Write();
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]));
                else FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]);
            }
            StartWorkIfNeeded();
        }

        #endregion
    }
}