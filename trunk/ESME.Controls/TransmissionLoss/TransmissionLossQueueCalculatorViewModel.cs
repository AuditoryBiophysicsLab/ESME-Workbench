﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Cinch;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.CASS;

namespace ESME.Views.TransmissionLoss
{
    public class TransmissionLossQueueCalculatorViewModel : ViewModelBase
    {
        #region public constructor

        public TransmissionLossQueueCalculatorViewModel()
        {
             FieldCalculatorViewModels = new ObservableCollection<TransmissionLossFieldCalculatorViewModel>();
        }

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
                NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FieldCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossQueueCalculatorViewModel>(x => x.FieldCalculatorViewModels);
        ObservableCollection<TransmissionLossFieldCalculatorViewModel> _fieldCalculatorViewModels;

        void FieldCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    if (e.NewItems != null)
                        foreach (var newItem in e.NewItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Move:
                    break;
                case NotifyCollectionChangedAction.Remove:
                    if (e.OldItems != null) foreach (var oldItem in e.OldItems.Cast<TransmissionLossFieldCalculatorViewModel>()) {}
                    break;
                case NotifyCollectionChangedAction.Replace:
                    break;
                case NotifyCollectionChangedAction.Reset:
                    break;
            }
            StartWorkIfNeeded();
            NotifyPropertyChanged(FieldCalculatorViewModelsChangedEventArgs);
        }

        void StartWorkIfNeeded()
        {
            if (FieldCalculatorViewModels.Count > 0)
                FieldCalculatorViewModels[0].Start(delegate { HandleCompletedQueueItem(); });
        }

        void HandleCompletedQueueItem()
        {
            while ((FieldCalculatorViewModels.Count > 0) && (FieldCalculatorViewModels[0].IsCompleted))
            {
                var runfileName = FieldCalculatorViewModels[0].RunfileName;
                var runfilePath = Path.GetDirectoryName(runfileName);
                var outputFileName = Path.Combine(runfilePath, Path.GetFileNameWithoutExtension(runfileName) + ".bin");
                var runFile = FieldCalculatorViewModels[0].TransmissionLossRunFile;
                var meanFrequency = Math.Sqrt(runFile.TransmissionLossJob.SoundSource.AcousticProperties.HighFrequency * runFile.TransmissionLossJob.SoundSource.AcousticProperties.LowFrequency);
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
                    SiteRefLatLocation = 0,     // todo
                    SiteRefLonLocation = 0,     // todo
                    SourceRefLatLocation = (float)runFile.TransmissionLossJob.SoundSource.Latitude,
                    SourceRefLonLocation = (float)runFile.TransmissionLossJob.SoundSource.Longitude,
                    PlatformName = runFile.TransmissionLossJob.PlatformName,
                    SourceName = runFile.TransmissionLossJob.SourceName,
                    ModeName = runFile.TransmissionLossJob.ModeName,
                    Frequency = (float)meanFrequency,
                    DepressionElevationAngle = runFile.TransmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle,
                    VerticalBeamPattern = runFile.TransmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth,
                    SourceDepth = runFile.TransmissionLossJob.SoundSource.AcousticProperties.SourceDepth,
                    SourceLevel = runFile.TransmissionLossJob.SoundSource.SourceLevel,
                    MinWaterDepth = 0,  // todo
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
                FieldCalculatorViewModels.Remove(FieldCalculatorViewModels[0]);
            }
            StartWorkIfNeeded();
        }

        #endregion
    }
}