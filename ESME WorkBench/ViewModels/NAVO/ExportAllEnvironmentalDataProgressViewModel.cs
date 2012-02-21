using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using HRC.Utility;
using RangeComplex = ESME.Environment.Descriptors.RangeComplex;

namespace ESMEWorkBench.ViewModels.NAVO
{
    public class ExportAllEnvironmentalDataProgressViewModel:ViewModelBase
    {
        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { CancelHandler(); })); }
        }

        SimpleCommand<object, object> _cancel;
        readonly CancellationTokenSource _cancellationTokenSource;

        void CancelHandler() { _cancellationTokenSource.Cancel(); }
        #endregion

        #region public ProgressInfoInt BathymetryProgress { get; set; }

        public ProgressInfoInt BathymetryProgress
        {
            get { return _bathymetryProgress; }
            set
            {
                if (_bathymetryProgress == value) return;
                _bathymetryProgress = value;
                NotifyPropertyChanged(BathymetryProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryProgressChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.BathymetryProgress);
        ProgressInfoInt _bathymetryProgress;

        #endregion

        #region public ProgressInfoInt EnvironmentProgress { get; set; }

        public ProgressInfoInt EnvironmentProgress
        {
            get { return _environmentProgress; }
            set
            {
                if (_environmentProgress == value) return;
                _environmentProgress = value;
                NotifyPropertyChanged(EnvironmentProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentProgressChangedEventArgs = ObservableHelper.CreateArgs<ExportAllEnvironmentalDataProgressViewModel>(x => x.EnvironmentProgress);
        ProgressInfoInt _environmentProgress;

        #endregion

        int _environmentFileCountMultiplier = 1;

        void ExportAll(RangeComplex rangeComplex, Dispatcher dispatcher, CancellationToken cancellationToken, IProgress<ProgressInfoInt> bathymetryProgress = null, IProgress<ProgressInfoInt> environmentProgress = null)
        {
            var bathymetryMax = 0;
            var bathymetryCount = 0;
            var environmentMax = 0;
            var environmentCount = 0;
            if (File.Exists(Path.Combine(rangeComplex.DataPath, "data.bottomloss"))) _environmentFileCountMultiplier = 4;

            var bottomLossTask = new Task<BottomLoss>(() => BottomLoss.Load(Path.Combine(rangeComplex.DataPath, "data.bottomloss")));
            var sedimentTask = new Task<Sediment>(() => Sediment.Load(Path.Combine(rangeComplex.DataPath, "data.sediment")));
            bottomLossTask.Start();
            sedimentTask.Start();
            var envBlock = new ActionBlock<Tuple<RangeComplexArea, EnvironmentFile, List<Geo>, List<SedimentSample>, List<BottomLossSample>, Task<Bathymetry>, TimePeriod>>(stuff =>
            {
                var area = stuff.Item1;
                var resolution = stuff.Item2;
                var locations = stuff.Item3;
                var sedimentList = stuff.Item4;
                var bottomLossList = stuff.Item5;
                var bathyTask = stuff.Item6;
                var period = stuff.Item7;

                var bathyFileName = Path.Combine(rangeComplex.BathymetryPath, string.Format("{0}_{1}_bathy.txt", area.Name, resolution.Name));

                var cassEnvironmentFileName = Path.Combine(rangeComplex.EnvironmentPath, string.Format("{0}_{1}_env_{2}", area.Name, resolution.Name, period));
                var windTask = new Task<Wind>(() => Wind.Load(Path.Combine(rangeComplex.DataPath, "data.wind")));

                var soundSpeedTask = new Task<SoundSpeed>(() => EnvironmentFile.CalculateSoundSpeed(rangeComplex, period, bathyTask, resolution.GeoRect));
                windTask.Start();
                soundSpeedTask.Start();

                TaskEx.WhenAll(windTask, soundSpeedTask).ContinueWith(task2 =>
                {
                    var soundSpeedProfiles = new List<SoundSpeedProfileGeneric<SoundSpeedSample>>();
                    var windSamples = new List<WindSample>();
                    foreach (var location in locations)
                    {
                        soundSpeedProfiles.Add(soundSpeedTask.Result[period].EnvironmentData.GetNearestPoint(location));
                        windSamples.Add((windTask.Result[period].EnvironmentData.GetNearestPoint(location)));
                    }
#if IS_CLASSIFIED_MODEL
                    CASSFiles.WriteEnvironmentFiles(period, locations, cassEnvironmentFileName, sedimentList, soundSpeedProfiles, windSamples, bathyFileName, area.Name + ".ovr", bottomLossList);
                    if (environmentProgress != null) environmentProgress.Report(new ProgressInfoInt
                    {
                        MaximumValue = environmentMax,
                        CurrentValue = Interlocked.Add(ref environmentCount, _environmentFileCountMultiplier),
                    });
#endif
                });

            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = -1, 
                BoundedCapacity = -1,
                CancellationToken = cancellationToken,
            });
            var envBuffer = new BufferBlock<Tuple<RangeComplexArea, EnvironmentFile, List<Geo>, List<SedimentSample>, List<BottomLossSample>, Task<Bathymetry>, TimePeriod>>();
            envBuffer.LinkTo(envBlock);
            envBuffer.Completion.ContinueWith(task => envBlock.Complete());
            envBlock.Completion.ContinueWith(task =>
            {
                dispatcher.InvokeIfRequired(() =>
                {
                    if (task.IsCanceled) EnvironmentProgress.Status += " (canceled)";
                    if (task.IsFaulted) EnvironmentProgress.Status += " (failed)";
                    if (task.IsCompleted) EnvironmentProgress.Status += " (completed)";
                });
                Thread.Sleep(5000);
                CloseActivePopUpCommand.Execute(true);
            });

            var bathyBlock = new ActionBlock<Tuple<RangeComplexArea, EnvironmentFile, List<Geo>, List<SedimentSample>, List<BottomLossSample>>>(stuff =>
            {
                var area = stuff.Item1;
                var resolution = stuff.Item2;
                var locations = stuff.Item3;
                var sedimentList = stuff.Item4;
                var bottomLossList = stuff.Item5;

                var bathyTask = new Task<Bathymetry>(() => Bathymetry.Load(Path.Combine(area.BathymetryPath, resolution.FileName)));
                bathyTask.Start();
                var bathyFileName = Path.Combine(rangeComplex.BathymetryPath, string.Format("{0}_{1}_bathy.txt", area.Name, resolution.Name));
                if (!File.Exists(bathyFileName))
                {
                    bathyTask.Result.ToYXZ(bathyFileName, -1);
                }
                if (bathymetryProgress != null) bathymetryProgress.Report(new ProgressInfoInt
                {
                    MaximumValue = bathymetryMax,
                    CurrentValue = Interlocked.Increment(ref bathymetryCount),
                });
                foreach (var timePeriod in NAVOConfiguration.AllTimePeriods)
                {
                    Interlocked.Add(ref environmentMax, _environmentFileCountMultiplier);
                    envBuffer.Post(Tuple.Create(area, resolution, locations, sedimentList, bottomLossList, bathyTask, timePeriod));
                }
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = -1,
                BoundedCapacity = -1,
                CancellationToken = cancellationToken,
            });
            var bathyBuffer = new BufferBlock<Tuple<RangeComplexArea, EnvironmentFile, List<Geo>, List<SedimentSample>, List<BottomLossSample>>>();
            bathyBuffer.LinkTo(bathyBlock);
            bathyBuffer.Completion.ContinueWith(task => bathyBlock.Complete());
            bathyBlock.Completion.ContinueWith(task =>
            {
                dispatcher.InvokeIfRequired(() =>
                {
                    if (task.IsCanceled) BathymetryProgress.Status += " (canceled)";
                    if (task.IsFaulted) BathymetryProgress.Status += " (failed)";
                    if (task.IsCompleted) BathymetryProgress.Status += " (completed)";
                });
                envBuffer.Complete();
            });

            var areaBlock = new ActionBlock<RangeComplexArea>(area =>
            {
                var requestedLocations = new List<Geo>();

                double lat, lon;
                for (lon = area.GeoRect.West; lon < area.GeoRect.East; lon += 0.25)
                {
                    for (lat = area.GeoRect.South; lat < area.GeoRect.North; lat += 0.25) requestedLocations.Add(new Geo(lat, lon));
                    if ((lat - area.GeoRect.North) < 0.125) requestedLocations.Add(new Geo(lat, lon));
                }
                if ((lon - area.GeoRect.East) < 0.125)
                    for (lat = area.GeoRect.South; lat < area.GeoRect.North; lat += 0.25)
                        requestedLocations.Add(new Geo(lat, lon));

                var sedimentPoints = new List<SedimentSample>();
                var bottomLossPoints = new List<BottomLossSample>();

                foreach (var location in requestedLocations)
                {
                    sedimentPoints.Add(sedimentTask.Result.Samples.GetNearestPoint(location));
                    BottomLossSample sample;
                    if (bottomLossTask.Result != null && bottomLossTask.Result.Samples != null && bottomLossTask.Result.Samples.Count > 0)
                        if (bottomLossTask.Result.Samples.TryGetExactPoint(location, out sample)) bottomLossPoints.Add(sample);
                }

                foreach (var resolution in area.BathymetryList)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    if (!resolution.IsCached) continue;
                    Interlocked.Increment(ref bathymetryMax);
                    bathyBuffer.Post(Tuple.Create(area, resolution, requestedLocations, sedimentPoints, bottomLossPoints));
                }
            }, new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 1,
                BoundedCapacity = -1,
                CancellationToken = cancellationToken,
            });
            var areaBuffer = new BufferBlock<RangeComplexArea>();
            areaBuffer.LinkTo(areaBlock);
            areaBuffer.Completion.ContinueWith(task => areaBlock.Complete());
            areaBlock.Completion.ContinueWith(task => bathyBuffer.Complete());

            foreach (var area in rangeComplex.AreaList)
            {
                cancellationToken.ThrowIfCancellationRequested();
                areaBuffer.Post(area);
            }
            areaBuffer.Complete();
        }

        public ExportAllEnvironmentalDataProgressViewModel(RangeComplex rangeComplex, Dispatcher dispatcher) 
        {
            BathymetryProgress = new ProgressInfoInt
            {
                CurrentValue = 0,
                MaximumValue = 100,
                Status = "Starting..."
            };
            EnvironmentProgress = new ProgressInfoInt
            {
                CurrentValue = 0,
                MaximumValue = 100,
                Status = "Starting..."
            };

            _cancellationTokenSource = new CancellationTokenSource();
            var bathymetryProgress = new Progress<ProgressInfoInt>();
            bathymetryProgress.ProgressChanged += (s, e) => dispatcher.InvokeIfRequired(() =>
            {
                BathymetryProgress.CurrentValue = e.CurrentValue;
                BathymetryProgress.MaximumValue = e.MaximumValue;
                BathymetryProgress.Status = string.Format("{0}/{1}", e.CurrentValue, e.MaximumValue);
            });
            var environmentProgress = new Progress<ProgressInfoInt>();
            environmentProgress.ProgressChanged += (s, e) => dispatcher.InvokeIfRequired(() =>
            {
                EnvironmentProgress.CurrentValue = e.CurrentValue;
                EnvironmentProgress.MaximumValue = e.MaximumValue;
                EnvironmentProgress.Status = string.Format("{0}/{1}", e.CurrentValue, e.MaximumValue);
            });
            TaskEx.Run(() => ExportAll(rangeComplex, dispatcher, _cancellationTokenSource.Token, bathymetryProgress, environmentProgress));
        }
    }
}
