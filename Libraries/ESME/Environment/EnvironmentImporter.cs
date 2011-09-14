using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Timers;
using Cinch;
using ESME.Environment.Descriptors;
using HRC.Navigation;
using ESME.Environment.NAVO;

namespace ESME.Environment
{
    public static class NAVOImporter
    {
        static readonly ActionBlock<ImportJobDescriptor> TemperatureWorker;
        static readonly ActionBlock<ImportJobDescriptor> SalinityWorker;
        static readonly ActionBlock<ImportJobDescriptor> SedimentWorker;
        static readonly ActionBlock<ImportJobDescriptor> WindWorker;
        static readonly ActionBlock<ImportJobDescriptor> BathymetryWorker;
        static readonly ActionBlock<ImportJobDescriptor> BottomLossWorker;
        static readonly Timer Timer;
        public static readonly ImportProgressViewModel TemperatureProgress;
        public static readonly ImportProgressViewModel SalinityProgress;
        public static readonly ImportProgressViewModel SedimentProgress;
        public static readonly ImportProgressViewModel WindProgress;
        public static readonly ImportProgressViewModel BathymetryProgress;
        public static readonly ImportProgressViewModel BottomLossProgress;
        static NAVOImporter()
        {
            if (TemperatureWorker == null) TemperatureWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                TemperatureProgress.CurrentJob = string.Format("{0} {1}", Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.TimePeriod);
                Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                var temperature = new SoundSpeed();
                temperature.SoundSpeedFields.Add(temperatureField);
                temperature.Save(job.DestinationFilename);
                job.Resolution = 15;
                job.SampleCount = (uint)temperatureField.EnvironmentData.Count;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType,
                                job.TimePeriod);
                TemperatureProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 4,
            });
            TemperatureProgress = new ImportProgressViewModel("Temperature", TemperatureWorker);

            if (SalinityWorker == null) SalinityWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                SalinityProgress.CurrentJob = string.Format("{0} {1}", Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.TimePeriod);
                Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(job.TimePeriod), "salinity", job.TimePeriod, job.GeoRect);
                var salinity = new SoundSpeed();
                salinity.SoundSpeedFields.Add(salinityField);
                salinity.Save(job.DestinationFilename);
                job.Resolution = 15;
                job.SampleCount = (uint)salinityField.EnvironmentData.Count;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType,
                                job.TimePeriod);
                SalinityProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 4,
            });
            SalinityProgress = new ImportProgressViewModel("Salinity", SalinityWorker);

            if (SedimentWorker == null) SedimentWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                SedimentProgress.CurrentJob = string.Format("{0}", Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))));
                Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                var sediment = BST.Extract(job.GeoRect);
                sediment.Save(job.DestinationFilename);
                job.SampleCount = (uint)sediment.Samples.Count;
                job.Resolution = 5;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                SedimentProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 1,
            });
            SedimentProgress = new ImportProgressViewModel("Sediment", SedimentWorker);

            if (WindWorker == null) WindWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                WindProgress.CurrentJob = string.Format("{0}", Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))));
                Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                var wind = await SMGC.NewImportAsync(job.GeoRect);
                wind.Save(job.DestinationFilename);
                job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                job.Resolution = 60;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                WindProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 4,
            });
            WindProgress = new ImportProgressViewModel("Wind", WindWorker);

            if (BathymetryWorker == null) BathymetryWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                BathymetryProgress.CurrentJob = string.Format("{0} {1}", Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)), Path.GetFileNameWithoutExtension(job.DestinationFilename));
                Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)),
                                Path.GetFileNameWithoutExtension(job.DestinationFilename), job.DataType);
                var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                bathymetry.Save(job.DestinationFilename);
                job.SampleCount = (uint)bathymetry.Samples.Count;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)),
                                Path.GetFileNameWithoutExtension(job.DestinationFilename), job.DataType);
                BathymetryProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 4,
            });
            BathymetryProgress = new ImportProgressViewModel("Bathymetry", BathymetryWorker);

            if (BottomLossWorker == null) BottomLossWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                BottomLossProgress.CurrentJob = string.Format("{0}", Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))));
                Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                var bottomLoss = await BottomLossDatabase.ExtractAsync(job.GeoRect);
                bottomLoss.Save(job.DestinationFilename);
                job.SampleCount = (uint)bottomLoss.Samples.Count;
                job.Resolution = 15;
                job.CompletionAction(job);
                Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                BottomLossProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                MaxDegreeOfParallelism = 4,
            });
            BottomLossProgress = new ImportProgressViewModel("Bottom Loss", BottomLossWorker);

            if (Timer == null)
            {
                Timer = new Timer(15000) {AutoReset = true};
                Timer.Elapsed += (s, e) =>
                {
                    var total = 0;
                    total += WorkerReport(TemperatureWorker, "temperature");
                    total += WorkerReport(SalinityWorker, "salinity");
                    total += WorkerReport(SedimentWorker, "sediment");
                    total += WorkerReport(WindWorker, "wind");
                    total += WorkerReport(BathymetryWorker, "bathymetry");
                    total += WorkerReport(BottomLossWorker, "bottom loss");
                    if (total > 0) Debug.WriteLine("{0} {1} total jobs queued", DateTime.Now, total);
                    else
                    {
                        Debug.WriteLine("{0} Work complete!", DateTime.Now, total);
                        ((Timer)s).Stop();
                    }
                };
            }
        }

        static int WorkerReport(ActionBlock<ImportJobDescriptor> worker, string workerName)
        {
            if (worker.InputCount > 0) Debug.WriteLine("{0} {1} {2} jobs still in queue", DateTime.Now, worker.InputCount, workerName);
            else Debug.WriteLine("{0} {1} queue is EMPTY", DateTime.Now, workerName);
            return worker.InputCount;
        }

        public static void Import(IEnumerable<ImportJobDescriptor> jobDescriptors)
        {
            Timer.Start();
            foreach (var jobDescriptor in jobDescriptors)
            {
                switch (jobDescriptor.DataType)
                {
                    case EnvironmentDataType.Bathymetry:
                        BathymetryProgress.Post(jobDescriptor);
                        break;
                    case EnvironmentDataType.BottomLoss:
                        BottomLossProgress.Post(jobDescriptor);
                        break;
                    case EnvironmentDataType.Salinity:
                        SalinityProgress.Post(jobDescriptor);
                        break;
                    case EnvironmentDataType.Sediment:
                        SedimentProgress.Post(jobDescriptor);
                        break;
                    case EnvironmentDataType.Temperature:
                        TemperatureProgress.Post(jobDescriptor);
                        break;
                    case EnvironmentDataType.Wind:
                        WindProgress.Post(jobDescriptor);
                        break;
                }
            }
        }
    }

    public class ImportJobDescriptor
    {
        public EnvironmentDataType DataType { get; set; }
        public GeoRect GeoRect { get; set; }
        public NAVOTimePeriod TimePeriod { get; set; }
        public float Resolution { get; set; }
        public string DestinationFilename { get; set; }
        public uint SampleCount { get; set; }
        public Action<ImportJobDescriptor> CompletionAction { get; set; }
    }

    public class ImportProgressViewModel : ViewModelBase
    {
        public ImportProgressViewModel(string name, ActionBlock<ImportJobDescriptor> importer) 
        {
            Name = name;
            _importer = importer;
        }

        public void Post(ImportJobDescriptor job)
        {
            lock (_lockObject)
            {
                JobsSubmitted++;
                _importer.Post(job);
                IsWorkInProgress = true;
            }
        }

        public void JobCompleted(ImportJobDescriptor job)
        {
            lock (_lockObject)
            {
                JobsCompleted++;
                if (JobsCompleted != JobsSubmitted) return;
                IsWorkInProgress = false;
                JobsSubmitted = 0;
                JobsCompleted = 0;
            }
        }

        #region public string Name { get; set; }

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Name);
        string _name;

        #endregion

        #region public bool IsWorkInProgress { get; set; }

        public bool IsWorkInProgress
        {
            get { return _isWorkInProgress; }
            set
            {
                if (_isWorkInProgress == value) return;
                _isWorkInProgress = value;
                NotifyPropertyChanged(IsWorkInProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsWorkInProgressChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.IsWorkInProgress);
        bool _isWorkInProgress;

        #endregion

        #region public int JobsSubmitted { get; set; }

        public int JobsSubmitted
        {
            get { return _jobsSubmitted; }
            set
            {
                if (_jobsSubmitted == value) return;
                _jobsSubmitted = value;
                NotifyPropertyChanged(JobsSubmittedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JobsSubmittedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.JobsSubmitted);
        int _jobsSubmitted;

        #endregion

        #region public int JobsCompleted { get; set; }

        public int JobsCompleted
        {
            get { return _jobsCompleted; }
            set
            {
                if (_jobsCompleted == value) return;
                _jobsCompleted = value;
                NotifyPropertyChanged(JobsCompletedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JobsCompletedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.JobsCompleted);
        int _jobsCompleted;

        #endregion

        #region public string CurrentJob { get; set; }

        public string CurrentJob
        {
            get { return _currentJob; }
            set
            {
                if (_currentJob == value) return;
                _currentJob = value;
                NotifyPropertyChanged(CurrentJobChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CurrentJobChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.CurrentJob);
        string _currentJob;

        #endregion

        readonly ActionBlock<ImportJobDescriptor> _importer;
        readonly object _lockObject = new object();
    }
}
