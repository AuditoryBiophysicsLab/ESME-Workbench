using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows.Threading;
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
        public static readonly ImportProgressViewModel TemperatureProgress;
        public static readonly ImportProgressViewModel SalinityProgress;
        public static readonly ImportProgressViewModel SedimentProgress;
        public static readonly ImportProgressViewModel WindProgress;
        public static readonly ImportProgressViewModel BathymetryProgress;
        public static readonly ImportProgressViewModel BottomLossProgress;
        static NAVOImporter()
        {
            try
            {
                if (TemperatureWorker == null) TemperatureWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    TemperatureProgress.JobStarting(job);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                        var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(job.TimePeriod), "water_temp", job.TimePeriod, job.GeoRect);
                        var temperature = new SoundSpeed();
                        temperature.SoundSpeedFields.Add(temperatureField);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            temperature.Save(job.DestinationFilename);
                            job.Resolution = 15;
                            job.SampleCount = (uint)temperatureField.EnvironmentData.Count;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                    TemperatureProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 4,
                    MaxDegreeOfParallelism = 4,
                })
                ;
                TemperatureProgress = new ImportProgressViewModel("Temperature", TemperatureWorker);

                if (SalinityWorker == null) SalinityWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    SalinityProgress.JobStarting(job);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                        var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(job.TimePeriod), "salinity", job.TimePeriod, job.GeoRect);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            var salinity = new SoundSpeed();
                            salinity.SoundSpeedFields.Add(salinityField);
                            salinity.Save(job.DestinationFilename);
                            job.Resolution = 15;
                            job.SampleCount = (uint)salinityField.EnvironmentData.Count;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType, job.TimePeriod);
                    SalinityProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 4,
                    MaxDegreeOfParallelism = 4,
                })
                ;
                SalinityProgress = new ImportProgressViewModel("Salinity", SalinityWorker);

                if (SedimentWorker == null) SedimentWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    SedimentProgress.JobStarting(job);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                        var sediment = BST.Extract(job.GeoRect);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            sediment.Save(job.DestinationFilename);
                            job.SampleCount = (uint)sediment.Samples.Count;
                            job.Resolution = 5;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                    SedimentProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 1,
                    MaxDegreeOfParallelism = 1,
                })
                ;
                SedimentProgress = new ImportProgressViewModel("Sediment", SedimentWorker);

                if (WindWorker == null) WindWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    WindProgress.JobStarting(job);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                        var wind = await
                        SMGC.NewImportAsync(job.GeoRect);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            wind.Save(job.DestinationFilename);
                            job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                            job.Resolution = 60;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                    WindProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 4,
                    MaxDegreeOfParallelism = 4,
                })
                ;
                WindProgress = new ImportProgressViewModel("Wind", WindWorker);

                if (BathymetryWorker == null) BathymetryWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    try
                    {
                        BathymetryProgress.JobStarting(job);
                    }
                    catch (Exception e)
                    {
                        job.Exception = e;
                    }
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)), Path.GetFileNameWithoutExtension(job.DestinationFilename), job.DataType);
                        var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            bathymetry.Save(job.DestinationFilename);
                            job.SampleCount = (uint)bathymetry.Samples.Count;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)), Path.GetFileNameWithoutExtension(job.DestinationFilename), job.DataType);
                    BathymetryProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 4,
                    MaxDegreeOfParallelism = 4,
                })
                ;
                BathymetryProgress = new ImportProgressViewModel("Bathymetry", BathymetryWorker);

                if (BottomLossWorker == null) BottomLossWorker = new ActionBlock<ImportJobDescriptor>(async
                job =>
                {
                    BottomLossProgress.JobStarting(job);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        //Debug.WriteLine("{0} About to import {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                        var bottomLoss = await
                        BottomLossDatabase.ExtractAsync(job.GeoRect);
                        if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                        {
                            bottomLoss.Save(job.DestinationFilename);
                            job.SampleCount = (uint)bottomLoss.Samples.Count;
                            job.Resolution = 15;
                            job.CompletionAction(job);
                        }
                    }
                    //Debug.WriteLine("{0} Finished importing {1} {2}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(Path.GetDirectoryName(job.DestinationFilename))), job.DataType);
                    BottomLossProgress.JobCompleted(job);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    BoundedCapacity = 4,
                    MaxDegreeOfParallelism = 4,
                })
                ;
                BottomLossProgress = new ImportProgressViewModel("Bottom Loss", BottomLossWorker);
            }
            catch (Exception e)
            {
                System.Media.SystemSounds.Beep.Play();
                Debug.WriteLine("************* Static Constructor caught exception **************");
                Debugger.Break();
            }
            AwaitFailure();
        }

        public async static void AwaitFailure()
        {
            try
            {
                var workers = new Dictionary<string, IDataflowBlock>
                {
                    {"Temperature", TemperatureWorker},
                    {"Salinity", SalinityWorker},
                    {"Sediment", SedimentWorker},
                    {"Wind", WindWorker},
                    {"Bathymetry", BathymetryWorker},
                    {"Bottom Loss", BottomLossWorker},
                };
                while (workers.Count > 0)
                {
                    var runningTasks = (from worker in workers.Values
                                        where worker.Completion.IsCanceled == false &&
                                              worker.Completion.IsCompleted == false &&
                                              worker.Completion.IsFaulted == false
                                        select worker.Completion).ToList();
                    if (runningTasks.Count == 0)
                    {
                        Debug.WriteLine("{0} All workers have completed");
                        break;
                    }
                    await TaskEx.WhenAny(runningTasks);
                    Debug.WriteLine("{0} Worker completed", DateTime.Now);
                    workers.ToList().ForEach(worker => Debug.WriteLine("{0} {1,15}: completed: {2,-5}  canceled: {3,-5}  faulted: {4,-5}", DateTime.Now, worker.Key, worker.Value.Completion.IsCompleted, worker.Value.Completion.IsCanceled, worker.Value.Completion.IsFaulted));
                }
            }
            catch (Exception e)
            {
                System.Media.SystemSounds.Beep.Play();
                Debug.WriteLine("************* AwaitFailure caught exception **************");
                Debugger.Break();
            }
        }

        public static void Import(IEnumerable<ImportJobDescriptor> jobDescriptors)
        {
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
        public Exception Exception { get; set; }
    }

    public class ImportProgressCollection : ReadOnlyObservableCollection<ImportProgressViewModel>
    {
        static readonly ObservableCollection<ImportProgressViewModel> Importers = new ObservableCollection<ImportProgressViewModel>(
            new List<ImportProgressViewModel>
            {
                NAVOImporter.TemperatureProgress,
                NAVOImporter.SalinityProgress,
                NAVOImporter.BathymetryProgress,
                NAVOImporter.BottomLossProgress,
                NAVOImporter.SedimentProgress,
                NAVOImporter.WindProgress,
            });

        static readonly ImportProgressCollection Instance = new ImportProgressCollection();
        public static ImportProgressCollection Singleton { get { return Instance; } }
        ImportProgressCollection() : base(Importers) { }
    }

    public class ImportProgressViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher = Dispatcher.CurrentDispatcher;

        public ImportProgressViewModel(string name, ActionBlock<ImportJobDescriptor> importer = null) 
        {
            Name = name;
            _importer = importer;
            _buffer = new BufferBlock<ImportJobDescriptor>();
            _buffer.LinkTo(_importer);
            AwaitCompletion();
        }

        async void AwaitCompletion()
        {
            try
            {
                await _importer.Completion;
            }
            catch {}
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                IsCompleted = _importer.Completion.IsCompleted;
                IsCanceled = _importer.Completion.IsCanceled;
                IsFaulted = _importer.Completion.IsFaulted;
                if (IsFaulted)
                {
                    Status = "Error";
                    ToolTip = "";
                    var faultCount = 0;
                    foreach (var ex in _importer.Completion.Exception.InnerExceptions)
                    {
                        if (faultCount > 0) ToolTip += new string(' ', 2 * faultCount);
                        ToolTip += ex.Message + "\r\n";
                        faultCount++;
                    }
                    ToolTip = _importer.Completion.Exception.InnerExceptions[0].Message;
                }
            });
        }

        public void Post(ImportJobDescriptor job)
        {
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                Submitted++;
                UpdateStatus();
                _buffer.Post(job);
                IsWorkInProgress = true;
            });
        }

        public void JobStarting(ImportJobDescriptor job)
        {
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                Running++;
                UpdateStatus();
            });
        }

        public void JobCompleted(ImportJobDescriptor job)
        {
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                Running--;
                Completed++;
                UpdateStatus();
            });
            if (Completed != Submitted) return;
            IsWorkInProgress = false;
            Submitted = 0;
            Completed = 0;
        }

        void UpdateStatus()
        {
            if (!IsFaulted)
            {
                Status = string.Format("{0}/{1} complete, {2} running", Completed, Submitted, Running);
                ToolTip = Status;
            }
        }

        #region public string ToolTip { get; set; }

        public string ToolTip
        {
            get { return _toolTip; }
            set
            {
                if (_toolTip == value) return;
                _toolTip = value;
                NotifyPropertyChanged(ToolTipChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ToolTipChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.ToolTip);
        string _toolTip;

        #endregion

        #region public string Name { get; private set; }

        public string Name
        {
            get { return _name; }
            private set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Name);
        string _name;

        #endregion

        #region public bool IsWorkInProgress { get; private set; }

        public bool IsWorkInProgress
        {
            get { return _isWorkInProgress; }
            private set
            {
                if (_isWorkInProgress == value) return;
                _isWorkInProgress = value;
                NotifyPropertyChanged(IsWorkInProgressChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsWorkInProgressChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.IsWorkInProgress);
        bool _isWorkInProgress;

        #endregion

        #region public int Submitted { get; private set; }

        public int Submitted
        {
            get { return _submitted; }
            private set
            {
                if (_submitted == value) return;
                _submitted = value;
                NotifyPropertyChanged(JobsSubmittedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JobsSubmittedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Submitted);
        int _submitted;

        #endregion

        #region public int Completed { get; private set; }

        public int Completed
        {
            get { return _completed; }
            private set
            {
                if (_completed == value) return;
                _completed = value;
                NotifyPropertyChanged(JobsCompletedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JobsCompletedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Completed);
        int _completed;

        #endregion

        #region public int Running { get; private set; }

        public int Running
        {
            get { return _running; }
            private set
            {
                if (_running == value) return;
                _running = value;
                NotifyPropertyChanged(JobsInFlightChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs JobsInFlightChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Running);
        int _running;

        #endregion

        #region public string Status { get; private set; }

        public string Status
        {
            get { return _status; }
            private set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(CurrentJobChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CurrentJobChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.Status);
        string _status;

        #endregion

        #region public bool IsCompleted { get; set; }

        public bool IsCompleted
        {
            get { return _isCompleted; }
            set
            {
                if (_isCompleted == value) return;
                _isCompleted = value;
                NotifyPropertyChanged(IsCompletedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCompletedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.IsCompleted);
        bool _isCompleted;

        #endregion

        #region public bool IsCanceled { get; set; }

        public bool IsCanceled
        {
            get { return _isCanceled; }
            set
            {
                if (_isCanceled == value) return;
                _isCanceled = value;
                NotifyPropertyChanged(IsCanceledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCanceledChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.IsCanceled);
        bool _isCanceled;

        #endregion

        #region public bool IsFaulted { get; set; }

        public bool IsFaulted
        {
            get { return _isFaulted; }
            set
            {
                if (_isFaulted == value) return;
                _isFaulted = value;
                NotifyPropertyChanged(IsFaultedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsFaultedChangedEventArgs = ObservableHelper.CreateArgs<ImportProgressViewModel>(x => x.IsFaulted);
        bool _isFaulted;

        #endregion

        readonly ActionBlock<ImportJobDescriptor> _importer;
        readonly BufferBlock<ImportJobDescriptor> _buffer;
    }

    public static class SynchronizationContextExtensions
    {
        /// <summary>
        /// Synchronously invokes a delegate by passing it to a <see cref="SynchronizationContext">, waiting for it to complete.
        /// </see></summary>
        /// <param name="synchronizationContext"/>The <see cref="SynchronizationContext"> to pass the delegate to. May not be null.
        /// <param name="action"/>The delegate to invoke. May not be null.
        /// <remarks>
        /// <para>This method is guaranteed to not be reentrant.</para>
        /// </remarks>
        public static void SafeSend(this SynchronizationContext synchronizationContext, Action action)
        {
            // The semantics of SynchronizationContext.Send allow it to invoke the delegate directly, but we can't allow that.
            Action forwardDelegate = () => synchronizationContext.Send((state) => action(), null);
            var result = forwardDelegate.BeginInvoke(null, null);
            result.AsyncWaitHandle.WaitOne();
        }

        /// <summary>
        /// Asynchronously invokes a delegate by passing it to a <see cref="SynchronizationContext">, returning immediately.
        /// </see></summary>
        /// <param name="synchronizationContext"/>The <see cref="SynchronizationContext"> to pass the delegate to. May not be null.
        /// <param name="action"/>The delegate to invoke. May not be null.
        /// <remarks>
        /// <para>This method is guaranteed to not be reentrant.</para>
        /// </remarks>
        public static void SafePost(this SynchronizationContext synchronizationContext, Action action)
        {
            // The semantics of SynchronizationContext.Post allow it to invoke the delegate directly, but we can't allow that.
            ThreadPool.QueueUserWorkItem((state) => synchronizationContext.Post((state2) => action(), null));
        }
    }
}
