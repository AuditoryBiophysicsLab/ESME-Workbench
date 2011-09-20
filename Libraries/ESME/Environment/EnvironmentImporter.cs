﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.Descriptors;
using HRC.Navigation;
using ESME.Environment.NAVO;
using HRC.Utility;

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
        static readonly object BitmapLockObject = new object();
        static readonly Dispatcher Dispatcher = Dispatcher.CurrentDispatcher;

        static NAVOImporter()
        {
            if (TemperatureWorker == null) TemperatureWorker = new ActionBlock<ImportJobDescriptor>(async job =>
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
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await
                        job.CompletionTask;
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

            if (SalinityWorker == null) SalinityWorker = new ActionBlock<ImportJobDescriptor>(async job =>
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
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await
                        job.CompletionTask;
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

            if (SedimentWorker == null) SedimentWorker = new ActionBlock<ImportJobDescriptor>(async job =>
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
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await
                        job.CompletionTask;
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
                    var wind = await SMGC.NewImportAsync(job.GeoRect);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        wind.Save(job.DestinationFilename);
                        job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                        job.Resolution = 60;
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await
                        job.CompletionTask;
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
            });
            WindProgress = new ImportProgressViewModel("Wind", WindWorker);

            if (BathymetryWorker == null) BathymetryWorker = new ActionBlock<ImportJobDescriptor>(async job =>
            {
                BathymetryProgress.JobStarting(job);
                if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                {
                    //Debug.WriteLine("{0} About to import {1} {2} {3}", DateTime.Now, Path.GetFileName(Path.GetDirectoryName(job.DestinationFilename)), Path.GetFileNameWithoutExtension(job.DestinationFilename), job.DataType);
                    var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                    if (Directory.Exists(Path.GetDirectoryName(job.DestinationFilename)))
                    {
                        bathymetry.Save(job.DestinationFilename);
                        job.SampleCount = (uint)bathymetry.Samples.Count;
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await job.CompletionTask;
                    }
#if true
                    Dispatcher.InvokeAsynchronouslyInBackground(() =>
                    {
                        var colormap = new DualColormap(Colormap.Summer, Colormap.Jet) {Threshold = 0};
                        var bathysize = Math.Max(bathymetry.Samples.Longitudes.Length, bathymetry.Samples.Latitudes.Length);
                        var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                        var displayValues = bathymetry.Samples;
                        if (bathysize > screenSize)
                        {
                            var scaleFactor = screenSize / bathysize;
                            displayValues = EnvironmentData<EarthCoordinate<float>>.Decimate(bathymetry.Samples, (int)(bathymetry.Samples.Longitudes.Length * scaleFactor),
                                                                                             (int)(bathymetry.Samples.Latitudes.Length * scaleFactor));
                        }

                        var imageFilename = Path.GetFileNameWithoutExtension(job.DestinationFilename) + ".bmp";
                        var imagePath = Path.GetDirectoryName(job.DestinationFilename);
                        
                        lock (BitmapLockObject)
                        {
                            var retryCount = 10;
                            Exception ex = null;
                            while (--retryCount > 0)
                            {
                                try
                                {
                                    var bitmapData = new float[displayValues.Longitudes.Length, displayValues.Latitudes.Length];
                                    for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;
                                    var displayBitmap = colormap.ToBitmap(bitmapData, bathymetry.Minimum.Data, bathymetry.Maximum.Data < 0 ? bathymetry.Maximum.Data : 8000);
                                    var tmpBitmap = new Bitmap(displayBitmap.Width, displayBitmap.Height);
                                    var g = Graphics.FromImage(tmpBitmap);
                                    g.DrawImage(displayBitmap, 0, 0, displayBitmap.Width, displayBitmap.Height);
                                    g.Dispose();
                                    displayBitmap.Dispose();
                                    tmpBitmap.Save(Path.Combine(imagePath, imageFilename), ImageFormat.Bmp);
                                    ex = null;
                                    break;
                                }
                                catch (Exception e)
                                {
                                    ex = e;
                                    Debug.WriteLine("{0} Bitmap creation caught exception {1}", DateTime.Now, e.Message);
                                    Thread.Sleep(50);
                                }
                            }
                            if (ex != null) throw ex;
                        }

                        var sb = new StringBuilder();
                        sb.AppendLine(job.Resolution.ToString());
                        sb.AppendLine("0.0");
                        sb.AppendLine("0.0");
                        sb.AppendLine(job.Resolution.ToString());
                        sb.AppendLine(bathymetry.Samples.GeoRect.West.ToString());
                        sb.AppendLine(bathymetry.Samples.GeoRect.North.ToString());
                        using (var writer = new StreamWriter(Path.Combine(imagePath, Path.GetFileNameWithoutExtension(imageFilename) + ".bpw"), false)) writer.Write(sb.ToString());
                    });
#endif
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
                        //job.CompletionAction(job);
                        job.CompletionTask.Start();
                        await
                        job.CompletionTask;
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
                    workers.ToList().ForEach(
                                             worker =>
                                             Debug.WriteLine("{0} {1,15}: completed: {2,-5}  canceled: {3,-5}  faulted: {4,-5}", DateTime.Now, worker.Key, worker.Value.Completion.IsCompleted,
                                                             worker.Value.Completion.IsCanceled, worker.Value.Completion.IsFaulted));
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
                Import(jobDescriptor);
            }
        }

        public static void Import(ImportJobDescriptor jobDescriptor)
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

    public class ImportJobDescriptor
    {
        public EnvironmentDataType DataType { get; set; }
        public GeoRect GeoRect { get; set; }
        public NAVOTimePeriod TimePeriod { get; set; }
        public float Resolution { get; set; }
        public string DestinationFilename { get; set; }
        public uint SampleCount { get; set; }
        //public Action<ImportJobDescriptor> CompletionAction { get; set; }
        public Task<ImportJobDescriptor> CompletionTask { get; set; }
        public Func<Object, ImportJobDescriptor> CompletionFunction { get; set; }
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
                    Debug.WriteLine("{0} Importer has caught an exception.  Message follows.", DateTime.Now);
                    System.Media.SystemSounds.Beep.Play();
                    Status = "Error";
                    ToolTip = "";
                    foreach (var ex in _importer.Completion.Exception.InnerExceptions)
                        ToolTip += FormatExceptionMessage(ex, 0) + "\r\n";
                    ToolTip.Remove(ToolTip.Length - 2, 2);
                    ToolTip.Trim();
                    Debug.WriteLine("{0} {1}", DateTime.Now, ToolTip);
                }
            });
        }

        public string FormatExceptionMessage(Exception exception, int indentLevel)
        {
            return new string(' ', 2 * indentLevel) + ((exception.InnerException == null)
                                                           ? exception.Message
                                                           : exception.Message + "\r\n" + FormatExceptionMessage(exception.InnerException, indentLevel + 1));
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

        public async void JobCompleted(ImportJobDescriptor job)
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
            if (IsFaulted) return;
            Status = IsWorkInProgress ? string.Format("{0:0}%", ((float)Completed / Submitted) * 100) : "";
            ToolTip = IsWorkInProgress ? string.Format("{0} jobs completed\r\n{1} jobs in queue\r\n{2} jobs running", Completed, Submitted - Completed, Running) : null;
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
}