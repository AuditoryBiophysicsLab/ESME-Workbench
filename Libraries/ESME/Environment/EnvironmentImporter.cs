using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
using Cinch;
using ESME.Environment.Descriptors;
using ESME.Plugins;
using HRC.Navigation;
using ESME.Environment.NAVO;
using HRC.NetCDF;
using HRC.Utility;

namespace ESME.Environment
{
    public static class Logger
    {
        public static void Start(string fileName)
        {
            if (_writer != null) _writer.Close();
            _writer = File.AppendText(fileName);
        }

        public static void LogString(string message) { Log(message); }

        static StreamWriter _writer;
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static void Log(string format, params object[] args)
        {
            //Console.WriteLine("{0} {1}", DateTime.Now, string.Format(format, args));
            //if (_writer == null) return;
            //_writer.WriteLine("{0} {1}", DateTime.Now, string.Format(format, args));
            //_writer.Flush();
        }

        public static void Stop()
        {
            if (_writer != null) _writer.Close();
            _writer = null;
        }
    }

    public static class NAVOImporter
    {
        static readonly ActionBlock<ImportJobDescriptor> SoundSpeedWorker;
        static readonly ActionBlock<ImportJobDescriptor> SedimentWorker;
        static readonly ActionBlock<ImportJobDescriptor> WindWorker;
        static readonly ActionBlock<ImportJobDescriptor> BathymetryWorker;
#if IS_CLASSIFIED_MODEL
        static readonly ActionBlock<ImportJobDescriptor> BottomLossWorker;
        public static readonly ImportProgressViewModel BottomLossProgress;
#endif
        public static readonly ImportProgressViewModel SoundSpeedProgress;
        public static readonly ImportProgressViewModel SedimentProgress;
        public static readonly ImportProgressViewModel WindProgress;
        public static readonly ImportProgressViewModel BathymetryProgress;

        public static IPluginManagerService PluginManagerService { get; set; }

        static void CheckDestinationDirectory(string destinationFilename)
        {
            if (string.IsNullOrEmpty(destinationFilename)) throw new ArgumentNullException("destinationFilename");
            var destinationDirectory = Path.GetDirectoryName(destinationFilename);
            if (string.IsNullOrEmpty(destinationDirectory)) throw new ArgumentException("Destination filename must contain a full path", "destinationFilename");
            if (!Directory.Exists(destinationDirectory)) Directory.CreateDirectory(destinationDirectory);
        }

        static NAVOImporter()
        {
            NcVarShort.Logger = Logger.LogString;
            NetCDFFile.Logger = Logger.LogString;
            //NetCDFReaders.Logger = Logger.LogString;
            Logger.Start(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "esme.log"));
            Logger.Log("About to create soundspeed worker");
            if (SoundSpeedWorker == null) SoundSpeedWorker = new ActionBlock<ImportJobDescriptor>(async job =>
            {
                SoundSpeedProgress.JobStarting(job);
                CheckDestinationDirectory(job.DestinationFilename);
                if (PluginManagerService != null)
                {
                    var soundSpeed = PluginManagerService.SoundSpeedSource.Extract(job.GeoRect, 15, job.TimePeriod);
                    soundSpeed.Serialize(job.DestinationFilename);
                    job.SampleCount = (uint)soundSpeed[job.TimePeriod].EnvironmentData.Count;
                }
#if false
                var soundSpeedField = GDEM.ReadFile(job.TimePeriod, job.GeoRect);
                var soundSpeed = new SoundSpeed();
                soundSpeed.SoundSpeedFields.Add(soundSpeedField);
                soundSpeed.Serialize(job.DestinationFilename);
                job.SampleCount = (uint)soundSpeedField.EnvironmentData.Count;
#endif
                job.Resolution = 15;
                job.CompletionTask.Start();
                await job.CompletionTask;
                SoundSpeedProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = Globals.AppSettings.MaxImportThreadCount,
                MaxDegreeOfParallelism = Globals.AppSettings.MaxImportThreadCount,
            });
            SoundSpeedProgress = new ImportProgressViewModel("Sound Speed", SoundSpeedWorker);

            if (SedimentWorker == null) SedimentWorker = new ActionBlock<ImportJobDescriptor>(job =>
            {
                SedimentProgress.JobStarting(job);
                CheckDestinationDirectory(job.DestinationFilename);
                var sediment = BST.Extract(job.GeoRect);
                sediment.Save(job.DestinationFilename);
                job.SampleCount = (uint)sediment.Samples.Count;
                job.Resolution = 5;
                job.CompletionTask.Start();
               SedimentProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = 1,
            });
            SedimentProgress = new ImportProgressViewModel("Sediment", SedimentWorker);

            if (WindWorker == null) WindWorker = new ActionBlock<ImportJobDescriptor>(async job =>
            {
                WindProgress.JobStarting(job);
                CheckDestinationDirectory(job.DestinationFilename);
                var wind = await SMGC.ImportAsync(job.GeoRect);
                wind.Save(job.DestinationFilename);
                job.SampleCount = (uint)wind.TimePeriods[0].EnvironmentData.Count;
                job.Resolution = 60;
                job.CompletionTask.Start();
                await job.CompletionTask;
                WindProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = Globals.AppSettings.MaxImportThreadCount,
            });
            WindProgress = new ImportProgressViewModel("Wind", WindWorker);

            if (BathymetryWorker == null) BathymetryWorker = new ActionBlock<ImportJobDescriptor>(async job =>
            {
                try
                {
                    BathymetryProgress.JobStarting(job);
                    CheckDestinationDirectory(job.DestinationFilename);
                    var bathymetry = DBDB.Extract(job.Resolution, job.GeoRect);
                    bathymetry.Save(job.DestinationFilename);
                    job.SampleCount = (uint)bathymetry.Samples.Count;
                    job.CompletionTask.Start();
                    await job.CompletionTask;
                    var colormap = new DualColormap(Colormap.Summer, Colormap.Jet) { Threshold = 0 };
                    var bathysize = Math.Max(bathymetry.Samples.Longitudes.Count, bathymetry.Samples.Latitudes.Count);
                    var screenSize = Math.Min(SystemParameters.PrimaryScreenWidth, SystemParameters.PrimaryScreenHeight);
                    var displayValues = bathymetry.Samples;
                    if (bathysize > screenSize)
                    {
                        var scaleFactor = screenSize / bathysize;
                        displayValues = EnvironmentData<Geo<float>>.Decimate(bathymetry.Samples,
                                                                                        (int)(bathymetry.Samples.Longitudes.Count * scaleFactor),
                                                                                        (int)(bathymetry.Samples.Latitudes.Count * scaleFactor));
                    }

                    var imageFilename = Path.GetFileNameWithoutExtension(job.DestinationFilename) + ".bmp";
                    var imagePath = Path.GetDirectoryName(job.DestinationFilename);

                    var bitmapData = new float[displayValues.Longitudes.Count, displayValues.Latitudes.Count];
                    for (var latIndex = 0; latIndex < bitmapData.GetLength(1); latIndex++) 
                        for (var lonIndex = 0; lonIndex < bitmapData.GetLength(0); lonIndex++) 
                            bitmapData[lonIndex, latIndex] = displayValues[(uint)lonIndex, (uint)latIndex].Data;

                    var displayData = colormap.ToPixelValues(bitmapData, bathymetry.Minimum.Data, bathymetry.Maximum.Data < 0 ? 
                                      bathymetry.Maximum.Data : 8000, Colors.Black);
                    BitmapWriter.Write(Path.Combine(imagePath, imageFilename), displayData);

                    var sb = new StringBuilder();
                    sb.AppendLine(job.Resolution.ToString(CultureInfo.InvariantCulture));
                    sb.AppendLine("0.0");
                    sb.AppendLine("0.0");
                    sb.AppendLine(job.Resolution.ToString(CultureInfo.InvariantCulture));
                    sb.AppendLine(bathymetry.Samples.GeoRect.West.ToString(CultureInfo.InvariantCulture));
                    sb.AppendLine(bathymetry.Samples.GeoRect.North.ToString(CultureInfo.InvariantCulture));
                    using (var writer = new StreamWriter(Path.Combine(imagePath, Path.GetFileNameWithoutExtension(imageFilename) + ".bpw"), false)) writer.Write(sb.ToString());
                    BathymetryProgress.JobCompleted(job);
                }
                catch (Exception e)
                {
                    Debug.WriteLine("Bathymetry extraction caught exception: {0}", e.Message);
                    throw;
                }
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = Globals.AppSettings.MaxImportThreadCount,
            });
            BathymetryProgress = new ImportProgressViewModel("Bathymetry", BathymetryWorker);
#if IS_CLASSIFIED_MODEL
            if (BottomLossWorker == null) BottomLossWorker = new ActionBlock<ImportJobDescriptor>(async
            job =>
            {
                BottomLossProgress.JobStarting(job);
                CheckDestinationDirectory(job.DestinationFilename);
                var bottomLoss = await BottomLossDatabase.ExtractAsync(job.GeoRect);
                bottomLoss.Save(job.DestinationFilename);
                job.SampleCount = (uint)bottomLoss.Samples.Count;
                job.Resolution = 15;
                job.CompletionTask.Start();
                await job.CompletionTask;
                BottomLossProgress.JobCompleted(job);
            },
            new ExecutionDataflowBlockOptions
            {
                TaskScheduler = TaskScheduler.Default,
                BoundedCapacity = -1,
                MaxDegreeOfParallelism = Globals.AppSettings.MaxImportThreadCount,
            });
            BottomLossProgress = new ImportProgressViewModel("Bottom Loss", BottomLossWorker);
#endif
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
#if IS_CLASSIFIED_MODEL
                case EnvironmentDataType.BottomLoss:
                    BottomLossProgress.Post(jobDescriptor);
                    break;
#endif
#if false
                case EnvironmentDataType.Salinity:
                    SalinityProgress.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Temperature:
                    //Logger.Log("Temperature job about to post");
                    TemperatureProgress.Post(jobDescriptor);
                    //Logger.Log("Temperature job after post");
                    break;
#endif
                case EnvironmentDataType.SoundSpeed:
                    SoundSpeedProgress.Post(jobDescriptor);
                    break;
                case EnvironmentDataType.Sediment:
                    SedimentProgress.Post(jobDescriptor);
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
        public TimePeriod TimePeriod { get; set; }
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
                NAVOImporter.SoundSpeedProgress,
                NAVOImporter.BathymetryProgress,
#if IS_CLASSIFIED_MODEL
                NAVOImporter.BottomLossProgress,
#endif
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
            catch
            {
                _dispatcher.InvokeInBackgroundIfRequired(() =>
                {
                    IsCompleted = _importer.Completion.IsCompleted;
                    IsCanceled = _importer.Completion.IsCanceled;
                    IsFaulted = _importer.Completion.IsFaulted;
                    if (!IsFaulted) return;
                    //Logger.Log("Importer has caught an exception.  Message follows.");
                    System.Media.SystemSounds.Beep.Play();
                    Status = "Error";
                    ToolTip = "";
                    if (_importer.Completion.Exception != null)
                        foreach (var ex in _importer.Completion.Exception.InnerExceptions) 
                            ToolTip += FormatExceptionMessage(ex, 0) + "\r\n";
                    ToolTip = ToolTip.Remove(ToolTip.Length - 2, 2).Trim();
                });

            }
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
