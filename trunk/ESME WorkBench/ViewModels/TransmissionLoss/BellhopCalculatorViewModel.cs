using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class BellhopCalculatorViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

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

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.TransmissionLossField);
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
            }
        }

        static readonly PropertyChangedEventArgs TotalProgressChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.TotalProgress);
        float _totalProgress;

        #endregion

        #region public BellhopRunFile BellhopRunFile { get; set; }

        public BellhopRunFile BellhopRunFile
        {
            get { return _bellhopRunFile; }
            set
            {
                if (_bellhopRunFile == value) return;
                _bellhopRunFile = value;
                NotifyPropertyChanged(BellhopRunFileChangedEventArgs);
                if (_dispatcher != null) SetupRadialViewModels();
            }
        }

        static readonly PropertyChangedEventArgs BellhopRunFileChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.BellhopRunFile);
        BellhopRunFile _bellhopRunFile;

        void SetupRadialViewModels()
        {
            if ((_dispatcher == null) || (BellhopRunFile == null)) return;

            TransmissionLossField = new TransmissionLossField(BellhopRunFile);
            for (var i = 0; i < BellhopRunFile.BellhopRadials.Count; i++)
            {
                var radialViewModel = new BellhopRadialCalculatorViewModel(_bellhopRunFile.BellhopRadials[i], i, _dispatcher);
                radialViewModel.PropertyChanged += (s, e) =>
                                                   {
                                                       if (e.PropertyName != "ProgressPercent") return;
                                                       float radialCount = BellhopRadialCalculatorViewModels.Count;
                                                       var progress = BellhopRadialCalculatorViewModels.Sum(radial => radial.ProgressPercent/radialCount);
                                                       TotalProgress = progress;
                                                   };
                BellhopRadialCalculatorViewModels.Add(radialViewModel);
            }
            var bw = new BackgroundWorker();
            bw.DoWork += Calculate;
            bw.RunWorkerCompleted += delegate { CloseActivePopUpCommand.Execute(true); };
            bw.RunWorkerAsync(_bellhopRunFile);
        }

        #endregion

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
            if (BellhopRunFile != null) SetupRadialViewModels();
        }

        #endregion

        #region PropertyChangedCallback PropertyChangedDelegate { get; }

        delegate void PropertyChangedCallback(PropertyChangedEventArgs eventArgs);

        PropertyChangedCallback PropertyChangedDelegate
        {
            get { return _propertyChangedDelegate ?? (_propertyChangedDelegate = new PropertyChangedCallback(NotifyPropertyChanged)); }
        }

        static readonly PropertyChangedEventArgs PropertyChangedDelegateChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.PropertyChangedDelegate);
        PropertyChangedCallback _propertyChangedDelegate;

        #endregion

        #region public ObservableCollection<BellhopRadialCalculatorViewModel> BellhopRadialCalculatorViewModels { get; set; }

        public ObservableCollection<BellhopRadialCalculatorViewModel> BellhopRadialCalculatorViewModels
        {
            get { return _bellhopRadialCalculatorViewModels; }
            set
            {
                if (_bellhopRadialCalculatorViewModels == value) return;
                if (_bellhopRadialCalculatorViewModels != null) _bellhopRadialCalculatorViewModels.CollectionChanged -= BellhopRadialCalculatorViewModelsCollectionChanged;
                _bellhopRadialCalculatorViewModels = value;
                if (_bellhopRadialCalculatorViewModels != null) _bellhopRadialCalculatorViewModels.CollectionChanged += BellhopRadialCalculatorViewModelsCollectionChanged;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs));
            }
        }

        void BellhopRadialCalculatorViewModelsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(BellhopRadialCalculatorViewModelsChangedEventArgs); }
        static readonly PropertyChangedEventArgs BellhopRadialCalculatorViewModelsChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.BellhopRadialCalculatorViewModels);
        ObservableCollection<BellhopRadialCalculatorViewModel> _bellhopRadialCalculatorViewModels = new ObservableCollection<BellhopRadialCalculatorViewModel>();

        #endregion

        #region BackgroundWorker thread that computes the TransmissionLossField in parallel

        void Calculate(object sender, DoWorkEventArgs args)
        {
            var runFile = (BellhopRunFile) args.Argument;
            var radialNum = 0;
            var radialProgress = 100f/runFile.BellhopRadials.Count;
            TotalProgress = 0f;

            Parallel.ForEach<BellhopRadial, float>(runFile.BellhopRadials, () => 0, (radial, loopstate, progress) =>
                                                                                    {
                                                                                        var localRadialNum = Interlocked.Increment(ref radialNum);
                                                                                        var radialViewModel = BellhopRadialCalculatorViewModels[localRadialNum - 1];
                                                                                        radialViewModel.Start();
                                                                                        return radialProgress;
                                                                                    }, (finalResult) => TotalProgress += finalResult);
            foreach (var radial in BellhopRadialCalculatorViewModels) TransmissionLossField.AddRadial(radial.TransmissionLossRadial);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            _dispatcher.Invoke(PropertyChangedDelegate, TransmissionLossFieldChangedEventArgs);
        }

        #endregion
    }

    public class BellhopRadialCalculatorViewModel : ViewModelBase
    {
        readonly Dispatcher _dispatcher;
        readonly StringBuilder _bellhopOutputData;
        readonly BellhopRadial _bellhopRadial;

        public BellhopRadialCalculatorViewModel(BellhopRadial bellhopRadial, int radialNumber, Dispatcher dispatcher)
        {
            //dispatcher.VerifyAccess();
            _bellhopRadial = bellhopRadial;
            _dispatcher = dispatcher;
            _bellhopOutputData = new StringBuilder();
            RadialNumber = radialNumber;
            BearingFromSource = bellhopRadial.BearingFromSourceDegrees;
            Status = "Starting";
        }

        public void Start()
        {
            TransmissionLossRadial = ComputeRadial(_bellhopRadial.Configuration, _bellhopRadial.BottomProfile, _bellhopRadial.BearingFromSourceDegrees);
            Status = "Complete";
        }

        #region public double BearingFromSource { get; set; }

        public double BearingFromSource
        {
            get { return _bearingFromSource; }
            set
            {
                if (_bearingFromSource == value) return;
                _bearingFromSource = value;
                NotifyPropertyChanged(BearingFromSourceChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BearingFromSourceChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.BearingFromSource);
        double _bearingFromSource;

        #endregion

        #region public TransmissionLossRadial TransmissionLossRadial { get; set; }

        public TransmissionLossRadial TransmissionLossRadial
        {
            get { return _transmissionLossRadial; }
            set
            {
                if (_transmissionLossRadial == value) return;
                _transmissionLossRadial = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRadialChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.TransmissionLossRadial);
        TransmissionLossRadial _transmissionLossRadial;

        #endregion

        #region public int RadialNumber { get; set; }

        public int RadialNumber
        {
            get { return _radialNumber; }
            set
            {
                if (_radialNumber == value) return;
                _radialNumber = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(RadialNumberChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs RadialNumberChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.RadialNumber);
        int _radialNumber;

        #endregion

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(StatusChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.Status);
        string _status;

        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent == value) return;
                _progressPercent = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ProgressPercentChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.ProgressPercent);
        int _progressPercent;

        #endregion

        #region public string ErrorText { get; set; }

        public string ErrorText
        {
            get { return _errorText; }
            set
            {
                if (_errorText == value) return;
                _errorText = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ErrorTextChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs ErrorTextChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.ErrorText);
        string _errorText;

        #endregion

        #region Code that computes the radial by running bellhop and reading the output files it creates

        TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, float bearing)
        {
            var workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));

            Directory.CreateDirectory(workingDirectory);

            // Write the bottom profile file that will be read by BELLHOP
            using (var writer = new StreamWriter(Path.Combine(workingDirectory, "BTYFIL"))) writer.Write(bottomProfile);

            var bellhopProcess = new TransmissionLossProcess
                                 {
                                     StartInfo = new ProcessStartInfo(Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "Bellhop.exe"))
                                                 {
                                                     CreateNoWindow = true,
                                                     UseShellExecute = false,
                                                     RedirectStandardInput = true,
                                                     RedirectStandardOutput = true,
                                                     RedirectStandardError = true,
                                                     WorkingDirectory = workingDirectory
                                                 },
                                 };
            bellhopProcess.PropertyChanged += (sender, e) => { if (e.PropertyName == "ProgressPercent") ProgressPercent = ((TransmissionLossProcess) sender).ProgressPercent; };
            bellhopProcess.OutputDataReceived += OutputDataRecieved;
            bellhopProcess.Start();
            bellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            bellhopProcess.StandardInput.WriteLine(bellhopConfiguration);
            bellhopProcess.BeginOutputReadLine();
            Status = "Running";
            while (!bellhopProcess.HasExited)
            {
                Thread.Sleep(100);
            }
            ErrorText = bellhopProcess.StandardError.ReadToEnd();
            //Debug.WriteLine("Bellhop error output for radial bearing " + bearing + " deg: \n" + ErrorText);

            // We don't need to keep the results files around anymore, we're finished with them
            File.Delete(Path.Combine(workingDirectory, "BTYFIL"));
            foreach (var s in Directory.GetFiles(workingDirectory, "ARRFIL_*")) File.Delete(s);

            // Convert the Bellhop output file into a Radial binary file
            var shdfile = Path.Combine(workingDirectory, "SHDFIL");
            var count = 0;
            while (!File.Exists(shdfile) && (count < 10))
            {
                Thread.Sleep(200);
                count++;
            }
            var result = new TransmissionLossRadial(bearing, new BellhopOutput(shdfile));
            File.Delete(Path.Combine(workingDirectory, "SHDFIL"));
            Directory.Delete(workingDirectory, true);
            bellhopProcess.ProgressPercent = 100;
            return result;
        }

        void OutputDataRecieved(object sendingProcess, DataReceivedEventArgs outLine)
        {
            var theProcess = (TransmissionLossProcess) sendingProcess;
            string[] fields;
            char[] separators = {
                                    ' ', '='
                                };

            // Collect the command output.
            if (String.IsNullOrEmpty(outLine.Data)) return;
            // Add the text to the collected output.
            _bellhopOutputData.Append(outLine.Data);
            var curLine = outLine.Data.Trim();
            if (curLine.StartsWith("Tracing beam"))
            {
                fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                theProcess.CurBeam = int.Parse(fields[2]);
            }
            if (!curLine.StartsWith("Number of beams")) return;
            fields = curLine.Split(separators);
            theProcess.BeamCount = int.Parse(fields[fields.Length - 1]);
        }

        #endregion
    }
}