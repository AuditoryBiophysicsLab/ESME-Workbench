using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
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
        StringBuilder[] _bellhopOutputData;
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

        #region public string Log { get; }

        public string Log
        {
            get { return _log.ToString(); }
        }

        static readonly PropertyChangedEventArgs LogChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.Log);
        readonly StringBuilder _log = new StringBuilder();

        void LogMessage(string format, params object[] args)
        {
            lock (this) _log.Append(string.Format(format, args));
            _dispatcher.Invoke(PropertyChangedDelegate, LogChangedEventArgs);
        }

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
                var bw = new BackgroundWorker();
                bw.DoWork += Calculate;
                bw.RunWorkerCompleted += delegate { CloseActivePopUpCommand.Execute(true); };
                bw.RunWorkerAsync(_bellhopRunFile);
            }
        }

        static readonly PropertyChangedEventArgs BellhopRunFileChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.BellhopRunFile);
        BellhopRunFile _bellhopRunFile;

        #endregion

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
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

        #region BackgroundWorker thread that computes the TransmissionLossField in parallel

        void Calculate(object sender, DoWorkEventArgs args)
        {
            var runFile = (BellhopRunFile) args.Argument;
            TransmissionLossField = new TransmissionLossField(runFile);
            var radialNum = 0;
            var radialProgress = 100f/runFile.BellhopRadials.Count;
            TotalProgress = 0f;
            LogMessage("{0} Starting bellhop calculation for {1} radials\n", DateTime.Now, runFile.BellhopRadials.Count);
            _bellhopOutputData = new StringBuilder[runFile.BellhopRadials.Count];
            Parallel.ForEach<BellhopRadial, float>(runFile.BellhopRadials, () => 0, (radial, loopstate, progress) =>
                                                                                    {
                                                                                        var localRadialNum = Interlocked.Increment(ref radialNum);
                                                                                        _bellhopOutputData[localRadialNum - 1] = new StringBuilder();
                                                                                        LogMessage("{0} Beginning calculation of radial {1}...\n", DateTime.Now, localRadialNum);
                                                                                        TransmissionLossField.AddRadial(ComputeRadial(radial.Configuration, radial.BottomProfile, radial.BearingFromSourceDegrees, _bellhopOutputData[localRadialNum - 1]));
                                                                                        _dispatcher.Invoke(PropertyChangedDelegate, TransmissionLossFieldChangedEventArgs);
                                                                                        LogMessage("{0} Calculation of radial {1} complete.\n", DateTime.Now, localRadialNum);
                                                                                        return radialProgress;
                                                                                    }, (finalResult) => TotalProgress += finalResult);
            LogMessage("{0} Bellhop calculation complete.\n", DateTime.Now);
            TransmissionLossField.Depths = TransmissionLossField.Radials[0].Depths;
            TransmissionLossField.Ranges = TransmissionLossField.Radials[0].Ranges;
            _dispatcher.Invoke(PropertyChangedDelegate, TransmissionLossFieldChangedEventArgs);
        }

        #endregion

        #region Code that computes the radial by running bellhop and reading the output files it creates

        static TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, float bearing, StringBuilder stringBuilder)
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
                                     StringBuilder = stringBuilder
                                 };

            bellhopProcess.OutputDataReceived += OutputDataRecieved;
            bellhopProcess.Start();
            bellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            bellhopProcess.StandardInput.WriteLine(bellhopConfiguration);
            bellhopProcess.BeginOutputReadLine();
            while (!bellhopProcess.HasExited)
            {
                Thread.Sleep(100);
            }
            Debug.WriteLine("Bellhop error output for radial bearing " + bearing + " deg: \n" + bellhopProcess.StandardError.ReadToEnd());

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
            return result;
        }

        static void OutputDataRecieved(object sendingProcess, DataReceivedEventArgs outLine)
        {
            var theProcess = (TransmissionLossProcess) sendingProcess;
            string[] fields;
            char[] separators = {
                                    ' ', '='
                                };

            // Collect the command output.
            if (String.IsNullOrEmpty(outLine.Data)) return;
            // Add the text to the collected output.
            theProcess.StringBuilder.Append(outLine.Data);
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