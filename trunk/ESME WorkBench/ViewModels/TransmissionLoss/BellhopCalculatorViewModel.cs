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
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class BellhopCalculatorViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        StringBuilder[] _bellhopOutputData;
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        public BellhopCalculatorViewModel()
        {}

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.TransmissionLossField);

        TransmissionLossField _transmissionLossField;

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            private set
            {
                if (value == _transmissionLossField) return;
                _transmissionLossField = value;
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
            }
        }

        public string Log
        {
            get { return _log.ToString(); }
        }

        static readonly PropertyChangedEventArgs LogChangedEventArgs = ObservableHelper.CreateArgs<BellhopCalculatorViewModel>(x => x.Log);
        readonly StringBuilder _log = new StringBuilder();

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

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
        }

        #endregion


        public void Calculate(BellhopRunFile runFile)
        {
            var fieldData = new TransmissionLossField(runFile);
            var radialNum = 0;
            var radialProgress = 100f/runFile.BellhopRadials.Count;
            TotalProgress = 0f;
            _log.Append(String.Format("{0} Starting bellhop calculation\n", DateTime.Now));
            NotifyPropertyChanged(LogChangedEventArgs);
            _bellhopOutputData = new StringBuilder[runFile.BellhopRadials.Count];
            Parallel.ForEach<BellhopRadial, float>(runFile.BellhopRadials, () => 0, (radial, loopstate, progress) =>
                                                                                    {
                                                                                        var localRadialNum = Interlocked.Increment(ref radialNum);
                                                                                        _bellhopOutputData[localRadialNum] = new StringBuilder();
                                                                                        lock (this)
                                                                                        {
                                                                                            _log.Append(String.Format("{0} Launching radial {1} of {2} for calculation...\n", DateTime.Now, localRadialNum, runFile.BellhopRadials.Count));
                                                                                            NotifyPropertyChanged(LogChangedEventArgs);
                                                                                        }
                                                                                        fieldData.AddRadial(ComputeRadial(radial.Configuration, radial.BottomProfile, radial.BearingFromSourceDegrees, _bellhopOutputData[localRadialNum]));
                                                                                        lock (this)
                                                                                        {
                                                                                            _log.Append(String.Format("{0} radial {1} complete.\n", DateTime.Now, localRadialNum));
                                                                                            NotifyPropertyChanged(LogChangedEventArgs);
                                                                                        }
                                                                                        return radialProgress;
                                                                                    }, (finalResult) => TotalProgress += finalResult);
            _log.Append(String.Format("{0} Bellhop calculation complete.\n", DateTime.Now));
            NotifyPropertyChanged(LogChangedEventArgs);
            fieldData.Depths = fieldData.Radials[0].Depths;
            fieldData.Ranges = fieldData.Radials[0].Ranges;
            CloseActivePopUpCommand.Execute(true);
        }

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
            var theProcess = (TransmissionLossProcess)sendingProcess;
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

    }
}