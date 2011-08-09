using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Windows.Threading;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESME.Views.TransmissionLoss
{
    public class BellhopRadialCalculatorViewModel : TransmissionLossRadialCalculatorViewModel
    {
        readonly BellhopRunFileRadial _bellhopRunFileRadial;

        public BellhopRadialCalculatorViewModel(BellhopRunFileRadial bellhopRunFileRadial, int radialNumber, Dispatcher dispatcher) : base(radialNumber, dispatcher)
        {
            _bellhopRunFileRadial = bellhopRunFileRadial;
            BearingFromSource = bellhopRunFileRadial.BearingFromSourceDegrees;
        }

        public override void Start()
        {
            Status = "Starting";
            TransmissionLossRadial = ComputeRadial(_bellhopRunFileRadial.Configuration, _bellhopRunFileRadial.BottomProfile, _bellhopRunFileRadial.BearingFromSourceDegrees);
            Status = "Complete";
        }

        #region Code that computes the radial by running bellhop and reading the output files it creates

        TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, float bearing)
        {
            var workingDirectory = CreateTemporaryDirectory();

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
            bellhopProcess.PropertyChanged += (sender, e) => { if (e.PropertyName == "ProgressPercent") 
                ProgressPercent = Math.Max(ProgressPercent, ((TransmissionLossProcess) sender).ProgressPercent); };
#if DEBUG
            File.WriteAllText(Path.Combine(workingDirectory, "bellhop.config"), bellhopConfiguration);
#endif
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

            if (ErrorText.Contains("forrtl"))
            {
                Console.WriteLine("{0}: Bellhop failure: {1}", DateTime.Now, ErrorText);
                Console.WriteLine("{0}: Bellhop input: {1}", DateTime.Now, bellhopConfiguration);
                Status = "Error";
                return null;
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
            OutputData.Append(outLine.Data);
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