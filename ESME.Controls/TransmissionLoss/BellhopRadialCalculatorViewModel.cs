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
            TransmissionLossRadial = ComputeRadial(_bellhopRunFileRadial.Configuration, _bellhopRunFileRadial.BottomProfile,_bellhopRunFileRadial.TopReflectionCoefficient, _bellhopRunFileRadial.BearingFromSourceDegrees);
            Status = TransmissionLossRadial == null ? "Error" : CancelRequested ? "Canceled" : "Complete";
            OnCalculationCompleted();
        }

        #region Code that computes the radial by running bellhop and reading the output files it creates

        TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, string topReflectionCoefficients, float bearing)
        {
            var workingDirectory = CreateTemporaryDirectory();
            TransmissionLossRadial result = null;

            // Write the bottom profile file that will be read by BELLHOP);
            File.WriteAllText(Path.Combine(workingDirectory, "BTYFIL"), bottomProfile);
            if (!string.IsNullOrEmpty(topReflectionCoefficients)) File.WriteAllText(Path.Combine(workingDirectory, "TRCFIL"), topReflectionCoefficients);

            TransmissionLossProcess = new TransmissionLossProcess
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
            TransmissionLossProcess.PropertyChanged += (sender, e) => { if (e.PropertyName == "ProgressPercent") 
                ProgressPercent = Math.Max(ProgressPercent, ((TransmissionLossProcess) sender).ProgressPercent); };
#if DEBUG
            //File.WriteAllText(Path.Combine(workingDirectory, "BellhopEnvironmentFile.txt"), bellhopConfiguration);
#endif
            TransmissionLossProcess.OutputDataReceived += OutputDataRecieved;
            TransmissionLossProcess.Start();
            TransmissionLossProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            TransmissionLossProcess.StandardInput.WriteLine(bellhopConfiguration);
            TransmissionLossProcess.BeginOutputReadLine();
            Status = "Running";
            while (!TransmissionLossProcess.HasExited)
            {
                Thread.Sleep(100);
                if ((!CancelRequested) || (TransmissionLossProcess.HasExited)) continue;
                TransmissionLossProcess.Kill();
                break;
            }
            if (!CancelRequested)
            {
                ErrorText = TransmissionLossProcess.StandardError.ReadToEnd();
                //Debug.WriteLine("Bellhop error output for radial bearing " + bearing + " deg: \n" + ErrorText);

                // Convert the Bellhop output file into a Radial binary file
                var shdfile = Path.Combine(workingDirectory, "SHDFIL");
                var count = 0;
                while (!File.Exists(shdfile) && (count < 10))
                {
                    Thread.Sleep(200);
                    count++;
                }

                if (ErrorText.Contains("forrtl") || (ErrorText.Contains("Error")))
                {
                    Console.WriteLine("{0}: Bellhop failure: {1}", DateTime.Now, ErrorText);
                    Console.WriteLine("{0}: Bellhop input: {1}", DateTime.Now, bellhopConfiguration);
                    Status = "Error";
                    return null;
                }
#if DEBUG
                //File.WriteAllText(Path.Combine(workingDirectory, "BellhopStandardOutput.txt"), OutputData.ToString());
#endif
                result = new TransmissionLossRadial(bearing, new BellhopOutput(shdfile));
            }
            var tries = 10;
            while (tries > 0)
            {
                try
                {
                    foreach (var file in Directory.EnumerateFiles(workingDirectory)) File.Delete(file);
                    Directory.Delete(workingDirectory, true);
                    break;
                }
                catch (Exception)
                {
                    tries--;
                    Thread.Sleep(100);
                }
            }
            TransmissionLossProcess.ProgressPercent = 100;
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