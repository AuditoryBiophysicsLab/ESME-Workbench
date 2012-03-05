using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Threading;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;

namespace ESME.Views.TransmissionLoss
{
    public class BellhopRadialCalculatorViewModel : TransmissionLossRadialCalculatorViewModel
    {
        readonly BellhopRunFileRadial _bellhopRunFileRadial;

        public BellhopRadialCalculatorViewModel(BellhopRunFileRadial bellhopRunFileRadial, int radialNumber) : base(radialNumber)
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

        TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, double[,] topReflectionCoefficients, float bearing)
        {
            var workingDirectory = CreateTemporaryDirectory();
            TransmissionLossRadial result = null;

            // Write the bottom profile file that will be read by BELLHOP);
            // File.WriteAllText(Path.Combine(workingDirectory, "bellhop.env"), bellhopConfiguration);
            File.WriteAllText(Path.Combine(workingDirectory, "BTYFIL"), bottomProfile);

            var trcfil = Path.Combine(workingDirectory, "TRCFIL");
            if (topReflectionCoefficients != null)
            {
                using (var writer = new StreamWriter(trcfil, false))
                {
                    writer.WriteLine(topReflectionCoefficients.GetLength(0));
                    for (var rowIndex = 0; rowIndex < topReflectionCoefficients.GetLength(0); rowIndex++)
                        writer.WriteLine("{0} {1} {2} ", topReflectionCoefficients[rowIndex, 0], topReflectionCoefficients[rowIndex, 1], topReflectionCoefficients[rowIndex, 2]);
                }
            }
            if (!string.IsNullOrEmpty(LogPath))
            {
                foreach (var file in Directory.EnumerateFiles(Path.GetDirectoryName(LogPath), Path.GetFileName(LogPath) + "*"))
                    File.Delete(file);
                File.WriteAllText(LogPath + "ENVFIL.txt", bellhopConfiguration);
                File.WriteAllText(LogPath + "BTYFIL.txt", bottomProfile);
                if (topReflectionCoefficients != null) File.Copy(LogPath + "TRCFIL.txt", trcfil);
            }
            var bellhopExecutable = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "bellhop.exe");
            var info = new ProcessStartInfo(bellhopExecutable, "bellhop")
            {
                WorkingDirectory = workingDirectory,
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = false,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            };
            //var process = Process.Start(bellhopExecutable, "bellhop");
            TransmissionLossProcess = new TransmissionLossProcess
            {
                StartInfo = info
            };
            TransmissionLossProcess.PropertyChanged += (sender, e) =>
            {
                if (e.PropertyName == "ProgressPercent") 
                    ProgressPercent = Math.Max(ProgressPercent, ((TransmissionLossProcess)sender).ProgressPercent);
            };
#if DEBUG
            //File.WriteAllText(Path.Combine(workingDirectory, "BellhopEnvironmentFile.txt"), bellhopConfiguration);
#endif
            TransmissionLossProcess.OutputDataReceived += (s, e) =>
            {
                var theProcess = (TransmissionLossProcess)s;
                string[] fields;
                char[] separators = { ' ', '=' };

                // Collect the command output.
                if (String.IsNullOrEmpty(e.Data)) return;
                // Add the text to the collected output.
                OutputData.Append(e.Data);
                var curLine = e.Data.Trim();
                if (curLine.StartsWith("Tracing beam"))
                {
                    fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    theProcess.CurBeam = int.Parse(fields[2]);
                }
                if (!curLine.StartsWith("Number of beams")) return;
                fields = curLine.Split(separators);
                theProcess.BeamCount = int.Parse(fields[fields.Length - 1]);
            };
            TransmissionLossProcess.Start();
            TransmissionLossProcess.PriorityClass = ProcessPriorityClass.Idle;
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
                Debug.WriteLine("{0}: Bellhop error output for radial bearing {1} deg:\n{2}", DateTime.Now, bearing, ErrorText);

                // Convert the Bellhop output file into a Radial binary file
                var shdfile = Path.Combine(workingDirectory, "SHDFIL");
                var count = 0;
                while (!File.Exists(shdfile) && (count < 10))
                {
                    Thread.Sleep(200);
                    count++;
                }

#if DEBUG
                //File.WriteAllText(Path.Combine(workingDirectory, "BellhopStandardOutput.txt"), OutputData.ToString());
#endif
                if (!string.IsNullOrEmpty(LogPath))
                {
                    if (File.Exists(shdfile)) File.Copy(shdfile, LogPath + "SHDFIL.shd");
                    File.WriteAllText(LogPath + "STDOUT.txt", OutputData.ToString());
                    File.WriteAllText(LogPath + "STDERR.txt", ErrorText);
                }

                if (ErrorText.Contains("forrtl") || (ErrorText.Contains("Error")))
                {
                    Console.WriteLine("{0}: Bellhop failure: {1}", DateTime.Now, ErrorText);
                    Console.WriteLine("{0}: Bellhop input: {1}", DateTime.Now, bellhopConfiguration);
                    Console.WriteLine("{0}: Bellhop output: {1}", DateTime.Now, OutputData);
                    Status = "Error";
                    return null;
                }
                
                //if (!string.IsNullOrEmpty(LogPath)) File.Copy(shdfile, LogPath + "SHDFIL");
                if (File.Exists(shdfile)) result = new TransmissionLossRadial(bearing, new BellhopOutput(shdfile));
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
            //if (!string.IsNullOrEmpty(LogPath)) File.WriteAllText(LogPath + "STDOUT.txt", OutputData.ToString());
            TransmissionLossProcess.ProgressPercent = 100;
            return result;
        }
        #endregion
    }
}