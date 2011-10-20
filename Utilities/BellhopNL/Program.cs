using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Pipes;
using System.Reflection;
using System.Text;
using System.Threading;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.BellhopNL;
using ESME.TransmissionLoss.REFMS;
using HRC;
using ESME.TransmissionLoss;
using BellhopNLLibNative;

namespace BellhopNLWrapper
{
    class Program
    {
        static void Main(string[] args)
        {
            var dataFile = new BellhopNLInput();
            var outFile = "";
            var arrivalsFile = "";
            var isTest = false;
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-dataFile":
                        dataFile = BellhopNLInput.Load(args[++i]);
                        break;
                    case "-outFile":
                        outFile = args[++i];
                        break;
                    case "-isTest":
                        isTest = bool.Parse(args[++i]);
                        break;
                    case "-arrivalsFile":
                        arrivalsFile = args[++i];
                        break;
                    default:
                        Useage();
                        return;
                }
            }
            //make sure we're using a valid model
            BellhopNLInput.NLModelType modelType;
            if (!Enum.TryParse(dataFile.ModelType, true, out modelType)) throw new ApplicationException("modelType invalid.  'arons' or 'chapman'.");
            //run bellhop.(or skip if arrivals file exists
            if (!isTest) { arrivalsFile = ComputeRadial(dataFile); }
            
            //bellhopNL now computes waveforms and saves them to disk. 
            var result = BellhopNLWrapper.Run(arrivalsFile, dataFile.ChargeDepth, dataFile.ChargeMass,
                                              dataFile.OutputFreq, dataFile.OutputTime, modelType);
            
            BellhopNLWrapper.ComputeThirdOctaves(result); // technically, we don't need to do this in other cases. 
            
            new BellhopNLOutput
            {
                    Waveforms = result.Waveforms,
                    Ranges = result.Ranges,
                    Depths = result.Depths,
                    ChargeDepth = result.ChargeDepth,
                    PeakEnergy = result.PeakEnergy,
                    EFD = result.EFD,
                    MaxEnergy = result.MaxEnergy,
                    ThirdOctaveCenterFrequencies = result.ThirdOctaveCenterFrequencies,
                    OutputTime = dataFile.OutputTime,
            }.Save(outFile);
            
        }

        static string ComputeRadial(BellhopNLInput bellhopNLInput)
        {
            var envFile = File.ReadAllText(bellhopNLInput.EnvFilename);
            string shdfile = null;
            var outputData = new StringBuilder();
            var workingDirectory = CreateTemporaryDirectory();
            TransmissionLossRadial result = null;

            // Write the bottom profile file that will be read by BELLHOP);
            // File.WriteAllText(Path.Combine(workingDirectory, "bellhop.env"), bellhopConfiguration);
            File.WriteAllText(Path.Combine(workingDirectory, "BTYFIL"), string.Format(@"'L' \n2 \n0 {0} \n{1} {0}\n",bellhopNLInput.WaterDepth,bellhopNLInput.CalculationRange));

            var trcfil = Path.Combine(workingDirectory, "TRCFIL");
            if (bellhopNLInput.TopReflectionCoefficients != null)
            {
                using (var writer = new StreamWriter(trcfil, false))
                {
                    writer.WriteLine(bellhopNLInput.TopReflectionCoefficients.GetLength(0));
                    for (var rowIndex = 0; rowIndex < bellhopNLInput.TopReflectionCoefficients.GetLength(0); rowIndex++)
                        writer.WriteLine("{0} {1} {2} ", bellhopNLInput.TopReflectionCoefficients[rowIndex, 0], bellhopNLInput.TopReflectionCoefficients[rowIndex, 1], bellhopNLInput.TopReflectionCoefficients[rowIndex, 2]);
                }
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
           
            var transmissionLossProcess = new TransmissionLossProcess
            {
                StartInfo = info
            };
#if false
            transmissionLossProcess.PropertyChanged += (sender, e) =>
               {
                   if (e.PropertyName == "ProgressPercent")
                       ProgressPercent = Math.Max(ProgressPercent, ((TransmissionLossProcess)sender).ProgressPercent);
               }; 
#endif
            transmissionLossProcess.OutputDataReceived += (s, e) =>
            {
                var theProcess = (TransmissionLossProcess)s;
                string[] fields;
                char[] separators = { ' ', '=' };

                // Collect the command output.
                if (String.IsNullOrEmpty(e.Data)) return;
                // Add the text to the collected output.
                outputData.Append(e.Data);
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
            transmissionLossProcess.Start();
            transmissionLossProcess.PriorityClass = ProcessPriorityClass.Idle;
            transmissionLossProcess.StandardInput.WriteLine(bellhopNLInput.BellhopConfiguration);
            transmissionLossProcess.BeginOutputReadLine();
            while (!transmissionLossProcess.HasExited)
            {
                Thread.Sleep(100);
                if ((transmissionLossProcess.HasExited)) continue;
                transmissionLossProcess.Kill();
                break;
            }
            
            var errorText = transmissionLossProcess.StandardError.ReadToEnd();
            Debug.WriteLine("{0}: Bellhop error output for radial bearing {1} deg:\n{2}", DateTime.Now, bellhopNLInput.Bearing, errorText);

            // Convert the Bellhop output file into a Radial binary file
            
                
            var count = 0;
            while (!File.Exists(shdfile) && (count < 10))
            {
                Thread.Sleep(200);
                count++;
            }

            if (errorText.Contains("forrtl") || (errorText.Contains("Error")))
            {
                Console.WriteLine(@"{0}: Bellhop failure: {1}", DateTime.Now, errorText);
                Console.WriteLine(@"{0}: Bellhop input: {1}", DateTime.Now, bellhopNLInput.BellhopConfiguration);
                Console.WriteLine(@"{0}: Bellhop output: {1}", DateTime.Now, outputData);
                throw new FileIsEmptyException("arrivals file contains errors");
            }
            
           // if (File.Exists(shdfile)) result = new TransmissionLossRadial(BellhopNLInput.Bearing, new BellhopOutput(shdfile));
            
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
            shdfile = Path.Combine(workingDirectory, "SHDFIL");
            if(shdfile == null) throw new FileIsEmptyException("arrivals file not generated!");
            transmissionLossProcess.ProgressPercent = 100;
            return shdfile;
        }



        protected static string CreateTemporaryDirectory()
        {
            var workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(workingDirectory);
            return workingDirectory;
        }

        static void Useage()
        {
            var sb = new StringBuilder();
            sb.AppendLine("BellhopNL");
            sb.AppendLine("-dataFile : a path to a file containing a fully populated, serialzed BellhopNLInput object");
            sb.AppendLine(
                          "-outFile  : a path to a file which will be created or overwritten, containing a serialized BellhopNLOutput object for postprocessing.");
            Console.Write(sb.ToString());
        }
    }
}