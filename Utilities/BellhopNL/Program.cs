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

namespace BellhopNL
{
    class Program
    {
        static void Main(string[] args)
        {
            var data = new DataBlob();
            
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-data":
                        data = DataBlob.Load(args[++i]);
                        break;
                    default:
                        Useage();
                        return;
                }
            }
            string arrivalsFile;
            ComputeRadial(data, out arrivalsFile);
            //give NLWrapper the arrivals file. 
            BellhopNLWrapper.ModelType modelType;
            var modelOk = Enum.TryParse(data.ModelType, true, out modelType);
            if (!modelOk) throw new ApplicationException("modelType invalid.  'arons' or 'chapman'.");
            var result =BellhopNLWrapper.Run(arrivalsFile, data.ChargeDepth, data.ChargeMass, data.OutputFreq, data.OutputTime, modelType).Waveforms;

            //todo: make a refms-like file from this data.
            var records = Transform(result, data);
        }

        static List<EffectsRecord> Transform(double[,,] waveforms, DataBlob data)
        {
            var result = new List<EffectsRecord>();
            for (var i = 0; i < waveforms.GetLength(0); i++)//ranges
            {
                
                for (var j = 0; j < waveforms.GetLength(1); j++)//depths
                {
                    var effectsRecord = new EffectsRecord(){Depth = data.Depths[j],Range = data.Ranges[i]};
                    var maxPa = double.MinValue;
                    for (var k = 0; k < waveforms.GetLength(2); k++)//payload waveform.  May NOT ACTUALLY BE IN Pa!!
                    {
                        if (waveforms[i, j, k] > maxPa) maxPa = waveforms[i, j, k];
                    }
                    effectsRecord.PeakPressurekPa = maxPa;
                    result.Add(effectsRecord);
                }
            }
            return result;
        }

        static void ComputeRadial(DataBlob data, out string shdfile)
        {
            var envFile = File.ReadAllText(data.EnvFilename);
            shdfile = null;
            var outputData = new StringBuilder();
            var workingDirectory = CreateTemporaryDirectory();
            TransmissionLossRadial result = null;

            // Write the bottom profile file that will be read by BELLHOP);
            // File.WriteAllText(Path.Combine(workingDirectory, "bellhop.env"), bellhopConfiguration);
            File.WriteAllText(Path.Combine(workingDirectory, "BTYFIL"), string.Format(@"'L' \n2 \n0 {0} \n{1} {0}\n",data.WaterDepth,data.CalculationRange));

            var trcfil = Path.Combine(workingDirectory, "TRCFIL");
            if (data.TopReflectionCoefficients != null)
            {
                using (var writer = new StreamWriter(trcfil, false))
                {
                    writer.WriteLine(data.TopReflectionCoefficients.GetLength(0));
                    for (var rowIndex = 0; rowIndex < data.TopReflectionCoefficients.GetLength(0); rowIndex++)
                        writer.WriteLine("{0} {1} {2} ", data.TopReflectionCoefficients[rowIndex, 0], data.TopReflectionCoefficients[rowIndex, 1], data.TopReflectionCoefficients[rowIndex, 2]);
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
            transmissionLossProcess.StandardInput.WriteLine(data.BellhopConfiguration);
            transmissionLossProcess.BeginOutputReadLine();
            while (!transmissionLossProcess.HasExited)
            {
                Thread.Sleep(100);
                if ((transmissionLossProcess.HasExited)) continue;
                transmissionLossProcess.Kill();
                break;
            }
            
            var errorText = transmissionLossProcess.StandardError.ReadToEnd();
            Debug.WriteLine("{0}: Bellhop error output for radial bearing {1} deg:\n{2}", DateTime.Now, data.Bearing, errorText);

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
                Console.WriteLine(@"{0}: Bellhop input: {1}", DateTime.Now, data.BellhopConfiguration);
                Console.WriteLine(@"{0}: Bellhop output: {1}", DateTime.Now, outputData);
                return;
            }
            
           // if (File.Exists(shdfile)) result = new TransmissionLossRadial(data.Bearing, new BellhopOutput(shdfile));
            
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
            return;
        }

        protected static string CreateTemporaryDirectory()
        {
            var workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(workingDirectory);
            return workingDirectory;
        }

        static void Useage() { throw new NotImplementedException(); }
    }
}