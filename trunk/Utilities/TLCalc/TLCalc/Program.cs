using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using System.Timers;
using ESME.Environment;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;
using HRC.Utility;

namespace TLCalc
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            // input argument: directory to scan
            var searchPath = args[0];
            // Input files are put in this directory after they have been processed.
            var processedPath = args[1];

#if true
            // Usage: TLCalc <SearchPath> <ProcessedPath>
            while (true)
            {
                var files = Directory.GetFiles(searchPath, "*.bellhop");
                foreach (var file in files)
                {
                    var runFile = BellhopRunFile.Load(file);
                    var outputFileName = Path.Combine(Path.GetDirectoryName(file),
                                                      Path.GetFileNameWithoutExtension(file) + ".tlf");
                    var fieldCalculator = new FieldCalculator()
                                              {
                                                  OutputFileName = outputFileName,
                                                  RunFile = runFile,
                                              };
                    fieldCalculator.ComputeField();

                    var destFileName = Path.Combine(processedPath, Path.GetFileName(file));
                    File.Delete(destFileName);
                    File.Move(file, destFileName);
                }
                Thread.Sleep(1000);
            }
#else
            // Usage: TLCalc <SearchPath> <ProcessedPath> <EnvironmentFile> <OutputFile>
            var environmentFile = args[2];
            var outputFile = args[3];
            #region create transmission loss job. the new base class for all acoustic simulations!

            var transmissionLossJob = new TransmissionLossJob
                                          {
                                              AcousticProperties = new AcousticProperties
                                                                       {
                                                                           SourceDepth_meters = 5,
                                                                           VerticalBeamWidth_degrees = 10,
                                                                           DepressionElevationAngle_degrees =
                                                                               (float)20.2,
                                                                           LowFrequency_Hz = 50,
                                                                           HighFrequency_Hz = 10000,
                                                                       },
                                              NewAnalysisPoint = new NewAnalysisPoint
                                                                     {
                                                                         IDField = 1,
                                                                         Location = new EarthCoordinate(26, -76.8),
                                                                         RadialBearing = 0,
                                                                         RadialCount = 16,

                                                                     },
                                              Radius = 50000,
                                              MaxDepth = 3000,
                                          };

            #endregion

            #region create bellhop run file from tlj (and stuff)

            var environmentInformation = new EnvironmentInformation
                                             {
                                                 Bathymetry = new Bathymetry(environmentFile),
                                                 SoundSpeedField = new SoundSpeedField(environmentFile),
                                                 Sediment = SedimentTypes.SedimentArray[0],
                                             };

            var transmissionLossSettings = new TransmissionLossSettings()
                                               {
                                                   DepthCellSize = 50,
                                                   RangeCellSize = 50,
                                               };

            var bellhopRunFile = BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);

            // things transmissionLossJob currently doesn't include: a radialList, transmissionLossField.


            #endregion



            #region serialize bellhop run file object to file.

            bellhopRunFile.Save(outputFile);

            #endregion

            #region deserialize bellhop run file from file to new object -- inside a timer tick with logic?

            var newRunFile = BellhopRunFile.Load(outputFile);

            #endregion

            #region run bellhop on new run file object.
            //ComputeField(newRunFile, @"C:\tests\bellhop\50kmbahamas.tlf");

            #endregion
#endif

        }

        //public void ComputeField(object sender, DoWorkEventArgs e)
        //todo: fieldcalculator class.
        public static void ComputeField(BellhopRunFile runFile, string outputFileName)
        {
            var fieldData = new TransmissionLossField(runFile)
                                {
                                    Filename = outputFileName,

                                };
            int radialNum = 0;
            foreach (var radial in runFile.BellhopRadials)
            {
                Console.WriteLine(@"computing radial {0} of {1}", radialNum++, runFile.BellhopRadials.Count);
                var result = ComputeRadial(radial.Configuration, radial.BottomProfile, radial.BearingFromSource_degrees);
                fieldData.AddRadial(result);
            }

            fieldData.Depths = fieldData.Radials[0].Depths;
            fieldData.Ranges = fieldData.Radials[0].Ranges;
            fieldData.Save();
        }

        //public void ComputeRadial(object sender, DoWorkEventArgs e, string BellhopConfiguration, string BottomProfile, float Bearing)

        //todo: put below in a class in esme.tl.bellhop.radialcalculator; examine what's in bellhop now and prune 
        public static TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, float bearing)
        {
            //ImprovedBackgroundWorker bw;

            //if ((sender == null))
            //    return;
            //bw = (ImprovedBackgroundWorker)sender;
            //System.Diagnostics.Debug.WriteLine("Worker: Beginning computation of " + bw.TaskName);

            //bw.WorkerReportsProgress = true;
            //bw.WorkerSupportsCancellation = true;

            var workingDirectory = Path.Combine(Path.GetTempPath(),
                                                   Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));

            Directory.CreateDirectory(workingDirectory);

            // Write the bottom profile file that will be read by BELLHOP
            using (var writer = new StreamWriter(Path.Combine(workingDirectory, "BTYFIL")))
                writer.Write(bottomProfile);

            // Create a process, run BELLHOP, and exit
            //BellhopProcess = new TLProcess(bw);
            var bellhopProcess = new TLProcess
                                     {
                                         StartInfo = new ProcessStartInfo(
                                             Path.Combine(
                                                 Path.GetDirectoryName(
                                                     System.Reflection.Assembly.GetCallingAssembly().Location),
                                                 "Bellhop.exe"))
                                                         {
                                                             CreateNoWindow = true,
                                                             UseShellExecute = false,
                                                             RedirectStandardInput = true,
                                                             RedirectStandardOutput = true,
                                                             RedirectStandardError = true,
                                                             WorkingDirectory = workingDirectory
                                                         }
                                     };

            bellhopProcess.OutputDataReceived += OutputDataRecieved;
            _mBellhopOutputData = new StringBuilder();
            bellhopProcess.Start();
            bellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            bellhopProcess.StandardInput.WriteLine(bellhopConfiguration);
            bellhopProcess.BeginOutputReadLine();
            while (!bellhopProcess.HasExited)
            {
                Thread.Sleep(100);
#if false
                if (bw.CancellationPending)
                {
                    BellhopProcess.Kill();
                    ErrorOutput = BellhopProcess.StandardError.ReadToEnd();
                    while (!BellhopProcess.HasExited)
                        Thread.Sleep(100);
                    if (File.Exists(Path.Combine(WorkingDirectory, "BYTFIL")))
                        File.Delete(Path.Combine(WorkingDirectory, "BTYFIL"));
                    if (File.Exists(Path.Combine(WorkingDirectory, "SHDFIL")))
                        File.Delete(Path.Combine(WorkingDirectory, "SHDFIL"));
                    if (File.Exists(Path.Combine(WorkingDirectory, "ARRFIL")))
                        File.Delete(Path.Combine(WorkingDirectory, "ARRFIL"));
                    if (Directory.Exists(WorkingDirectory))
                        Directory.Delete(WorkingDirectory, true);
                    return;
                }
                
#endif               // mProgress_percent = BellhopProcess.ProgressPercent;
            }
            // mProgress_percent = BellhopProcess.ProgressPercent;
            bellhopProcess.StandardError.ReadToEnd();
            //if (BellhopOutputData.ToString() != "")
            //    MessageBox.Show(BellhopOutputData.ToString());
            //System.Diagnostics.Debug.WriteLine("BELLHOP Exit Code: " + BellhopProcess.ExitCode);

            // We don't need to keep the results files around anymore, we're finished with them
            File.Delete(Path.Combine(workingDirectory, "BTYFIL"));
            foreach (var s in Directory.GetFiles(workingDirectory, "ARRFIL_*"))
                File.Delete(s);
            //if (File.Exists(Path.Combine(WorkingDirectory, "ARRFIL")))
            //    File.Move(Path.Combine(WorkingDirectory, "ARRFIL"), "ARRFIL_" + TLParams.RadialNumber.ToString("00"));

            // Convert the Bellhop output file into a Radial binary file
            var result = new TransmissionLossRadial(bearing,
                new BellhopOutput(Path.Combine(workingDirectory, "SHDFIL")));

#if false
            
            TLParams.SoundSource.TransmissionLossVertical[TLParams.RadialNumber] =
                new TransmissionLossVertical(
                    ReaderBellhopOutput.ReadBellhop(
                        Path.Combine(WorkingDirectory, "SHDFIL")),
                        TLParams.BottomProfile,
                        TLParams.Experiment,
                        TLParams.SoundSource,
                        TLParams.SSPLocation,
                        TLParams.RadialNumber);
#endif
            File.Delete(Path.Combine(workingDirectory, "SHDFIL"));
            //bw.ReportProgress(100);
            Directory.Delete(workingDirectory, true);
            return result;
            //System.Diagnostics.Debug.WriteLine("Worker: Completed computation of " + bw.TaskName);
        }
        //private field
        private static StringBuilder _mBellhopOutputData;

        private static void OutputDataRecieved(object sendingProcess, DataReceivedEventArgs outLine)
        {
            TLProcess theProcess = (TLProcess)sendingProcess;
            string CurLine;
            string[] Fields;
            char[] Separators = { ' ', '=' };

            // Collect the sort command output.
            if (!String.IsNullOrEmpty(outLine.Data))
            {
                // Add the text to the collected output.
                _mBellhopOutputData.Append(outLine.Data);
                CurLine = outLine.Data.ToString().Trim();
                if (CurLine.StartsWith("Tracing beam"))
                {
                    Fields = CurLine.Split(Separators, StringSplitOptions.RemoveEmptyEntries);
                    theProcess.CurBeam = int.Parse(Fields[2]);
                    //System.Diagnostics.Debug.WriteLine("Currently tracing beam " + theProcess.CurBeam + " of " + theProcess.MaxBeam + " (" + theProcess.ProgressPercent.ToString("0.0") + "%)");
                }
                if (CurLine.StartsWith("Number of beams"))
                {
                    Fields = CurLine.Split(Separators);
                    theProcess.BeamCount = int.Parse(Fields[Fields.Length - 1]);
                }
            }
        } 
    }
}