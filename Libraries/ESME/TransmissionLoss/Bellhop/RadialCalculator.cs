using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;

namespace ESME.TransmissionLoss.Bellhop
{
    internal class RadialCalculator
    {
        static StringBuilder _mBellhopOutputData;

        public static TransmissionLossRadial ComputeRadial(string bellhopConfiguration, string bottomProfile, float bearing)
        {
            //ImprovedBackgroundWorker bw;
            //if ((sender == null))
            //    return;
            //bw = (ImprovedBackgroundWorker)sender;
            //System.Diagnostics.Debug.WriteLine("Worker: Beginning computation of " + bw.TaskName);

            //bw.WorkerReportsProgress = true;
            //bw.WorkerSupportsCancellation = true;

            string workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));

            Directory.CreateDirectory(workingDirectory);

            // Write the bottom profile file that will be read by BELLHOP
            using (var writer = new StreamWriter(Path.Combine(workingDirectory, "BTYFIL"))) writer.Write(bottomProfile);

            // Create a process, run BELLHOP, and exit
            //BellhopProcess = new TLProcess(bw);
            var bellhopProcess = new TLProcess
                                 {
                                     StartInfo = new ProcessStartInfo(Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "Bellhop.exe"))
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

#endif
                // mProgress_percent = BellhopProcess.ProgressPercent;
            }
            // mProgress_percent = BellhopProcess.ProgressPercent;
            bellhopProcess.StandardError.ReadToEnd();
            //if (BellhopOutputData.ToString() != "")
            //    MessageBox.Show(BellhopOutputData.ToString());
            //System.Diagnostics.Debug.WriteLine("BELLHOP Exit Code: " + BellhopProcess.ExitCode);

            // We don't need to keep the results files around anymore, we're finished with them
            File.Delete(Path.Combine(workingDirectory, "BTYFIL"));
            foreach (string s in Directory.GetFiles(workingDirectory, "ARRFIL_*")) File.Delete(s);
            //if (File.Exists(Path.Combine(WorkingDirectory, "ARRFIL")))
            //    File.Move(Path.Combine(WorkingDirectory, "ARRFIL"), "ARRFIL_" + TLParams.RadialNumber.ToString("00"));

            // Convert the Bellhop output file into a Radial binary file
            var result = new TransmissionLossRadial(bearing, new BellhopOutput(Path.Combine(workingDirectory, "SHDFIL")));

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

        static void OutputDataRecieved(object sendingProcess, DataReceivedEventArgs outLine)
        {
            var theProcess = (TLProcess) sendingProcess;
            string curLine;
            string[] fields;
            char[] separators = {
                                    ' ', '='
                                };

            // Collect the sort command output.
            if (!String.IsNullOrEmpty(outLine.Data))
            {
                // Add the text to the collected output.
                _mBellhopOutputData.Append(outLine.Data);
                curLine = outLine.Data.Trim();
                if (curLine.StartsWith("Tracing beam"))
                {
                    fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    theProcess.CurBeam = int.Parse(fields[2]);
                    //System.Diagnostics.Debug.WriteLine("Currently tracing beam " + theProcess.CurBeam + " of " + theProcess.MaxBeam + " (" + theProcess.ProgressPercent.ToString("0.0") + "%)");
                }
                if (curLine.StartsWith("Number of beams"))
                {
                    fields = curLine.Split(separators);
                    theProcess.BeamCount = int.Parse(fields[fields.Length - 1]);
                }
            }
        }
    }
}