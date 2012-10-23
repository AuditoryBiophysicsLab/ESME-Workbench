using System;
using System.Windows;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Threading;
using HRC.Utility;
using ESME.TransmissionLoss;
using System.ComponentModel;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using ESME.TransmissionLoss.Bellhop;
using System.Windows.Controls;

// This file is intended to be edited manually

namespace ESME.TransmissionLoss
{
    partial class TLDataContext
    {

        // Place your implementation of partial extension methods here
    }

    partial class Field
    {
        private float[] mRangeAxis = null;
        private float[] mDepthAxis = null;
        public FieldData FieldData = null;
        public float mProgress_percent = 0f;
        public float TotalProgress_percent { get { return mProgress_percent; } }
        public bool IsComputing { get; private set; }
        public string ActiveTag
        {
            get
            {
                if (IsComputing)
                    return "Items currently being processed";
                else
                    return "Items waiting to be processed";
            }
        }
        public TreeViewItem TreeViewItem { get; set; }

        public float[] Ranges_meters
        {
            get
            {
                if (mRangeAxis == null)
                {
                    if (RangeAxisBytes == null)
                        return null;
                    MemoryStream stream = new MemoryStream(RangeAxisBytes);
                    BinaryReader reader = new BinaryReader(stream);
                    IFormatter formatter = new BinaryFormatter();
                    mRangeAxis = (float[])formatter.Deserialize(stream);
                    reader.Close();
                    stream.Close();
                }
                return mRangeAxis;
            }
            set
            {
                mRangeAxis = new float[value.Length];
                Array.Copy(value, mRangeAxis, value.Length);
                MemoryStream stream = new MemoryStream();
                BinaryWriter writer = new BinaryWriter(stream);
                IFormatter formatter = new BinaryFormatter();
                formatter.Serialize(stream, mRangeAxis);
                writer.Close();
                RangeAxisBytes = stream.ToArray();
                stream.Close();
            }
        }

        public float[] Depths_meters
        {
            get
            {
                if (mDepthAxis == null)
                {
                    if (DepthAxisBytes == null)
                        return null;
                    MemoryStream stream = new MemoryStream(DepthAxisBytes);
                    BinaryReader reader = new BinaryReader(stream);
                    IFormatter formatter = new BinaryFormatter();
                    mDepthAxis = (float[])formatter.Deserialize(stream);
                    reader.Close();
                    stream.Close();
                }
                return mDepthAxis;
            }
            set
            {
                mDepthAxis = new float[value.Length];
                Array.Copy(value, mDepthAxis, value.Length);
                MemoryStream stream = new MemoryStream();
                BinaryWriter writer = new BinaryWriter(stream);
                IFormatter formatter = new BinaryFormatter();
                formatter.Serialize(stream, mDepthAxis);
                writer.Close();
                DepthAxisBytes = stream.ToArray();
                stream.Close();
            }
        }

        public void Compute(object sender, DoWorkEventArgs e)
        {
            int CurRadial;
            bool CancellationPending = false;
            ImprovedBackgroundWorker MainWorker = (ImprovedBackgroundWorker)sender;
            ImprovedBackgroundWorker[] RadialWorkers = new ImprovedBackgroundWorker[Radials.Count];
            MainWorker.WorkerReportsProgress = true;
            MainWorker.WorkerSupportsCancellation = true;

            IsComputing = true;
            BinaryFileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + ".tlf";

            for (CurRadial = 0; CurRadial < Radials.Count; CurRadial++)
                RadialWorkers[CurRadial] = new ImprovedBackgroundWorker("radial " + CurRadial, 
                    new DoWorkEventHandler(Radials[CurRadial].Compute),
                    new DoWorkEventHandler(Radials[CurRadial].Complete), 
                    null);
            for (CurRadial = 0; CurRadial < Radials.Count; CurRadial++)
            {
                // This loop will launch all the threads needed to compute the radials for this TL Field.
                // The maximum number of threads that will run at one time is limited by the number of processors on
                // the host system.
                CancellationPending = TrackProgress(MainWorker, RadialWorkers, System.Environment.ProcessorCount);

                if (CancellationPending)
                    break;

                RadialWorkers[CurRadial].Priority = ThreadPriority.BelowNormal;
                RadialWorkers[CurRadial].RunWorkerAsync(null);
                Thread.Sleep(1000);
            }
            
            if (!CancellationPending)
                CancellationPending = TrackProgress(MainWorker, RadialWorkers, 1);

            if ((CancellationPending) && (FieldData != null) && (File.Exists(FieldData.Filename)))
            {
                File.Delete(FieldData.Filename);
                FieldData = null;
            }
        }

        private bool TrackProgress(ImprovedBackgroundWorker MainThread, ImprovedBackgroundWorker[] RadialThreads, int MinimumBusyWorkerCountBeforeReturn)
        {
            int RunningWorkerCount = RadialThreads.Length;
            float TotalProgress, RadialProgress;
            bool CancellationPending = false;
            float RadialProgressDivisor = 1f / (float)RadialThreads.Length;

            if (MinimumBusyWorkerCountBeforeReturn > RadialThreads.Length)
                throw new ArgumentException("Radial: TrackProgress: Infinite loop requested.  Check your parameters!");

            while (RunningWorkerCount >= MinimumBusyWorkerCountBeforeReturn)
            {
                TotalProgress = 0;
                RunningWorkerCount = 0;
                for (int CurWorker = 0; CurWorker < RadialThreads.Length; CurWorker++)
                {
                    RadialProgress = 0f;
                    if (MainThread.CancellationPending)
                    {
                        CancellationPending = true;
                        if (RadialThreads[CurWorker].IsBusy) RadialThreads[CurWorker].CancelAsync();
                    }
                    if (RadialThreads[CurWorker].IsBusy)
                    {
                        RunningWorkerCount++;
                        RadialProgress = RadialThreads[CurWorker].ProgressPercent * RadialProgressDivisor;
                    }
                    else
                    {
                        if (RadialThreads[CurWorker].HasCompleted)
                        {
                            RadialProgress = 100 * RadialProgressDivisor;
                        }
                        else
                            RadialProgress = 0;
                    }
                    TotalProgress += RadialProgress;
                    mProgress_percent = TotalProgress;
                }
                //System.Diagnostics.Debug.WriteLine(RunningWorkerCount + " worker threads still running");
                MainThread.ReportProgress((int)TotalProgress);
                if (RunningWorkerCount >= MinimumBusyWorkerCountBeforeReturn)
                    Thread.Sleep(500);
            }
            return CancellationPending;
        }

        public void Complete(object sender, DoWorkEventArgs e)
        {
            ImprovedBackgroundWorker MainWorker = (ImprovedBackgroundWorker)sender;
            IsComputing = false;
            if (!MainWorker.CancellationPending)
            {
                IsCalculated = true;
                MainWorker.ReportProgress((int)100);
                mProgress_percent = 100;
            }
        }

        public void AddRadialData(RadialData RadialData)
        {
            if (RadialData == null)
                return;
            if (((Depths_meters == null) || (Ranges_meters == null)) && 
                ((RadialData.Depths_meters != null) && (RadialData.Ranges_meters != null)))
            {
                Depths_meters = RadialData.Depths_meters;
                Ranges_meters = RadialData.Ranges_meters;
            }
            else
            {
                if ((Depths_meters != null) && (RadialData.Depths_meters != null))
                    ValidateDepths(RadialData.Depths_meters);
                if ((Ranges_meters != null) && (RadialData.Ranges_meters != null))
                    ValidateRanges(RadialData.Ranges_meters);
            }

            if (FieldData == null)
            {
                FieldData = new FieldData(this);
                FieldData.Filename = Path.Combine(DataDirectoryPath, BinaryFileName);
            }

            RadialData.ClearAxisData();
            lock (FieldData)
            {
                FieldData.AddRadial(RadialData);
                FieldData.Save(true);
            }
        }

        private void ValidateDepths(float[] NewRadialDepthAxis)
        {
            if (!CompareAxes(Depths_meters, NewRadialDepthAxis))
                throw new ApplicationException("Attempt to add a computed radial where the depth axis does not match the field's depth axis");
        }

        private void ValidateRanges(float[] NewRadialRangeAxis)
        {
            if (!CompareAxes(Ranges_meters, NewRadialRangeAxis))
                throw new ApplicationException("Attempt to add a computed radial where the depth axis does not match the field's depth axis");
        }

        private bool CompareAxes(float[] MyAxis, float[] NewAxis)
        {
            if (MyAxis.Length != NewAxis.Length)
                return false;
            for (int i = 0; i < MyAxis.Length; i++)
                if (MyAxis[i] != NewAxis[i])
                    return false;
            return true;
        }

        public Radial RadialAtBearing(float Bearing_degrees)
        {
            foreach (Radial r in Radials)
                if (r.BearingFromSource_degrees == Bearing_degrees)
                    return r;
            return null;
        }
    }

    public partial class Radial
    {
        private StringBuilder mBellhopOutputData;
        public RadialData RadialData {get; set;}
        public bool IsComputing { get; private set; }
        public TreeViewItem TreeViewItem { get; set; }
        float mProgress_percent = 0f;
        public float Progress_percent { get { return mProgress_percent; } }

        public void Complete(object sender, DoWorkEventArgs e)
        {
            ImprovedBackgroundWorker bw;

            if ((sender == null) || (e == null))
                return;
            bw = (ImprovedBackgroundWorker)sender;
            IsComputing = false;
            if (!bw.CancellationPending)
            {
                CalculationFinished = DateTime.Now;
                IsCalculated = true;
                this.Field.AddRadialData(RadialData);
                mProgress_percent = 100;
            }
        }

        public void Compute(object sender, DoWorkEventArgs e)
        {
            ImprovedBackgroundWorker bw;
            TLProcess BellhopProcess;
            string ErrorOutput, WorkingDirectory;

            IsComputing = true;
            CalculationStarted = DateTime.Now;
            if ((sender == null))
                return;
            bw = (ImprovedBackgroundWorker)sender;
            //System.Diagnostics.Debug.WriteLine("Worker: Beginning computation of " + bw.TaskName);

            bw.WorkerReportsProgress = true;
            bw.WorkerSupportsCancellation = true;

            WorkingDirectory = Path.Combine(Path.GetTempPath(),
                Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));

            Directory.CreateDirectory(WorkingDirectory);

            // Write the bottom profile file that will be read by BELLHOP
            using (StreamWriter writer = new StreamWriter(Path.Combine(WorkingDirectory, "BTYFIL")))
                writer.Write(BottomProfile);

            // Create a process, run BELLHOP, and exit
            BellhopProcess = new TLProcess(bw);
            BellhopProcess.StartInfo = new ProcessStartInfo(
                Path.Combine(Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location), "Bellhop.exe"));

            BellhopProcess.StartInfo.CreateNoWindow = true;
            BellhopProcess.StartInfo.UseShellExecute = false;
            BellhopProcess.StartInfo.RedirectStandardInput = true;
            BellhopProcess.StartInfo.RedirectStandardOutput = true;
            BellhopProcess.StartInfo.RedirectStandardError = true;
            BellhopProcess.StartInfo.WorkingDirectory = WorkingDirectory;
            BellhopProcess.OutputDataReceived += new DataReceivedEventHandler(OutputDataRecieved);
            mBellhopOutputData = new StringBuilder();
            BellhopProcess.Start();
            BellhopProcess.PriorityClass = ProcessPriorityClass.BelowNormal;
            BellhopProcess.StandardInput.WriteLine(BellhopConfiguration);
            BellhopProcess.BeginOutputReadLine();
            while (!BellhopProcess.HasExited)
            {
                Thread.Sleep(100);
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
                mProgress_percent = BellhopProcess.ProgressPercent;
            }
            mProgress_percent = BellhopProcess.ProgressPercent;
            ErrorOutput = BellhopProcess.StandardError.ReadToEnd();
            //if (BellhopOutputData.ToString() != "")
            //    MessageBox.Show(BellhopOutputData.ToString());
            //System.Diagnostics.Debug.WriteLine("BELLHOP Exit Code: " + BellhopProcess.ExitCode);

            // We don't need to keep the results files around anymore, we're finished with them
            File.Delete(Path.Combine(WorkingDirectory, "BTYFIL"));
            foreach (string s in Directory.GetFiles(WorkingDirectory, "ARRFIL_*"))
                File.Delete(s);
            //if (File.Exists(Path.Combine(WorkingDirectory, "ARRFIL")))
            //    File.Move(Path.Combine(WorkingDirectory, "ARRFIL"), "ARRFIL_" + TLParams.RadialNumber.ToString("00"));

            // TODO: Convert the Bellhop output file into a Radial binary file
            RadialData = new RadialData(BearingFromSource_degrees, 
                new BellhopOutput(Path.Combine(WorkingDirectory, "SHDFIL")));

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
            File.Delete(Path.Combine(WorkingDirectory, "SHDFIL"));
            bw.ReportProgress(100);
            Directory.Delete(WorkingDirectory, true);
            //System.Diagnostics.Debug.WriteLine("Worker: Completed computation of " + bw.TaskName);
        }

        private void OutputDataRecieved(object sendingProcess, DataReceivedEventArgs outLine)
        {
            TLProcess theProcess = (TLProcess)sendingProcess;
            string CurLine;
            string[] Fields;
            char[] Separators = { ' ', '=' };

            // Collect the sort command output.
            if (!String.IsNullOrEmpty(outLine.Data))
            {
                // Add the text to the collected output.
                mBellhopOutputData.Append(outLine.Data);
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