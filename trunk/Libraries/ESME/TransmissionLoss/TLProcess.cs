using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    internal class TLProcess : Process
    {
        public int BeamCount;
        private int progressPercent, curBeam;
        public ImprovedBackgroundWorker BackgroundWorker;

        public TLProcess(ImprovedBackgroundWorker BackgroundWorker)
        {
            this.BackgroundWorker = BackgroundWorker;
            BackgroundWorker.WorkerReportsProgress = true;
        }

        public int CurBeam
        {
            set
            {
                curBeam = value;
                ProgressPercent = (int)(((float)curBeam / (float)BeamCount) * 95.0f);
            }
        }

        public int ProgressPercent
        {
            set
            {
                if (value != progressPercent)
                {
                    progressPercent = value;
                    if (progressPercent < 0)
                        progressPercent = 0;
                    if (progressPercent > 100)
                        progressPercent = 100;
                    if (!this.HasExited)
                        BackgroundWorker.ReportProgress(progressPercent);
                }
            }
            get { return progressPercent; }
        }
    }
}
