using System.Diagnostics;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    public class TLProcess : Process
    {
        public ImprovedBackgroundWorker BackgroundWorker { get; set; }
        public int BeamCount { get; set; }
        private int _curBeam;
        private int _progressPercent;

        public TLProcess()
        {
        }

        public TLProcess(ImprovedBackgroundWorker backgroundWorker)
        {
            BackgroundWorker = backgroundWorker;
            if (BackgroundWorker != null)
                BackgroundWorker.WorkerReportsProgress = true;
        }

        public int CurBeam
        {
            set
            {
                _curBeam = value;
                ProgressPercent = (int) ((_curBeam/(float) BeamCount)*95.0f);
            }
        }

        public int ProgressPercent
        {
            set
            {
                if (value != _progressPercent)
                {
                    _progressPercent = value;
                    if (_progressPercent < 0)
                        _progressPercent = 0;
                    if (_progressPercent > 100)
                        _progressPercent = 100;
                    if ((!HasExited) && (BackgroundWorker != null))
                        BackgroundWorker.ReportProgress(_progressPercent);
                }
            }
            get { return _progressPercent; }
        }
    }
}