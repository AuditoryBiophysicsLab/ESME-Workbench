using System.Diagnostics;
using System.Text;
using HRC.Utility;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossProcess : Process
    {
        public ImprovedBackgroundWorker BackgroundWorker { get; set; }
        public int BeamCount { get; set; }
        public StringBuilder StringBuilder { get; set; }

        public TransmissionLossProcess() { }

        public TransmissionLossProcess(ImprovedBackgroundWorker backgroundWorker)
        {
            BackgroundWorker = backgroundWorker;
            if (BackgroundWorker != null) BackgroundWorker.WorkerReportsProgress = true;
        }

        public int CurBeam
        {
            set
            {
                _curBeam = value;
                ProgressPercent = (int) ((_curBeam/(float) BeamCount)*95.0f);
            }
        }
        int _curBeam;

        public int ProgressPercent
        {
            set
            {
                if (value == _progressPercent) return;
                _progressPercent = value;
                if (_progressPercent < 0) _progressPercent = 0;
                if (_progressPercent > 100) _progressPercent = 100;
                if ((!HasExited) && (BackgroundWorker != null)) BackgroundWorker.ReportProgress(_progressPercent);
            }
            get { return _progressPercent; }
        }
        int _progressPercent;
    }
}