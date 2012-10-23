using System.ComponentModel;
using System.Diagnostics;
using System.Text;
using HRC.Aspects;

namespace ESME.TransmissionLoss
{
    [NotifyPropertyChanged]
    public class TransmissionLossProcess : Process
    {
        public BackgroundWorker BackgroundWorker { get; set; }
        public int BeamCount { get; set; }
        public StringBuilder StringBuilder { get; set; }

        public TransmissionLossProcess() { }

        public TransmissionLossProcess(BackgroundWorker backgroundWorker)
        {
            BackgroundWorker = backgroundWorker;
            if (BackgroundWorker != null) BackgroundWorker.WorkerReportsProgress = true;
        }

        #region public int CurBeam { get; set; }

        public int CurBeam
        {
            get { return _curBeam; }
            set
            {
                _curBeam = value;
                ProgressPercent = (int)((_curBeam / (float)BeamCount) * 95.0f);
            }
        }

        int _curBeam;

        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                _progressPercent = value;
                if (_progressPercent < 0) _progressPercent = 0;
                if (_progressPercent > 100) _progressPercent = 100;
                if ((!HasExited) && (BackgroundWorker != null)) BackgroundWorker.ReportProgress(_progressPercent);
            }
        }

        int _progressPercent;

        #endregion
    }
}