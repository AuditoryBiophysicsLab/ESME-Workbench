using System.ComponentModel;
using System.Diagnostics;
using System.Text;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossProcess : Process, INotifyPropertyChanged
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
                if (_curBeam == value) return;
                _curBeam = value;
                ProgressPercent = (int)((_curBeam / (float)BeamCount) * 95.0f);
                NotifyPropertyChanged(CurBeamChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CurBeamChangedEventArgs = new PropertyChangedEventArgs("CurBeam");
        int _curBeam;

        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent == value) return;
                _progressPercent = value;
                if (_progressPercent < 0) _progressPercent = 0;
                if (_progressPercent > 100) _progressPercent = 100;
                if ((!HasExited) && (BackgroundWorker != null)) BackgroundWorker.ReportProgress(_progressPercent);
                NotifyPropertyChanged(ProgressPercentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = new PropertyChangedEventArgs("ProgressPercent");
        int _progressPercent;

        #endregion

        public event PropertyChangedEventHandler PropertyChanged;

        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }
    }
}