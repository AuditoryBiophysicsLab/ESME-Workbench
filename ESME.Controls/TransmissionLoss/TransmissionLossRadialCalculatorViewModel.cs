using System;
using System.ComponentModel;
using System.IO;
using System.Text;
using Cinch;
using ESME.TransmissionLoss;

namespace ESME.Views.TransmissionLoss
{
    public abstract class TransmissionLossRadialCalculatorViewModel : ViewModelBase
    {
        protected readonly StringBuilder OutputData = new StringBuilder();

        protected TransmissionLossRadialCalculatorViewModel(int radialNumber)
        {
            RadialNumber = radialNumber;
            Status = "Ready";
        }

        protected string CreateTemporaryDirectory()
        {
            var workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(workingDirectory);
            return workingDirectory;
        }

        public event EventHandler CalculationCompleted;
        protected virtual void OnCalculationCompleted()
        {
            if (CalculationCompleted != null) CalculationCompleted(this, new EventArgs());
        }

        #region public double BearingFromSource { get; set; }

        public double BearingFromSource
        {
            get { return _bearingFromSource; }
            set
            {
                if (_bearingFromSource == value) return;
                _bearingFromSource = value;
                NotifyPropertyChanged(BearingFromSourceChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BearingFromSourceChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.BearingFromSource);
        double _bearingFromSource;

        #endregion

        #region public TransmissionLossRadial TransmissionLossRadial { get; set; }

        public TransmissionLossRadial TransmissionLossRadial
        {
            get { return _transmissionLossRadial; }
            set
            {
                if (_transmissionLossRadial == value) return;
                _transmissionLossRadial = value;
                NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRadialChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.TransmissionLossRadial);
        TransmissionLossRadial _transmissionLossRadial;

        #endregion

        #region public int RadialNumber { get; set; }

        public int RadialNumber
        {
            get { return _radialNumber; }
            set
            {
                if (_radialNumber == value) return;
                _radialNumber = value;
                NotifyPropertyChanged(RadialNumberChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RadialNumberChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.RadialNumber);
        int _radialNumber;

        #endregion

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                NotifyPropertyChanged(StatusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.Status);
        string _status;

        #endregion

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent >= value) return;
                _progressPercent = value;
                NotifyPropertyChanged(ProgressPercentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.ProgressPercent);
        int _progressPercent;

        #endregion

        #region public string ErrorText { get; set; }

        public string ErrorText
        {
            get { return _errorText; }
            set
            {
                if (_errorText == value) return;
                _errorText = value;
                NotifyPropertyChanged(ErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ErrorTextChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.ErrorText);
        string _errorText;

        #endregion

        #region public bool CancelRequested { get; set; }

        public bool CancelRequested
        {
            get { return _cancelRequested; }
            set
            {
                if (_cancelRequested == value) return;
                _cancelRequested = value;
                NotifyPropertyChanged(CancelRequestedChangedEventArgs);
                if (!_cancelRequested || (TransmissionLossProcess == null)) return;
                if (!TransmissionLossProcess.HasExited) TransmissionLossProcess.Kill();
                Status = "Canceling";
            }
        }

        static readonly PropertyChangedEventArgs CancelRequestedChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialCalculatorViewModel>(x => x.CancelRequested);
        bool _cancelRequested;

        #endregion

        protected TransmissionLossProcess TransmissionLossProcess;

        public abstract void Start();
    }
}
