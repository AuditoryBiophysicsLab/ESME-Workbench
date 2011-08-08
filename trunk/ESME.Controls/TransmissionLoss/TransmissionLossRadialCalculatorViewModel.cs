using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;

namespace ESME.Views.TransmissionLoss
{
    public abstract class TransmissionLossRadialCalculatorViewModel : ViewModelBase
    {
        protected readonly StringBuilder OutputData = new StringBuilder();

        protected TransmissionLossRadialCalculatorViewModel(int radialNumber, Dispatcher dispatcher)
        {
            Dispatcher = dispatcher;
            RadialNumber = radialNumber;
            Status = "Queued";
        }

        protected string CreateTemporaryDirectory()
        {
            var workingDirectory = Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension(Path.GetRandomFileName()));
            Directory.CreateDirectory(workingDirectory);
            return workingDirectory;
        }

        public Dispatcher Dispatcher { get; set; }

        #region public double BearingFromSource { get; set; }

        public double BearingFromSource
        {
            get { return _bearingFromSource; }
            set
            {
                if (_bearingFromSource == value) return;
                _bearingFromSource = value;
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(BearingFromSourceChangedEventArgs));
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
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs));
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
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(RadialNumberChangedEventArgs));
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
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(StatusChangedEventArgs));
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
                if (_progressPercent == value) return;
                _progressPercent = value;
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ProgressPercentChangedEventArgs));
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
                if (Dispatcher != null) Dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ErrorTextChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs ErrorTextChangedEventArgs = ObservableHelper.CreateArgs<BellhopRadialCalculatorViewModel>(x => x.ErrorText);
        string _errorText;

        #endregion

        public abstract void Start();
    }
}
