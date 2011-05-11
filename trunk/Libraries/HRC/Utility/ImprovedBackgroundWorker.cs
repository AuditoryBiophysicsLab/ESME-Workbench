using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Threading;

namespace HRC.Utility
{
    public class ImprovedBackgroundWorker
    {
        Thread _theThread;
        ParameterizedThreadStart _theParams;
        ThreadPriority _thePriority = ThreadPriority.Normal;

        bool _isCancellationPending,
             _isBusy;

        bool _hasCompleted;
        int _progressPercent;

        #region Constructors

        public ImprovedBackgroundWorker()
        {
            _isCancellationPending = false;
            _isBusy = false;
        }

        public ImprovedBackgroundWorker(string taskName, DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog) { Initialize(taskName, doWorkDelegate, runWorkerCompletedDelegate, multiProgressDialog); }

        public ImprovedBackgroundWorker(string taskName, DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog, object runWorkerParameter)
        {
            Initialize(taskName, doWorkDelegate, runWorkerCompletedDelegate, multiProgressDialog);
            RunWorkerAsync(runWorkerParameter);
        }

        public ImprovedBackgroundWorker(string taskName, DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog) { Initialize(taskName, doWorkDelegate, runWorkerCompletedDelegate, multiProgressDialog); }

        public ImprovedBackgroundWorker(string taskName, DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog, object runWorkerParameter)
        {
            Initialize(taskName, doWorkDelegate, runWorkerCompletedDelegate, multiProgressDialog);
            RunWorkerAsync(runWorkerParameter);
        }

        #endregion

        #region Events

        public event DoWorkEventHandler DoWork;

        void OnDoWork(DoWorkEventArgs e)
        {
            if (DoWork != null)
            {
                _theParams = new ParameterizedThreadStart(WorkerThread);
                _theThread = new Thread(_theParams)
                             {
                                 IsBackground = true,
                                 Priority = _thePriority
                             };
                _theThread.Start(e);
            }
        }

        public event ProgressChangedEventHandler ProgressChanged;
        void OnProgressChanged(ProgressChangedEventArgs e)
        {
            if (ProgressChanged != null)
            {
                if (Dispatcher == null) ProgressChanged(this, e);
                else Dispatcher.BeginInvoke(ProgressChanged, this, e);
            }
        }

        public event EventHandler RunWorkerCompleted;

        protected void OnRunWorkerCompleted()
        {
            _hasCompleted = true;
            if (RunWorkerCompleted != null) RunWorkerCompleted(this, new EventArgs());
        }

        public event DoWorkEventHandler RunWorkerCompletedWithArg;

        protected void OnRunWorkerCompletedWithArg(DoWorkEventArgs e)
        {
            _hasCompleted = true;
            if (RunWorkerCompletedWithArg != null) RunWorkerCompletedWithArg(this, e);
        }

        #endregion

        #region Public methods

        public void RunWorkerAsync(Object arg)
        {
            var dw = new DoWorkEventArgs(arg);
            OnDoWork(dw);
        }

        public void CancelAsync()
        {
            if (WorkerSupportsCancellation) _isCancellationPending = true;
            else throw new InvalidOperationException("Worker thread does not support cancellation");
        }

        public void ReportProgress(int percentProgress, object userState = null)
        {
            _progressPercent = percentProgress;
            if (WorkerReportsProgress) OnProgressChanged(new ProgressChangedEventArgs(percentProgress, userState));
            else throw new InvalidOperationException("Worker thread does not support progress reporting");
        }

        #endregion

        #region Public Properties

        public Dispatcher Dispatcher { get; set; }

        public int MinValue { get; set; }
        public int MaxValue { get; set; }

        int _value = 0;
        public int Value
        {
            get { return _value; }
            set
            {
                if (value == _value) return;
                if ((value < MinValue) || (value > MaxValue)) throw new ArgumentOutOfRangeException("Value out of range");
                _value = value;
                ProgressPercent = Value / (MaxValue - MinValue);
            }
        }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set { ReportProgress(value); }
        }

        public ThreadPriority Priority
        {
            get { return _thePriority; }
            set
            {
                if ((_theThread != null) && (_theThread.ThreadState == ThreadState.Running)) _theThread.Priority = value;
                _thePriority = value;
            }
        }

        public string TaskName;

        public bool CancellationPending
        {
            get { return _isCancellationPending; }
        }

        public bool IsBusy
        {
            get { return _isBusy; }
        }

        public bool HasCompleted
        {
            get { return _hasCompleted; }
        }

        public bool WorkerReportsProgress;
        public bool WorkerSupportsCancellation;

        #endregion

        #region Private methods

        void Initialize(string taskName, DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog)
        {
            TaskName = taskName;

            if (doWorkDelegate != null) DoWork += doWorkDelegate;

            if (runWorkerCompletedDelegate != null) RunWorkerCompleted += runWorkerCompletedDelegate;

            if (multiProgressDialog != null) multiProgressDialog.RegisterBackgroundWorker(this);
        }

        void Initialize(string taskName, DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate, MultiProgressDialog multiProgressDialog)
        {
            TaskName = taskName;

            if (doWorkDelegate != null) DoWork += doWorkDelegate;

            if (runWorkerCompletedDelegate != null) RunWorkerCompletedWithArg += runWorkerCompletedDelegate;

            if (multiProgressDialog != null) multiProgressDialog.RegisterBackgroundWorker(this);
        }

        void WorkerThread(Object e)
        {
            _isBusy = true;
            DoWork(this, (DoWorkEventArgs) e);
            OnRunWorkerCompleted();
            OnRunWorkerCompletedWithArg((DoWorkEventArgs) e);
            _isBusy = false;
        }

        #endregion
    }
}