using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.ComponentModel;

namespace HRC.Utility
{
    public class ImprovedBackgroundWorker
    {
        private Thread theThread;
        private ParameterizedThreadStart theParams;
        private ThreadPriority thePriority = ThreadPriority.Normal;
        private bool isCancellationPending, isBusy;
        private bool hasCompleted = false;
        private int progressPercent = 0;

        #region Constructors
        public ImprovedBackgroundWorker()
        {
            isCancellationPending = false;
            isBusy = false;
        }

        public ImprovedBackgroundWorker(string TaskName,
                                        DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate,
                                        MultiProgressDialog MultiProgressDialog)
        {
            Initialize(TaskName, doWorkDelegate, runWorkerCompletedDelegate, MultiProgressDialog);
        }

        public ImprovedBackgroundWorker(string TaskName, 
                                        DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate, 
                                        MultiProgressDialog MultiProgressDialog, object RunWorkerParameter)
        {
            Initialize(TaskName, doWorkDelegate, runWorkerCompletedDelegate, MultiProgressDialog);
            this.RunWorkerAsync(RunWorkerParameter);
        }

        public ImprovedBackgroundWorker(string TaskName,
                                        DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate,
                                        MultiProgressDialog MultiProgressDialog)
        {
            Initialize(TaskName, doWorkDelegate, runWorkerCompletedDelegate, MultiProgressDialog);
        }

        public ImprovedBackgroundWorker(string TaskName,
                                        DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate,
                                        MultiProgressDialog MultiProgressDialog, object RunWorkerParameter)
        {
            Initialize(TaskName, doWorkDelegate, runWorkerCompletedDelegate, MultiProgressDialog);
            this.RunWorkerAsync(RunWorkerParameter);
        }
        #endregion

        #region Events
        public event DoWorkEventHandler DoWork;
        protected void OnDoWork(DoWorkEventArgs e)
        {
            if (DoWork != null)
            {
                theParams = new ParameterizedThreadStart(WorkerThread);
                theThread = new Thread(theParams);
                theThread.IsBackground = true;
                theThread.Priority = thePriority;
                theThread.Start(e);
            }
        }

        public event ProgressChangedEventHandler ProgressChanged;
        protected void OnProgressChanged(ProgressChangedEventArgs e)
        {
            if (ProgressChanged != null)
                ProgressChanged(this, e);
        }

        public event EventHandler RunWorkerCompleted;
        protected void OnRunWorkerCompleted()
        {
            hasCompleted = true;
            if (RunWorkerCompleted != null)
                RunWorkerCompleted(this, new EventArgs());
        }

        public event DoWorkEventHandler RunWorkerCompletedWithArg;
        protected void OnRunWorkerCompletedWithArg(DoWorkEventArgs e)
        {
            hasCompleted = true;
            if (RunWorkerCompletedWithArg != null)
                RunWorkerCompletedWithArg(this, e);
        }

        #endregion

        #region Public methods
        public void RunWorkerAsync(Object arg)
        {
            DoWorkEventArgs dw = new DoWorkEventArgs(arg);
            OnDoWork(dw);
        }

        public void CancelAsync()
        {
            if (WorkerSupportsCancellation)
                isCancellationPending = true;
            else
                throw new InvalidOperationException("Worker thread does not support cancellation");
        }

        public void ReportProgress(int percentProgress)
        {
            ReportProgress(percentProgress, null);
        }

        public void ReportProgress(int percentProgress, object userState)
        {
            progressPercent = percentProgress;
            if (WorkerReportsProgress)
                OnProgressChanged(new ProgressChangedEventArgs(percentProgress, userState));
            else
                throw new InvalidOperationException("Worker thread does not support progress reporting");
        }
        #endregion

        #region Public Properties
        public int ProgressPercent
        {
            get { return progressPercent; }
            set { ReportProgress(value); }
        }

        public ThreadPriority Priority 
        { 
            get { return thePriority; } 
            set 
            {
                if ((theThread != null) && (theThread.ThreadState == ThreadState.Running))
                    theThread.Priority = value;
                thePriority = value; 
            } 
        }

        public string TaskName;
        public bool CancellationPending { get { return isCancellationPending; } }
        public bool IsBusy { get { return isBusy; } }
        public bool HasCompleted { get { return hasCompleted; } }
        public bool WorkerReportsProgress = false;
        public bool WorkerSupportsCancellation = false;
        #endregion

        #region Private methods

        private void Initialize(string TaskName,
                                DoWorkEventHandler doWorkDelegate, EventHandler runWorkerCompletedDelegate,
                                MultiProgressDialog MultiProgressDialog)
        {
            this.TaskName = TaskName;

            if (doWorkDelegate != null)
                this.DoWork += doWorkDelegate;

            if (runWorkerCompletedDelegate != null)
                this.RunWorkerCompleted += runWorkerCompletedDelegate;

            if (MultiProgressDialog != null)
                MultiProgressDialog.RegisterBackgroundWorker(this);
        }

        private void Initialize(string TaskName,
                                DoWorkEventHandler doWorkDelegate, DoWorkEventHandler runWorkerCompletedDelegate,
                                MultiProgressDialog MultiProgressDialog)
        {
            this.TaskName = TaskName;

            if (doWorkDelegate != null)
                this.DoWork += doWorkDelegate;

            if (runWorkerCompletedDelegate != null)
                this.RunWorkerCompletedWithArg += runWorkerCompletedDelegate;

            if (MultiProgressDialog != null)
                MultiProgressDialog.RegisterBackgroundWorker(this);
        }

        private void WorkerThread(Object e)
        {
            isBusy = true;
            DoWork(this, (DoWorkEventArgs)e);
            OnRunWorkerCompleted();
            OnRunWorkerCompletedWithArg((DoWorkEventArgs)e);
            isBusy = false;
        }
        #endregion
    }
}
