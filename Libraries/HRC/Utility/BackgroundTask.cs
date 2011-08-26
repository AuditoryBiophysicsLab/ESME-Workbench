using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Input;
using Cinch;

namespace HRC.Utility
{
    public abstract class BackgroundTask : BackgroundWorker, INotifyPropertyChanged
    {
        protected BackgroundTask()
        {
            Minimum = 0;
            Maximum = 100;
            Value = 0;
            WorkerReportsProgress = true;
        }

        public virtual void Start()
        {
            RunState = "Waiting";
            DoWork += Run;
            RunWorkerCompleted += (s, e) => { IsDone = true; };
            RunState = "Starting";
            RunWorkerAsync(this);
        }

        protected abstract void Run(object sender, DoWorkEventArgs e);

        protected override void OnRunWorkerCompleted(RunWorkerCompletedEventArgs e)
        {
            RunState = IsCanceled ? "Canceled" : "Completed";
            Value = IsCanceled ? Value : Maximum;
            if ((e != null) && (e.Error != null)) Console.WriteLine("Got an exception!");
            base.OnRunWorkerCompleted(new RunWorkerCompletedEventArgs(this, e == null ? null : e.Error, IsCanceled));
        }

        protected override void OnProgressChanged(ProgressChangedEventArgs e)
        {
            ProgressPercentage = e.ProgressPercentage;
            RunState = string.Format("{0}% complete", ProgressPercentage);
            base.OnProgressChanged(e);
        }

        #region public bool IsDone { get; set; }

        public bool IsDone
        {
            get { return _isDone; }
            protected set
            {
                if (_isDone == value) return;
                _isDone = value;
                NotifyPropertyChanged(IsDoneChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDoneChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.IsDone);
        bool _isDone;

        #endregion

        #region public Semaphore WaitSemaphore { get; set; }

        /// <summary>
        /// Subclasses should use this semaphore to wait for dependencies.
        /// If the design of the subclass requires more than one semaphore, define it in the subclass
        /// </summary>
        public Semaphore WaitSemaphore
        {
            get { return _waitSemaphore; }
            set
            {
                if (_waitSemaphore == value) return;
                _waitSemaphore = value;
                NotifyPropertyChanged(WaitSemaphoreChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WaitSemaphoreChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.WaitSemaphore);
        Semaphore _waitSemaphore = new Semaphore(0, 1);

        #endregion

        #region public int ProgressPercentage { get; set; }

        public int ProgressPercentage
        {
            get { return _progressPercentage; }
            set
            {
                if (_progressPercentage == value) return;
                _progressPercentage = value;
                NotifyPropertyChanged(ProgressPercentChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.ProgressPercentage);
        int _progressPercentage;

        #endregion

        #region public string TaskName { get; set; }

        public string TaskName
        {
            get { return _taskName; }
            set
            {
                if (_taskName == value) return;
                _taskName = value;
                NotifyPropertyChanged(TaskNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TaskNameChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.TaskName);
        string _taskName;

        #endregion

        #region public int Minimum { get; set; }

        public int Minimum
        {
            get { return _minimum; }
            set
            {
                if (_minimum == value) return;
                _minimum = value;
                NotifyPropertyChanged(MinValueChangedEventArgs);
                _range = Maximum - Minimum;
            }
        }

        static readonly PropertyChangedEventArgs MinValueChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.Minimum);
        int _minimum;

        #endregion

        #region public int Maximum { get; set; }

        public int Maximum
        {
            get { return _maximum; }
            set
            {
                if (_maximum == value) return;
                _maximum = value;
                NotifyPropertyChanged(MaxValueChangedEventArgs);
                _range = Maximum - Minimum;
            }
        }

        static readonly PropertyChangedEventArgs MaxValueChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.Maximum);
        int _maximum;

        #endregion

        #region public int Value { get; set; }

        public int Value
        {
            get { return _value; }
            set
            {
                if (_value == value) return;
                if (value < Minimum) throw new ArgumentOutOfRangeException("Value", string.Format("Value {0} must be between {1} and {2}", value, Minimum, Maximum));
                if (value < Maximum) _value = value;
                if ((WorkerReportsProgress) && (_range > 0f) && IsBusy)
                    try
                    {
                        ReportProgress((int)((_value / _range) * 100f));
                    }
                    catch (InvalidOperationException) {}
                NotifyPropertyChanged(ValueChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValueChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.Value);
        int _value;
        float _range = 1f;

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

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.Status);
        string _status;

        #endregion

        #region public bool IsCanceled { get; set; }

        public bool IsCanceled
        {
            get { return _isCanceled; }
            set
            {
                if (_isCanceled == value) return;
                _isCanceled = value;
                NotifyPropertyChanged(IsCanceledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsCanceledChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.IsCanceled);
        bool _isCanceled;

        #endregion

        #region public string RunState { get; set; }

        public string RunState
        {
            get { return _runState; }
            set
            {
                if (_runState == value) return;
                _runState = value;
                NotifyPropertyChanged(RunStateChangedEventArgs);
                NotifyPropertyChanged(IsBusyChangedEventArgs);
                CommandManager.InvalidateRequerySuggested();
            }
        }

        static readonly PropertyChangedEventArgs IsBusyChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.IsBusy);
        static readonly PropertyChangedEventArgs RunStateChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTask>(x => x.RunState);
        string _runState;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                                                                                {
                                                                                    return WorkerSupportsCancellation;
                                                                                },
                                                                                delegate
                                                                                {
                                                                                    RunState = "Canceling";
                                                                                    CancelAsync();
                                                                                    IsCanceled = true;
                                                                                }));
            }
        }

        SimpleCommand<object, object> _cancel;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }
}