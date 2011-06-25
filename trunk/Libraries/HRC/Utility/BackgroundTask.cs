using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
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

        public virtual void Run()
        {
            RunState = "Starting";
            DoWork += Run;
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
            base.OnProgressChanged(e);
        }

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
                if ((value < Minimum) || (value > Maximum)) throw new ArgumentOutOfRangeException("Value", string.Format("Value {0} must be between {1} and {2}", value, Minimum, Maximum));
                _value = value;
                if ((WorkerReportsProgress) && (_range > 0f) && IsBusy) ReportProgress((int)((_value / _range) * 100f));
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

    public class BackgroundTaskAggregator : BackgroundTask
    {
        public BackgroundTaskAggregator() { BackgroundTasks = new ObservableCollection<BackgroundTask>(); }

        #region public ObservableCollection<BackgroundTask> BackgroundTasks { get; set; }

        public ObservableCollection<BackgroundTask> BackgroundTasks
        {
            get { return _backgroundTasks; }
            private set
            {
                if (_backgroundTasks == value) return;
                if (_backgroundTasks != null) _backgroundTasks.CollectionChanged -= BackgroundTasksCollectionChanged;
                _backgroundTasks = value;
                if (_backgroundTasks != null) _backgroundTasks.CollectionChanged += BackgroundTasksCollectionChanged;
                NotifyPropertyChanged(BackgroundTasksChangedEventArgs);
            }
        }

        void BackgroundTasksCollectionChanged(object sender, NotifyCollectionChangedEventArgs eventArgs)
        {
            NotifyPropertyChanged(BackgroundTasksChangedEventArgs);
            switch (eventArgs.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    foreach (var newItem in eventArgs.NewItems)
                    {
                        ((BackgroundTask)newItem).ProgressChanged += ProgressHasChanged;
                        ((BackgroundTask)newItem).RunWorkerCompleted += WorkerHasCompleted;
                    }
                    break;
            }
        }

        void WorkerHasCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            var activeTasks = BackgroundTasks.Where(b => b.IsBusy);
            if (activeTasks.Count() == 0) _mutex.ReleaseMutex();
        }

        void ProgressHasChanged(object sender, ProgressChangedEventArgs e)
        {
            Maximum = BackgroundTasks.Count * 100;
            Value = BackgroundTasks.Sum(b => b.ProgressPercentage);
        }

        static readonly PropertyChangedEventArgs BackgroundTasksChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskAggregator>(x => x.BackgroundTasks);
        ObservableCollection<BackgroundTask> _backgroundTasks;

        #endregion

        readonly Mutex _mutex = new Mutex(true);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            var backgroundTask = (BackgroundTask)sender;
            RunState = "Running";
            Thread.Sleep(1000); // to give the aggregated workers time to start
            _mutex.WaitOne();
        }
    }
}