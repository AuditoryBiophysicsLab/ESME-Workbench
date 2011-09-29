#if false
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Threading;
using Cinch;

namespace HRC.Utility
{
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
            _mutex.WaitOne();
            NotifyPropertyChanged(BackgroundTasksChangedEventArgs);
            switch (eventArgs.Action)
            {
            case NotifyCollectionChangedAction.Add:
                foreach (var newItem in eventArgs.NewItems)
                {
                    ((BackgroundTask)newItem).ProgressChanged += ProgressHasChanged;
                    ((BackgroundTask)newItem).RunWorkerCompleted += WorkerHasCompleted;
                    ((BackgroundTask)newItem).Start();
                }
                break;
            case NotifyCollectionChangedAction.Remove:
                foreach (var oldItem in eventArgs.OldItems)
                {
                    ((BackgroundTask)oldItem).ProgressChanged -= ProgressHasChanged;
                    ((BackgroundTask)oldItem).RunWorkerCompleted -= WorkerHasCompleted;
                }
                break;
            }
            if (_isIdle)
            {
                IsIdle = false;
                RunState = "Running";
            }
            _mutex.ReleaseMutex();
        }

        int _completedTaskCount = 0;

        void WorkerHasCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            _mutex.WaitOne();
            BackgroundTasks.Remove((BackgroundTask)sender);
            _completedTaskCount++;
            var activeTasks = BackgroundTasks.Count;
            ActiveTaskMessage = string.Format("Detail ({0} tasks active)", activeTasks);
            if (activeTasks == 0)
            {
                IsIdle = true;
                Status = "";
                RunState = "Idle";
                _completedTaskCount = 0;
            }
            _mutex.ReleaseMutex();
        }

        void ProgressHasChanged(object sender, ProgressChangedEventArgs e)
        {
            _mutex.WaitOne();
            var activeTaskCount = BackgroundTasks.Count;
            Maximum = (activeTaskCount * 100) + (_completedTaskCount * 100);
            ActiveTaskMessage = string.Format("Detail ({0} tasks active)", activeTaskCount);
            Value = BackgroundTasks.Sum(b => b.ProgressPercentage) + (_completedTaskCount * 100);
            Status = ProgressPercentage + "% complete";
            _mutex.ReleaseMutex();
        }

        static readonly PropertyChangedEventArgs BackgroundTasksChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskAggregator>(x => x.BackgroundTasks);
        ObservableCollection<BackgroundTask> _backgroundTasks;

        #endregion

        #region public string ActiveTaskMessage { get; set; }

        public string ActiveTaskMessage
        {
            get { return _activeTaskMessage; }
            set
            {
                if (_activeTaskMessage == value) return;
                _activeTaskMessage = value;
                NotifyPropertyChanged(ActiveTaskMessageChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ActiveTaskMessageChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskAggregator>(x => x.ActiveTaskMessage);
        string _activeTaskMessage;

        #endregion

        protected override void OnRunWorkerCompleted(RunWorkerCompletedEventArgs e)
        {
            BackgroundTasks.Clear();
            base.OnRunWorkerCompleted(e);
        }

        readonly Mutex _mutex = new Mutex();

        #region public bool IsIdle { get; set; }

        public bool IsIdle
        {
            get { return _isIdle; }
            set
            {
                if (_isIdle == value) return;
                _isIdle = value;
                NotifyPropertyChanged(IsIdleChangedEventArgs);
                IsRunning = !IsIdle;
            }
        }

        static readonly PropertyChangedEventArgs IsIdleChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskAggregator>(x => x.IsIdle);
        bool _isIdle = true;

        #endregion

        #region public bool IsRunning { get; set; }

        public bool IsRunning
        {
            get { return _isRunning; }
            set
            {
                if (_isRunning == value) return;
                _isRunning = value;
                NotifyPropertyChanged(IsBusyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsBusyChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskAggregator>(x => x.IsRunning);
        bool _isRunning;

        #endregion


        protected override void Run(object sender, DoWorkEventArgs e)
        {
            while (true)
            {
                Thread.Sleep(100);
                if (CancellationPending) break;
            }
        }
    }
}
#endif
