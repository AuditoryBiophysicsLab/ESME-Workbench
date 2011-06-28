using System;
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
            NotifyPropertyChanged(BackgroundTasksChangedEventArgs);
            switch (eventArgs.Action)
            {
            case NotifyCollectionChangedAction.Add:
                foreach (var newItem in eventArgs.NewItems)
                {
                    ((BackgroundTask)newItem).StartSemaphore = StartSemaphore;
                    ((BackgroundTask)newItem).ProgressChanged += ProgressHasChanged;
                    ((BackgroundTask)newItem).RunWorkerCompleted += WorkerHasCompleted;
                }
                break;
            }
        }

        void WorkerHasCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            _mutex.WaitOne();
            ((BackgroundTask)sender).ProgressChanged -= ProgressHasChanged;
            ((BackgroundTask)sender).RunWorkerCompleted -= WorkerHasCompleted;
            var activeTasks = BackgroundTasks.Where(b => (!b.IsDone && (b != sender)));
            ActiveTaskMessage = string.Format("Detail ({0} tasks active)", activeTasks.Count());
            if ((activeTasks.Count() == 0) || ((activeTasks.Count() == 1) && (activeTasks.First() == sender))) WaitSemaphore.Release();
            _mutex.ReleaseMutex();
        }

        void ProgressHasChanged(object sender, ProgressChangedEventArgs e)
        {
            _mutex.WaitOne();
            Maximum = BackgroundTasks.Count * 100;
            Value = BackgroundTasks.Sum(b => b.ProgressPercentage);
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

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            Status = "Operation in progress";
            RunState = "Starting tasks";

            foreach (var task in BackgroundTasks)
                task.Start();
            var taskCount = BackgroundTasks.Count;
            ActiveTaskMessage = string.Format("Detail ({0} tasks active)", taskCount);
            StartSemaphore = new Semaphore(0, taskCount);

            StartSemaphore.Release(taskCount);
            RunState = "Running";
            WaitSemaphore.WaitOne();
        }
    }
}