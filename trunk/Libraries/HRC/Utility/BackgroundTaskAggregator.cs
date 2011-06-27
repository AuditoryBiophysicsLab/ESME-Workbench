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
                    ((BackgroundTask)newItem).ProgressChanged += ProgressHasChanged;
                    ((BackgroundTask)newItem).RunWorkerCompleted += WorkerHasCompleted;
                }
                break;
            }
        }

        void WorkerHasCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            ((BackgroundTask)sender).ProgressChanged -= ProgressHasChanged;
            ((BackgroundTask)sender).RunWorkerCompleted -= WorkerHasCompleted;
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

        protected override void OnRunWorkerCompleted(RunWorkerCompletedEventArgs e)
        {
            BackgroundTasks.Clear();
            base.OnRunWorkerCompleted(e);
        }

        readonly Mutex _mutex = new Mutex(true);

        protected override void Run(object sender, DoWorkEventArgs e)
        {
            Status = "Operation in progress";
            RunState = "Running";
            foreach (var task in BackgroundTasks)
                task.Run();
            Thread.Sleep(1000); // to give the aggregated workers time to start
            _mutex.WaitOne();
        }
    }
}