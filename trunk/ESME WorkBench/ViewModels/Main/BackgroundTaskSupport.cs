using System.ComponentModel;
using Cinch;
using ESME.Views.Misc;
using HRC.Utility;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region public BackgroundTaskAggregator BackgroundTaskAggregator { get; set; }

        public BackgroundTaskAggregator BackgroundTaskAggregator
        {
            get { return _backgroundTaskAggregator; }
            set
            {
                if (_backgroundTaskAggregator == value) return;
                _backgroundTaskAggregator = value;
                NotifyPropertyChanged(BackgroundTaskAggregatorChangedEventArgs);
                if (_backgroundTaskAggregator != null) _backgroundTaskAggregator.PropertyChanged += (s, e) => NotifyPropertyChanged(BackgroundTaskAggregatorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BackgroundTaskAggregatorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.BackgroundTaskAggregator);
        BackgroundTaskAggregator _backgroundTaskAggregator;

        #endregion

        #region ShowDetailedProgressCommand
        public SimpleCommand<object, object> ShowDetailedProgressCommand
        {
            get { return _showDetailedProgress ?? (_showDetailedProgress = new SimpleCommand<object, object>(delegate { return IsShowDetailedProgressCommandEnabled; }, delegate { ShowDetailedProgressHandler(); })); }
        }

        SimpleCommand<object, object> _showDetailedProgress;

        bool IsShowDetailedProgressCommandEnabled
        {
            get { return BackgroundTaskAggregator.IsRunning; }
        }

        void ShowDetailedProgressHandler()
        {
            var vm = new BackgroundTaskStatusViewModel
            {
                BackgroundTaskAggregator = BackgroundTaskAggregator
            };
            var result = _visualizerService.ShowDialog("BackgroundTaskStatusView", vm);
            if ((result.HasValue) && (result.Value))
            {
                
            }
        }
        #endregion
    }
}
