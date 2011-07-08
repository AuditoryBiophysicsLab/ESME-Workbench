using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using HRC.Utility;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        #region public BackgroundTaskAggregator BackgroundTaskAggregator { get; set; }

        public BackgroundTaskAggregator BackgroundTaskAggregator
        {
            get { return _backgroundTaskAggregator ?? (_backgroundTaskAggregator = new BackgroundTaskAggregator()); }
            set
            {
                if (_backgroundTaskAggregator == value) return;
                _backgroundTaskAggregator = value;
                NotifyPropertyChanged(BackgroundTaskAggregatorChangedEventArgs);
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
            get { return BackgroundTaskAggregator.IsBusy; }
        }

        void ShowDetailedProgressHandler() { }
        #endregion
    }
}
