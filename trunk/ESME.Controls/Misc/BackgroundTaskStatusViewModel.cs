using System.ComponentModel;
using System.Windows;
using Cinch;
using HRC.Utility;

namespace ESME.Views.Misc
{
    public class BackgroundTaskStatusViewModel : ViewModelBase
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
                if (_backgroundTaskAggregator != null) _backgroundTaskAggregator.PropertyChanged += (s, e) =>
                {
                    if (e.PropertyName == "IsRunning")
                        if (!_backgroundTaskAggregator.IsRunning) CloseActivePopUpCommand.Execute(false);
                };
            }
        }

        static readonly PropertyChangedEventArgs BackgroundTaskAggregatorChangedEventArgs = ObservableHelper.CreateArgs<BackgroundTaskStatusViewModel>(x => x.BackgroundTaskAggregator);
        BackgroundTaskAggregator _backgroundTaskAggregator;

        #endregion

        #region public double MaxHeight { get; set; }

        public double MaxHeight
        {
            get { return SystemParameters.WorkArea.Height; }
        }

        #endregion

        #region ViewActivatedCommand
        public SimpleCommand<object, object> ViewActivatedCommand
        {
            get
            {
                return _viewActivated ?? (_viewActivated = new SimpleCommand<object, object>(obj => NotifyPropertyChanged(BackgroundTaskAggregatorChangedEventArgs)));
            }
        }

        SimpleCommand<object, object> _viewActivated;

        #endregion
    }
}
