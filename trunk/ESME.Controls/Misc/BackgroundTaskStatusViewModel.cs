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
                if (_backgroundTaskAggregator != null) _backgroundTaskAggregator.RunWorkerCompleted += (s, e) => CloseActivePopUpCommand.Execute(true);
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

    }
}
