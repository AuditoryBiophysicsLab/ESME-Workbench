using System.ComponentModel;
using System.Windows;
using Cinch;
using HRC.Utility;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Interaction logic for BackgroundTaskProgress.xaml
    /// </summary>
    public partial class BackgroundTaskProgress : INotifyPropertyChanged
    {
        public BackgroundTaskProgress()
        {
            InitializeComponent();
        }

        #region dependency property BackgroundTask BackgroundTask

        public static DependencyProperty BackgroundTaskProperty = DependencyProperty.Register("BackgroundTask", typeof (BackgroundTask), typeof (BackgroundTaskProgress),
                                                                                 new FrameworkPropertyMetadata(null));

        public BackgroundTask BackgroundTask
        {
            get { return (BackgroundTask)GetValue(BackgroundTaskProperty); }
            set { SetValue(BackgroundTaskProperty, value); }
        }

        #endregion

        #region CancelCommand

        SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate { return BackgroundTask.WorkerSupportsCancellation; }, delegate { BackgroundTask.CancelAsync(); }));} }

        SimpleCommand<object, object> _cancel;

        #endregion
        
        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs args) { if (PropertyChanged != null) PropertyChanged(this, args); }

        #endregion
    }
}
