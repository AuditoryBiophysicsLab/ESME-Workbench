using System.ComponentModel;
using Cinch;

namespace ESME.Views.Misc
{
    public class UtilitiesViewModel : ViewModelBase
    {
        IMessageBoxService _messageBoxService;
        public UtilitiesViewModel(IMessageBoxService messageBoxService) { _messageBoxService = messageBoxService; }

        #region RestartCommand
        public SimpleCommand<object, object> RestartCommand
        {
            get { return _restart ?? (_restart = new SimpleCommand<object, object>(delegate { return IsRestartCommandEnabled; }, delegate { RestartHandler(); })); }
        }

        SimpleCommand<object, object> _restart;

        bool IsRestartCommandEnabled
        {
            get { return ResetConfigurationDataRequested || ReimportEnvironmentDataRequested; }
        }

        void RestartHandler()
        {
            string message;
            if (ResetConfigurationDataRequested && !ReimportEnvironmentDataRequested)
                message = "When One Navy Model restarts, you will be required to complete the Configuration Wizard.\r\n\r\nContinue with restart?";
            else if (!ResetConfigurationDataRequested && ReimportEnvironmentDataRequested)
                message = "When One Navy Model restarts, all range complexes will have their environment data caches rebuilt.\r\nThis requires all other users of One Navy Model who are sharing the same range complex data folders to exit One Navy Model and not re-enter the program until the caches are rebuilt. Failure to do this may result in application crashes or invalid data being provided to simulation engines.\r\n\r\nContinue with restart?";
            else message = "When One Navy Model restarts, you will be required to complete the Configuration Wizard, and all range complexes will have their environment data caches rebuilt.\r\nThis requires all other users of One Navy Model who are sharing the same range complex data folders to exit One Navy Model and not re-enter the program until the caches are rebuilt. Failure to do this may result in application crashes or invalid data being provided to simulation engines.\r\n\r\nContinue with restart?";
            var result = _messageBoxService.ShowYesNo(message, CustomDialogIcons.Exclamation);
            if (result == CustomDialogResults.No) return;
        }
        #endregion

        #region public bool ResetConfigurationDataRequested { get; set; }

        public bool ResetConfigurationDataRequested
        {
            get { return _resetConfigurationDataRequested; }
            set
            {
                if (_resetConfigurationDataRequested == value) return;
                _resetConfigurationDataRequested = value;
                NotifyPropertyChanged(ResetConfigurationDataRequestedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResetConfigurationDataRequestedChangedEventArgs = ObservableHelper.CreateArgs<UtilitiesViewModel>(x => x.ResetConfigurationDataRequested);
        bool _resetConfigurationDataRequested;

        #endregion

        #region public bool ReimportEnvironmentDataRequested { get; set; }

        public bool ReimportEnvironmentDataRequested
        {
            get { return _reimportEnvironmentDataRequested; }
            set
            {
                if (_reimportEnvironmentDataRequested == value) return;
                _reimportEnvironmentDataRequested = value;
                NotifyPropertyChanged(ReimportEnvironmentDataRequestedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ReimportEnvironmentDataRequestedChangedEventArgs = ObservableHelper.CreateArgs<UtilitiesViewModel>(x => x.ReimportEnvironmentDataRequested);
        bool _reimportEnvironmentDataRequested;

        #endregion
    }
}
