using System.Windows;
using HRC.Services;
using HRC.ViewModels;

namespace ESME.Views.Misc
{
    public class UtilitiesViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
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
                message = "When ESME Workbench restarts, you will be required to complete the Configuration Wizard.\r\n\r\nContinue with restart?";
            else if (!ResetConfigurationDataRequested && ReimportEnvironmentDataRequested)
                message = "When ESME Workbench restarts, all range complexes will have their environment data caches rebuilt.\r\nThis requires all other users of One Navy Model who are sharing the same range complex data folders to exit One Navy Model and not re-enter the program until the caches are rebuilt. Failure to do this may result in application crashes or invalid data being provided to simulation engines.\r\n\r\nContinue with restart?";
            else message = "When ESME Workbench restarts, you will be required to complete the Configuration Wizard, and all range complexes will have their environment data caches rebuilt.\r\nThis requires all other users of One Navy Model who are sharing the same range complex data folders to exit One Navy Model and not re-enter the program until the caches are rebuilt. Failure to do this may result in application crashes or invalid data being provided to simulation engines.\r\n\r\nContinue with restart?";
            _messageBoxService.ShowYesNo(message, MessageBoxImage.Exclamation);
        }
        #endregion

        public bool ResetConfigurationDataRequested { get; set; }
        public bool ReimportEnvironmentDataRequested { get; set; }
    }
}
