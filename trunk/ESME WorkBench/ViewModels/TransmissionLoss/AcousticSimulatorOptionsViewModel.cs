using Cinch;
using ESME.Data;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class AcousticSimulatorOptionsViewModel : ViewModelBase
    {
        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                {
                    Globals.AppSettings.Save(null);
                    CloseActivePopUpCommand.Execute(true);
                }));
            }
        }

        SimpleCommand<object, object> _ok;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ?? (_cancel = new SimpleCommand<object, object>(delegate
                {
                    Globals.AppSettings.Reload(null);
                    CloseActivePopUpCommand.Execute(false);
                }));
            }
        }

        SimpleCommand<object, object> _cancel;

        #endregion

        public AppSettings AppSettings { get { return Globals.AppSettings; } }
    }
}
