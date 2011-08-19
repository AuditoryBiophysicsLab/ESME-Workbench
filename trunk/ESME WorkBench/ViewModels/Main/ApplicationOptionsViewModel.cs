using System.Collections.Generic;
using Cinch;
using ESME.Data;
using ESME.Environment.NAVO;

namespace ESMEWorkBench.ViewModels.Main
{
    public class ApplicationOptionsViewModel : ViewModelBase
    {
        public ApplicationOptionsViewModel()
        {
            Globals.AppSettings = AppSettings.Load();
            AppSettings = Globals.AppSettings;
        }

        public void DesignTimeInitialization() { AppSettings = AppSettings.Load(); }

        public AppSettings AppSettings { get; private set; }

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return IsOkCommandEnabled; },
                                                          delegate { OkHandler(); }));
            }
        }

        private SimpleCommand<object, object> _ok;

        private bool IsOkCommandEnabled
        {
            get
            {
                return true;
            }
        }

        private void OkHandler()
        {
            AppSettings.Save(null);
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get
            {
                return _cancel ??
                       (_cancel =
                        new SimpleCommand<object, object>(delegate { return IsCancelCommandEnabled; },
                                                          delegate { CancelHandler(); }));
            }
        }

        private SimpleCommand<object, object> _cancel;

        private bool IsCancelCommandEnabled
        {
            get { return true; }
        }

        private void CancelHandler()
        {
            AppSettings = AppSettings.Load();
            CloseActivePopUpCommand.Execute(false);
        }

        #endregion

        #region public List<NAVOTimePeriod> Months { get; set; }

        public List<NAVOTimePeriod> Months
        {
            get
            {
                return _months ?? (_months = new List<NAVOTimePeriod>
                                             {
                                                 NAVOTimePeriod.January,
                                                 NAVOTimePeriod.February,
                                                 NAVOTimePeriod.March,
                                                 NAVOTimePeriod.April,
                                                 NAVOTimePeriod.May,
                                                 NAVOTimePeriod.June,
                                                 NAVOTimePeriod.July,
                                                 NAVOTimePeriod.August,
                                                 NAVOTimePeriod.September,
                                                 NAVOTimePeriod.October,
                                                 NAVOTimePeriod.November,
                                                 NAVOTimePeriod.December,
                                             });
            }
        }

        List<NAVOTimePeriod> _months;

        #endregion


    }
}