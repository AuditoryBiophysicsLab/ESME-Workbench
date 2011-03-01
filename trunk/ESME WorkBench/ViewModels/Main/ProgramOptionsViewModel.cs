using System;
using System.Collections.Generic;
using Cinch;
using ESME.Data;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.Main
{
    public class ProgramOptionsViewModel : ViewModelBase, IDesignTimeAware
    {
        public ProgramOptionsViewModel()
        {
            AppSettings = Globals.AppSettings;

            OkCommand = new SimpleCommand<object, object>(delegate
            {
                AppSettings.Save(null);
                CloseActivePopUpCommand.Execute(true);
            });
            CancelCommand = new SimpleCommand<object, object>(delegate
            {
                AppSettings.Reload(null);
                CloseActivePopUpCommand.Execute(false);
            });
        }

        public void DesignTimeInitialization() { AppSettings = AppSettings.Load(null); }

        public AppSettings AppSettings { get; private set; }
        
        public SimpleCommand<Object, Object> OkCommand { get; private set; }
        public SimpleCommand<Object, Object> CancelCommand { get; private set; }

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