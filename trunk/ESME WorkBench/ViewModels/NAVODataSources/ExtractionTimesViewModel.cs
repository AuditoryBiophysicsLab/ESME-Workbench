using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Data;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("ExtractionTimesViewModel")]
    internal class ExtractionTimesViewModel : ViewModelBase
    {
        #region public List<NAVOTimePeriod> Months { get; set; }

        public List<NAVOTimePeriod> Months
        {
            get { return _months; }
            set
            {
                if (_months == value) return;
                _months = value;
                NotifyPropertyChanged(MonthsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MonthsChangedEventArgs = ObservableHelper.CreateArgs<ExtractionTimesViewModel>(x => x.Months);
        List<NAVOTimePeriod> _months;

        #endregion

        #region public AppSettings AppSettings { get; set; }

        public AppSettings AppSettings
        {
            get { return _appSettings; }
            set
            {
                if (_appSettings == value) return;
                _appSettings = value;
                NotifyPropertyChanged(AppSettingsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AppSettingsChangedEventArgs = ObservableHelper.CreateArgs<ExtractionTimesViewModel>(x => x.AppSettings);
        AppSettings _appSettings;

        #endregion


        public ExtractionTimesViewModel()
        {
            Months = new List<NAVOTimePeriod>
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
                     };
            AppSettings = Globals.AppSettings; //todo
        }

        
    }
}