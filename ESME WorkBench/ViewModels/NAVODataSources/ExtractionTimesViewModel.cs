using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("ExtractionTimesViewModel")]
    internal class ExtractionTimesViewModel : ViewModelBase
    {
        #region public List<string> Months { get; set; }

        static readonly PropertyChangedEventArgs MonthsChangedEventArgs = ObservableHelper.CreateArgs<ExtractionTimesViewModel>(x => x.Months);
        List<string> _months;

        public List<string> Months
        {
            get { return _months; }
            set
            {
                if (_months == value) return;
                _months = value;
                NotifyPropertyChanged(MonthsChangedEventArgs);
            }
        }

        #endregion

        public ExtractionTimesViewModel()
        {
            Months = new List<string>
                     {
                         "January",
                         "February",
                         "March",
                         "April",
                         "May",
                         "June",
                         "July",
                         "August",
                         "September",
                         "October",
                         "November",
                         "December"
                     };
        }

        //mediator message sink method here, source is view OK command exiting cleanly.  If that's true, then send mediator messages to database viewmodels about their Min/Max Months.
    }
}