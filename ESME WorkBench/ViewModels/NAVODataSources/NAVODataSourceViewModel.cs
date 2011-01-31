using System.ComponentModel;
using Cinch;
using ESME.Environment;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    public abstract class NAVODataSourceViewModel : ViewModelBase
    {
        #region public EnvironmentData ExtractedArea { get; set; }

        static readonly PropertyChangedEventArgs ExtractedAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSourceViewModel>(x => x.ExtractedArea);
        EnvironmentData _extractedArea;

        public EnvironmentData ExtractedArea
        {
            get { return _extractedArea; }
            set
            {
                if (_extractedArea == value) return;
                _extractedArea = value;
                NotifyPropertyChanged(ExtractedAreaChangedEventArgs);
            }
        }

        #endregion

        #region ValidateDatabaseCommand

        SimpleCommand<object, object> _validateDatabase;

        public SimpleCommand<object, object> ValidateDatabaseCommand
        {
            get { return _validateDatabase ?? (_validateDatabase = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("ValidateDatabaseCommandMessage"); })); }
        }

        #endregion

        
    }
}