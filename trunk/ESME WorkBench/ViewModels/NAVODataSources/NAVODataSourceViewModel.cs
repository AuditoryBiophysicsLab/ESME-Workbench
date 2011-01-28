using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    public abstract class NAVODataSourceViewModel: ViewModelBase
    {
        #region public EnvironmentData ExtractedArea { get; set; }

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

        static readonly PropertyChangedEventArgs ExtractedAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSourceViewModel>(x => x.ExtractedArea);
        EnvironmentData _extractedArea;

        #endregion

        public void SetDatabasePaths(NAVODataSource dataSource)
        {
            dataSource.DatabasePath = Properties.Settings.Default.BSTDirectory;
            dataSource.ExtractionProgramPath = Properties.Settings.Default.BSTEXEDirectory;
        }

        #region ValidateDatabaseCommand

        public SimpleCommand<object, object> ValidateDatabaseCommand
        {
            get { return _validateDatabase ?? (_validateDatabase = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("ValidateDatabaseCommandMessage"); })); }
        }

        SimpleCommand<object, object> _validateDatabase;

        #endregion
    }
}
