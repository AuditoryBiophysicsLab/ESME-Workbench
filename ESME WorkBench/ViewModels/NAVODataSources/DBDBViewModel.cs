using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("DBDBViewModel")]
    public class DBDBViewModel : NAVODataSourceViewModel
    {
        DBDB _dbdb = new DBDB();
        #region public string SelectedResolution { get; set; }

        public string SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<DBDBViewModel>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        #region public List<string> Resolutions { get; set; }

        public List<string> Resolutions
        {
            get { return _resolutions; }
            set
            {
                if (_resolutions == value) return;
                _resolutions = value;
                NotifyPropertyChanged(ResolutionsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<DBDBViewModel>(x => x.Resolutions);
        List<string> _resolutions;

        #endregion

        public DBDBViewModel()
        {
        }

        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _dbdb.DatabasePath = Properties.Settings.Default.DBDBDirectory;
            _dbdb.ExtractionProgramPath = Properties.Settings.Default.DBDBEXEDirectory;
        }

        void GetResolutions()
        {
            _dbdb.GetAllResolutions();
            Resolutions = _dbdb.Resolutions;
        }

        [MediatorMessageSink(MediatorMessage.ExtractDBDB)]
        void ExtractData(NAVOExtractionPacket packet)
        {
            _dbdb.ExtractArea(packet);
            ExtractedArea = _dbdb.ExtractedArea;
            MediatorMessage.Send(MediatorMessage.DBDBExtracted);
        }
    }
}