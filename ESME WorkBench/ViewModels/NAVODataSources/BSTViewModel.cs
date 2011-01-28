using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("BSTViewModel")]
    class BSTViewModel : NAVODataSourceViewModel
    {
        

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

        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<BSTViewModel>(x => x.Resolutions);
        List<string> _resolutions;

        #endregion

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

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<BSTViewModel>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        BST _bst = new BST();

        public BSTViewModel()
        {
            if (Resolutions != null) return;
            SetDatabasePaths(_bst);
            _bst.GetAllResolutions();
            Resolutions = _bst.Resolutions;
        }
        
        [MediatorMessageSink(MediatorMessage.ExtractBST)]
        void ExtractData(NAVOExtractionPacket packet)
        {
            _bst.ExtractArea(packet);
            ExtractedArea = _bst.ExtractedArea;
            MediatorMessage.Send(MediatorMessage.BSTExtracted);
        }

    }
}
