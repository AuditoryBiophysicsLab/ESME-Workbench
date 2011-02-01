using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Properties;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("BSTViewModel")]
    internal class BSTViewModel : NAVODataSourceViewModel
    {
        #region public List<string> Resolutions { get; set; }

        static readonly PropertyChangedEventArgs ResolutionsChangedEventArgs = ObservableHelper.CreateArgs<BSTViewModel>(x => x.Resolutions);
        List<string> _resolutions;

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

        #endregion

        #region public string SelectedResolution { get; set; }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<BSTViewModel>(x => x.SelectedResolution);
        string _selectedResolution;

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

        #endregion

        readonly BST _bst = new BST();
        
        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _bst.DatabasePath = Globals.AppSettings.NAVOConfiguration.BSTDirectory;
            _bst.ExtractionProgramPath = Globals.AppSettings.NAVOConfiguration.BSTEXEPath;
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