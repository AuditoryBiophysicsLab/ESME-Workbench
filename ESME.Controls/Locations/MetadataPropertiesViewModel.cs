using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Metadata;

namespace ESME.Views.Locations
{
    public class MetadataPropertiesViewModel : ViewModelBase
    {
        public MetadataPropertiesViewModel(NAEMOOverlayMetadata overlayMetadata = null, NAEMOBathymetryMetadata bathymetryMetadata = null, NAEMOEnvironmentMetadata environmentMetadata = null, NAEMOScenarioMetadata scenarioMetadata = null)
        {
            if (overlayMetadata != null)
            {
                IsOverlayDisplay = true;
                DataSetType = "Overlay";
                DataSetName = Path.GetFileNameWithoutExtension(overlayMetadata.Filename);
                OverlayMetadata = overlayMetadata;
            } 
            else if (bathymetryMetadata != null)
            {
                IsBathymetryDisplay = true;
                DataSetType = "Bathymetry";
                DataSetName = Path.GetFileNameWithoutExtension(bathymetryMetadata.Filename);
                BathymetryMetadata = bathymetryMetadata;
            } 
            else if (environmentMetadata != null)
            {
                IsEnvironmentDisplay = true;
                DataSetType = "Environment";
                DataSetName = Path.GetFileNameWithoutExtension(environmentMetadata.Filename);
                EnvironmentMetadata = environmentMetadata;
            } 
            else if (scenarioMetadata != null)
            {
                IsScenarioDisplay = true;
                DataSetType = "Scenario";
                DataSetName = Path.GetFileNameWithoutExtension(scenarioMetadata.Filename);
                ScenarioMetadata = scenarioMetadata;
            }
        }

        #region public string DataSetType { get; set; }

        public string DataSetType
        {
            get { return _dataSetType; }
            set
            {
                if (_dataSetType == value) return;
                _dataSetType = value;
                NotifyPropertyChanged(DataSetTypeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataSetTypeChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.DataSetType);
        string _dataSetType;

        #endregion

        #region public string DataSetName { get; set; }

        public string DataSetName
        {
            get { return _dataSetName; }
            set
            {
                if (_dataSetName == value) return;
                _dataSetName = value;
                NotifyPropertyChanged(DataSetNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DataSetNameChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.DataSetName);
        string _dataSetName;

        #endregion

        #region public NAEMOOverlayMetadata OverlayMetadata { get; set; }

        public NAEMOOverlayMetadata OverlayMetadata
        {
            get { return _overlayMetadata; }
            set
            {
                if (_overlayMetadata == value) return;
                _overlayMetadata = value;
                NotifyPropertyChanged(OverlayMetadataChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs OverlayMetadataChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.OverlayMetadata);
        private NAEMOOverlayMetadata _overlayMetadata;

        #endregion

        #region public NAEMOBathymetryMetadata BathymetryMetadata { get; set; }

        public NAEMOBathymetryMetadata BathymetryMetadata
        {
            get { return _bathymetryMetadata; }
            set
            {
                if (_bathymetryMetadata == value) return;
                _bathymetryMetadata = value;
                NotifyPropertyChanged(BathymetryMetadataChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs BathymetryMetadataChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.BathymetryMetadata);
        private NAEMOBathymetryMetadata _bathymetryMetadata;

        #endregion

        #region public NAEMOEnvironmentMetadata EnvironmentMetadata { get; set; }

        public NAEMOEnvironmentMetadata EnvironmentMetadata
        {
            get { return _environmentMetadata; }
            set
            {
                if (_environmentMetadata == value) return;
                _environmentMetadata = value;
                NotifyPropertyChanged(EnvironmentMetadataChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs EnvironmentMetadataChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.EnvironmentMetadata);
        private NAEMOEnvironmentMetadata _environmentMetadata;

        #endregion

        #region public NAEMOScenarioMetadata ScenarioMetadata { get; set; }

        public NAEMOScenarioMetadata ScenarioMetadata
        {
            get { return _scenarioMetadata; }
            set
            {
                if (_scenarioMetadata == value) return;
                _scenarioMetadata = value;
                NotifyPropertyChanged(ScenarioMetadataChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs ScenarioMetadataChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.ScenarioMetadata);
        private NAEMOScenarioMetadata _scenarioMetadata;

        #endregion

        #region public bool IsBathymetryDisplay { get; set; }

        public bool IsBathymetryDisplay
        {
            get { return _isBathymetryDisplay; }
            set
            {
                if (_isBathymetryDisplay == value) return;
                _isBathymetryDisplay = value;
                NotifyPropertyChanged(IsBathymetryDisplayChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsBathymetryDisplayChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.IsBathymetryDisplay);
        bool _isBathymetryDisplay;

        #endregion

        #region public bool IsEnvironmentDisplay { get; set; }

        public bool IsEnvironmentDisplay
        {
            get { return _isEnvironmentDisplay; }
            set
            {
                if (_isEnvironmentDisplay == value) return;
                _isEnvironmentDisplay = value;
                NotifyPropertyChanged(IsEnvironmentDisplayChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs IsEnvironmentDisplayChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.IsEnvironmentDisplay);
        private bool _isEnvironmentDisplay;

        #endregion

        #region public bool IsScenarioDisplay { get; set; }

        public bool IsScenarioDisplay
        {
            get { return _isScenarioDisplay; }
            set
            {
                if (_isScenarioDisplay == value) return;
                _isScenarioDisplay = value;
                NotifyPropertyChanged(IsScenarioDisplayChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs IsScenarioDisplayChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.IsScenarioDisplay);
        private bool _isScenarioDisplay;

        #endregion

        #region public bool IsOverlayDisplay { get; set; }

        public bool IsOverlayDisplay
        {
            get { return _isOverlayDisplay; }
            set
            {
                if (_isOverlayDisplay == value) return;
                _isOverlayDisplay = value;
                NotifyPropertyChanged(IsOverlayDisplayChangedEventArgs);
            }
        }

        private static readonly PropertyChangedEventArgs IsOverlayDisplayChangedEventArgs = ObservableHelper.CreateArgs<MetadataPropertiesViewModel>(x => x.IsOverlayDisplay);
        private bool _isOverlayDisplay;

        #endregion

        #region OKCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _ok ??
                       (_ok =
                        new SimpleCommand<object, object>(delegate { return true; },
                                                          delegate { OKHandler(); }));
            }
        }

        private SimpleCommand<object, object> _ok;

        private void OKHandler()
        {
            if (BathymetryMetadata != null) BathymetryMetadata.Save();
            if (EnvironmentMetadata != null) EnvironmentMetadata.Save();
            if (ScenarioMetadata != null) ScenarioMetadata.Save();
            if (OverlayMetadata != null) OverlayMetadata.Save();
            CloseActivePopUpCommand.Execute(true);
        }

        #endregion
    }
}
