using System.ComponentModel;
using System.IO;
using Cinch;
using ESME.Overlay;
using ESME.TransmissionLoss.CASS;
using ESME.Views.Locations;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        void InitializeNAEMOCruft()
        {
            SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
        }

        #region public SimAreaCSV SimAreaCSV { get; set; }

        public SimAreaCSV SimAreaCSV
        {
            get { return _simAreaCSV; }
            set
            {
                if (_simAreaCSV == value) return;
                _simAreaCSV = value;
                NotifyPropertyChanged(SimAreaCSVChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaCSVChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SimAreaCSV);
        SimAreaCSV _simAreaCSV;

        #endregion

        #region public SimAreaDescriptor SelectedRangeComplex { get; set; }

        public SimAreaDescriptor SelectedRangeComplex
        {
            get { return _selectedRangeComplex; }
            set
            {
                if (_selectedRangeComplex == value) return;
                _selectedRangeComplex = value;
                NotifyPropertyChanged(SelectedSimAreaChangedEventArgs);
                if (_selectedRangeComplex != null)
                    OverlayFiles = new OverlayFiles(_selectedRangeComplex.Name);
            }
        }

        static readonly PropertyChangedEventArgs SelectedSimAreaChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplex);
        SimAreaDescriptor _selectedRangeComplex;

        #endregion

        #region public OverlayFiles OverlayFiles { get; set; }

        public OverlayFiles OverlayFiles
        {
            get { return _overlayFiles; }
            set
            {
                if (_overlayFiles == value) return;
                _overlayFiles = value;
                NotifyPropertyChanged(OverlayFilesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.OverlayFiles);
        OverlayFiles _overlayFiles;

        #endregion

        #region public string SelectedOverlayName { get; set; }

        public string SelectedOverlayName
        {
            get { return _selectedOverlayName; }
            set
            {
                if (_selectedOverlayName == value) return;
                _selectedOverlayName = value;
                NotifyPropertyChanged(SelectedOverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayName);
        string _selectedOverlayName;

        #endregion

        #region NewLocationCommand

        public SimpleCommand<object, object> NewLocationCommand
        {
            get
            {
                return _newLocation ??
                       (_newLocation =
                        new SimpleCommand<object, object>(delegate { NewLocationHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newLocation;

        void NewLocationHandler()
        {
            var vm = new NewLocationViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewLocationView", vm);
            if ((result.HasValue) && (result.Value))
                SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
        }
        #endregion
    }
}
