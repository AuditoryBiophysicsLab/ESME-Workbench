using System;
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

        #region RibbonTabSelectionChangedCommand
        public SimpleCommand<object, object> RibbonTabSelectionChangedCommand
        {
            get { return _ribbonTabSelectionChangedCommand ?? (_ribbonTabSelectionChangedCommand = new SimpleCommand<object, object>(delegate { RibbonTabSelectionChanged(); })); }
        }

        SimpleCommand<object, object> _ribbonTabSelectionChangedCommand;

        void RibbonTabSelectionChanged()
        {
            switch (((Views.MainView)_viewAwareStatus.View).Ribbon.SelectedIndex)
            {
                case 0:
                    Console.WriteLine("Experiment tab selected");
                    break;
                case 1:
                    Console.WriteLine("Scenario tab selected");
                    break;
                case 2:
                    Console.WriteLine("Environment tab selected");
                    break;
                case 3:
                    Console.WriteLine("Animals tab selected");
                    break;
                case 4:
                    Console.WriteLine("Acoustics tab selected");
                    break;
                default:
                    Console.WriteLine("Other tab selected");
                    break;
            }
        }
        #endregion

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
                {
                    NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name);
                    NAEMOBathymetryDescriptors = new NAEMOBathymetryDescriptors(_selectedRangeComplex.Name);
                    IsRangeComplexSelected = true;
                }
                else
                {
                    IsRangeComplexSelected = false;
                }
            }
        }

        static readonly PropertyChangedEventArgs SelectedSimAreaChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplex);
        SimAreaDescriptor _selectedRangeComplex;

        #endregion

        #region public bool IsRangeComplexSelected { get; set; }

        public bool IsRangeComplexSelected
        {
            get { return _isRangeComplexSelected; }
            set
            {
                if (_isRangeComplexSelected == value) return;
                _isRangeComplexSelected = value;
                NotifyPropertyChanged(RangeComplexIsSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RangeComplexIsSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsRangeComplexSelected);
        bool _isRangeComplexSelected;

        #endregion

        #region NewRangeComplexCommand

        public SimpleCommand<object, object> NewRangeComplexCommand
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
            var vm = new NewRangeComplexViewModel(Globals.AppSettings);
            var result = _visualizerService.ShowDialog("NewRangeComplexView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
                SelectedRangeComplex = SimAreaCSV[vm.LocationName];
            }
        }
        #endregion

        #region public NAEMOOverlayDescriptors NAEMOOverlayDescriptors { get; set; }

        public NAEMOOverlayDescriptors NAEMOOverlayDescriptors
        {
            get { return _naemoOverlayDescriptors; }
            set
            {
                if (_naemoOverlayDescriptors == value) return;
                _naemoOverlayDescriptors = value;
                NotifyPropertyChanged(OverlayFilesChangedEventArgs);
                SelectedOverlayDescriptor = NAEMOOverlayDescriptors[0].Value;
            }
        }

        static readonly PropertyChangedEventArgs OverlayFilesChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOOverlayDescriptors);
        NAEMOOverlayDescriptors _naemoOverlayDescriptors;

        #endregion

        #region public NAEMOOverlayDescriptor SelectedOverlayDescriptor { get; set; }

        public NAEMOOverlayDescriptor SelectedOverlayDescriptor
        {
            get { return _selectedOverlayDescriptor; }
            set
            {
                if (_selectedOverlayDescriptor == value) return;
                _selectedOverlayDescriptor = value;
                NotifyPropertyChanged(SelectedOverlayDescriptorChangedEventArgs);
                IsOverlayFileSelected = _selectedOverlayDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayDescriptor);
        NAEMOOverlayDescriptor _selectedOverlayDescriptor;

        #endregion

        #region public bool IsOverlayFileSelected { get; set; }

        public bool IsOverlayFileSelected
        {
            get { return _isOverlayFileSelected; }
            set
            {
                if (_isOverlayFileSelected == value) return;
                _isOverlayFileSelected = value;
                NotifyPropertyChanged(IsOverlayFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsOverlayFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsOverlayFileSelected);
        bool _isOverlayFileSelected;

        #endregion

        #region NewOverlayCommand

        public SimpleCommand<object, object> NewOverlayCommand
        {
            get
            {
                return _newOverlay ?? (_newOverlay = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { NewOverlayHandler(); }));
            }
        }

        private SimpleCommand<object, object> _newOverlay;

        void NewOverlayHandler()
        {
#if true
		    var vm = new NewOverlayViewModel(Globals.AppSettings, SelectedRangeComplex.Name);
            var result = _visualizerService.ShowDialog("NewOverlayView", vm);
            if ((result.HasValue) && (result.Value))
            {
                NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(SelectedRangeComplex.Name);
            }
#endif        
        }

        #endregion

        #region ExpandOverlayCommand
        public SimpleCommand<object, object> ExpandOverlayCommand
        {
            get { return _expandOverlay ?? (_expandOverlay = new SimpleCommand<object, object>(delegate { return IsExpandOverlayCommandEnabled; }, delegate { ExpandOverlayHandler(); })); }
        }

        SimpleCommand<object, object> _expandOverlay;

        bool IsExpandOverlayCommandEnabled
        {
            get { return IsOverlayFileSelected; }
        }

        void ExpandOverlayHandler()
        {
            // Graham : Show the popup dialog here
        }
        #endregion

        #region OverlayPropertiesCommand
        public SimpleCommand<object, object> OverlayPropertiesCommand
        {
            get { return _overlayProperties ?? (_overlayProperties = new SimpleCommand<object, object>(delegate { return IsOverlayPropertiesCommandEnabled; }, delegate { OverlayPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _overlayProperties;

        bool IsOverlayPropertiesCommandEnabled
        {
            get { return IsOverlayFileSelected; }
        }

        void OverlayPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(SelectedOverlayDescriptor.Metadata, null, null, null);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }
        #endregion

        #region public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors { get; set; }

        public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors
        {
            get { return _naemoBathymetryDescriptors; }
            set
            {
                if (_naemoBathymetryDescriptors == value) return;
                _naemoBathymetryDescriptors = value;
                NotifyPropertyChanged(NAEMOBathymetryDescriptorsChangedEventArgs);
                SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors[0].Value;
            }
        }

        static readonly PropertyChangedEventArgs NAEMOBathymetryDescriptorsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOBathymetryDescriptors);
        NAEMOBathymetryDescriptors _naemoBathymetryDescriptors;

        #endregion

        #region public NAEMOBathymetryDescriptor SelectedBathymetryDescriptor { get; set; }

        public NAEMOBathymetryDescriptor SelectedBathymetryDescriptor
        {
            get { return _selectedBathymetryDescriptor; }
            set
            {
                if (_selectedBathymetryDescriptor == value) return;
                _selectedBathymetryDescriptor = value;
                NotifyPropertyChanged(SelectedBathymetryDescriptorChangedEventArgs);
                IsBathymetryFileSelected = _selectedBathymetryDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryDescriptor);
        NAEMOBathymetryDescriptor _selectedBathymetryDescriptor;

        #endregion

        #region public bool IsBathymetryFileSelected { get; set; }

        public bool IsBathymetryFileSelected
        {
            get { return _isBathymetryFileSelected; }
            set
            {
                if (_isBathymetryFileSelected == value) return;
                _isBathymetryFileSelected = value;
                NotifyPropertyChanged(IsBathymetryFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsBathymetryFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsBathymetryFileSelected);
        bool _isBathymetryFileSelected;

        #endregion

        #region NewBathymetryCommand
        public SimpleCommand<object, object> NewBathymetryCommand
        {
            get { return _newBathymetry ?? (_newBathymetry = new SimpleCommand<object, object>(delegate { return IsNewBathymetryCommandEnabled; }, delegate { NewBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _newBathymetry;

        bool IsNewBathymetryCommandEnabled
        {
            get { return IsBathymetryFileSelected; }
        }

        void NewBathymetryHandler() { }
        #endregion

        #region BathymetryPropertiesCommand
        public SimpleCommand<object, object> BathymetryPropertiesCommand
        {
            get { return _bathymetryProperties ?? (_bathymetryProperties = new SimpleCommand<object, object>(delegate { return IsBathymetryPropertiesCommandEnabled; }, delegate { BathymetryPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _bathymetryProperties;

        bool IsBathymetryPropertiesCommandEnabled
        {
            get { return IsBathymetryFileSelected; }
        }

        void BathymetryPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(null, SelectedBathymetryDescriptor.Metadata, null, null);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }

        }
        #endregion

    }
}
