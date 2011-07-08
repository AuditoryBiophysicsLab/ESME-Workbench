using System;
using System.ComponentModel;
using System.IO;
using System.Windows.Input;
using Cinch;
using ESME.Metadata;
using ESME.Overlay;
using ESME.TransmissionLoss.CASS;
using ESME.Views.EnvironmentBuilder;
using ESME.Views.Locations;
using HRC.Navigation;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        void InitializeNAEMOCruft()
        {
            SimAreaCSV = SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            if (_simAreaCSVWatcher != null) _simAreaCSVWatcher.Dispose();
            _simAreaCSVWatcher = new FileSystemWatcher(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv");
            _simAreaCSVWatcher.Changed += (s, e) => SimAreaCSV.ReadCSV(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, "SimAreas.csv"));
            _simAreaCSVWatcher.EnableRaisingEvents = true;
        }

        FileSystemWatcher _simAreaCSVWatcher;

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

        #region Range Complex ribbon group

        #region public SimAreaCSV SimAreaCSV { get; set; }

        public SimAreaCSV SimAreaCSV
        {
            get { return _simAreaCSV; }
            set
            {
                if (_simAreaCSV == value) return;
                _simAreaCSV = value;
                NotifyPropertyChanged(SimAreaCSVChangedEventArgs);
                SelectedRangeComplex = _simAreaCSV[0];
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
                if (_selectedRangeComplex != null) SelectedRangeComplexInfo = string.Format("Name: {0}\nReference Point: ({1}, {2})\nHeight: {3}\nGeoid Separation: {4}\nOps Limit: {5}\nSim Limit: {6}", _selectedRangeComplex.Name, Math.Round(_selectedRangeComplex.Latitude, 5), Math.Round(_selectedRangeComplex.Longitude, 5), _selectedRangeComplex.Height, _selectedRangeComplex.GeoidSeparation, SelectedRangeComplex.OpsLimitFile, SelectedRangeComplex.SimLimitFile);
                IsRangeComplexSelected = _selectedRangeComplex != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedSimAreaChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplex);
        SimAreaDescriptor _selectedRangeComplex;

        #endregion

        #region public string SelectedRangeComplexInfo { get; set; }

        public string SelectedRangeComplexInfo
        {
            get { return _selectedRangeComplexInfo; }
            set
            {
                if (_selectedRangeComplexInfo == value) return;
                _selectedRangeComplexInfo = value;
                NotifyPropertyChanged(SelectedRangeComplexInfoChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexInfoChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedRangeComplexInfo);
        string _selectedRangeComplexInfo;

        #endregion

        #region public bool IsRangeComplexSelected { get; set; }

        public bool IsRangeComplexSelected
        {
            get { return _isRangeComplexSelected; }
            set
            {
                if (_isRangeComplexSelected == value) return;
                _isRangeComplexSelected = value;
                if (_isRangeComplexSelected)
                {
                    //Console.WriteLine("{0}: Selected range complex: {1}", DateTime.Now, _selectedRangeComplex.Name);

                    var bw1 = new BackgroundWorker();
                    bw1.DoWork += (s, e) => NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name);
                    bw1.RunWorkerCompleted += (s, e) => CommandManager.InvalidateRequerySuggested();
                    bw1.RunWorkerAsync();
                    //_dispatcher.BeginInvoke(new Action(() => NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name)) , DispatcherPriority.ApplicationIdle);
                    //Console.WriteLine("{0}: Overlay descriptors created", DateTime.Now);
                    var bw2 = new BackgroundWorker();
                    bw2.DoWork += (s, e) => NAEMOBathymetryDescriptors = new NAEMOBathymetryDescriptors(_selectedRangeComplex.Name);
                    bw2.RunWorkerCompleted += (s, e) => CommandManager.InvalidateRequerySuggested();
                    bw2.RunWorkerAsync();
                    //_dispatcher.BeginInvoke(new Action(() => NAEMOBathymetryDescriptors = new NAEMOBathymetryDescriptors(_selectedRangeComplex.Name)), DispatcherPriority.ApplicationIdle);
                    //Console.WriteLine("{0}: Bathymetry descriptors created", DateTime.Now);
                    var bw3 = new BackgroundWorker();
                    bw3.DoWork += (s, e) => NAEMOEnvironmentDescriptors = new NAEMOEnvironmentDescriptors(_selectedRangeComplex.Name);
                    bw3.RunWorkerCompleted += (s, e) => CommandManager.InvalidateRequerySuggested();
                    bw3.RunWorkerAsync();
                    //_dispatcher.BeginInvoke(new Action(() => NAEMOEnvironmentDescriptors = new NAEMOEnvironmentDescriptors(_selectedRangeComplex.Name)), DispatcherPriority.ApplicationIdle);
                    //Console.WriteLine("{0}: Environment descriptors created", DateTime.Now);
                }
                else
                {
                    NAEMOOverlayDescriptors = null;
                    NAEMOBathymetryDescriptors = null;
                    NAEMOEnvironmentDescriptors = null;
                }
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

        #endregion

        #region Overlay ribbon group

        #region public NAEMOOverlayDescriptors NAEMOOverlayDescriptors { get; set; }

        public NAEMOOverlayDescriptors NAEMOOverlayDescriptors
        {
            get { return _naemoOverlayDescriptors; }
            set
            {
                if (_naemoOverlayDescriptors == value) return;
                _naemoOverlayDescriptors = value;
                NotifyPropertyChanged(OverlayFilesChangedEventArgs);
                NotifyPropertyChanged(IsOverlayListReadyChangedEventArgs);
                SelectedOverlayDescriptor = NAEMOOverlayDescriptors != null && NAEMOOverlayDescriptors.Count > 0 ? NAEMOOverlayDescriptors[0].Value : null;
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
                _selectedOverlayDescriptor = value;
                NotifyPropertyChanged(SelectedOverlayDescriptorChangedEventArgs);
                if (_selectedOverlayDescriptor != null) SelectedOverlayInfo = string.Format("Name: {0}\nBuffer: {1}\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedOverlayDescriptor.DataFilename), _selectedOverlayDescriptor.Metadata.BufferZoneSize > 0 ? _selectedOverlayDescriptor.Metadata.BufferZoneSize.ToString() + " km" : "[N/A]", _selectedOverlayDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsOverlayFileSelected = _selectedOverlayDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayDescriptor);
        NAEMOOverlayDescriptor _selectedOverlayDescriptor;

        #endregion

        #region public string SelectedOverlayInfo { get; set; }

        public string SelectedOverlayInfo
        {
            get { return _selectedOverlayInfo; }
            set
            {
                if (_selectedOverlayInfo == value) return;
                _selectedOverlayInfo = value;
                NotifyPropertyChanged(SelectedOverlayNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedOverlayNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedOverlayInfo);
        string _selectedOverlayInfo;

        #endregion

        #region public bool IsOverlayListReady { get; set; }

        public bool IsOverlayListReady
        {
            get { return _naemoOverlayDescriptors != null; }
        }

        static readonly PropertyChangedEventArgs IsOverlayListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsOverlayListReady);

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
            var vm = new NewOverlayViewModel(Globals.AppSettings, SelectedRangeComplex.Name);
            var result = _visualizerService.ShowDialog("NewOverlayView", vm);
            if ((result.HasValue) && (result.Value))
            {
                NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(SelectedRangeComplex.Name);
            }
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
            var vm = new OverlayExpandViewModel(SelectedOverlayDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("OverlayExpandView", vm);
            if ((result.HasValue) && (result.Value))
            {
                //vm.BufferZoneSize
                var curOverlay = SelectedOverlayDescriptor.Data;
                var limits = (Limits)(new GeoRect(curOverlay.Shapes[0].BoundingBox));
                var expandedLimits = limits.CreateExpandedLimit(vm.BufferZoneSize);  //in km.
                var geoRect = new GeoRect(expandedLimits.GeoPointList);
                var overlayFileName = string.Format("{0}_{1}km.ovr",Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename),vm.BufferZoneSize);
                var metadataFileName = string.Format("{0}_{1}km.xml", Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename), vm.BufferZoneSize);
                var overlayPath = Path.Combine(Path.GetDirectoryName(SelectedOverlayDescriptor.DataFilename),overlayFileName);
                var metadataPath = Path.Combine(Path.GetDirectoryName(SelectedOverlayDescriptor.DataFilename), metadataFileName);
                using (var writer = new StreamWriter(overlayPath))
                {
                    writer.WriteLine("navigation");
                    writer.WriteLine("green");
                    writer.WriteLine("solid");
                    writer.WriteLine("move");
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthWest.Latitude, geoRect.NorthWest.Longitude);
                    writer.WriteLine("lines");
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthEast.Latitude, geoRect.NorthEast.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.SouthEast.Latitude, geoRect.SouthEast.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.SouthWest.Latitude, geoRect.SouthWest.Longitude);
                    writer.WriteLine("{0:0.0000}  {1:0.0000}", geoRect.NorthWest.Latitude, geoRect.NorthWest.Longitude);
                }

                var metadata = new NAEMOOverlayMetadata
                                   {
                                       Bounds = geoRect,
                                       BufferZoneSize = vm.BufferZoneSize,
                                       Filename = metadataPath,
                                       OverlayFilename = Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename),
                                   };
                metadata.Save();

                NAEMOOverlayDescriptors = new NAEMOOverlayDescriptors(_selectedRangeComplex.Name);
                SelectedOverlayDescriptor = (NAEMOOverlayDescriptor)NAEMOOverlayDescriptors[Path.GetFileNameWithoutExtension(overlayFileName)];

            }
        }

        #endregion

        #region OverlayPropertiesCommand
        public SimpleCommand<object, object> OverlayPropertiesCommand
        {
            get { return _overlayProperties ?? (_overlayProperties = new SimpleCommand<object, object>(delegate { return IsOverlayFileSelected; }, delegate { OverlayPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _overlayProperties;

        void OverlayPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(SelectedOverlayDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedOverlayDescriptor = _selectedOverlayDescriptor;
            }
        }
        #endregion

        #endregion

        #region Bathymetry ribbon group

        #region public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors { get; set; }

        public NAEMOBathymetryDescriptors NAEMOBathymetryDescriptors
        {
            get { return _naemoBathymetryDescriptors; }
            set
            {
                if (_naemoBathymetryDescriptors == value) return;
                _naemoBathymetryDescriptors = value;
                NotifyPropertyChanged(NAEMOBathymetryDescriptorsChangedEventArgs);
                NotifyPropertyChanged(IsBathymetryListReadyChangedEventArgs);
                SelectedBathymetryDescriptor = NAEMOBathymetryDescriptors != null && NAEMOBathymetryDescriptors.Count > 0 ? NAEMOBathymetryDescriptors[0].Value : null;
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
                _selectedBathymetryDescriptor = value;
                NotifyPropertyChanged(SelectedBathymetryDescriptorChangedEventArgs);
                if (_selectedBathymetryDescriptor != null) SelectedBathymetryInfo = string.Format("Name: {0}\nResolution: {1} min\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedBathymetryDescriptor.DataFilename), _selectedBathymetryDescriptor.Metadata.Resolution, _selectedBathymetryDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsBathymetryFileSelected = _selectedBathymetryDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryDescriptor);
        NAEMOBathymetryDescriptor _selectedBathymetryDescriptor;

        #endregion

        #region public string SelectedBathymetryInfo { get; set; }

        public string SelectedBathymetryInfo
        {
            get { return _selectedBathymetryInfo; }
            set
            {
                if (_selectedBathymetryInfo == value) return;
                _selectedBathymetryInfo = value;
                NotifyPropertyChanged(SelectedBathymetryNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBathymetryNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedBathymetryInfo);
        string _selectedBathymetryInfo;

        #endregion

        #region public bool IsBathymetryListReady { get; set; }

        public bool IsBathymetryListReady
        {
            get { return _naemoBathymetryDescriptors != null; }
        }

        static readonly PropertyChangedEventArgs IsBathymetryListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsBathymetryListReady);

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
            get { return _newBathymetry ?? (_newBathymetry = new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; }, delegate { NewBathymetryHandler(); })); }
        }

        SimpleCommand<object, object> _newBathymetry;

        void NewBathymetryHandler()
        {
            var vm = new BathymetryExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("BathymetryExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }

        #endregion

        #region BathymetryPropertiesCommand
        public SimpleCommand<object, object> BathymetryPropertiesCommand
        {
            get { return _bathymetryProperties ?? (_bathymetryProperties = new SimpleCommand<object, object>(delegate { return IsBathymetryFileSelected; }, delegate { BathymetryPropertiesHandler(); })); }
        }

        SimpleCommand<object, object> _bathymetryProperties;

        void BathymetryPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(null, SelectedBathymetryDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedBathymetryDescriptor = _selectedBathymetryDescriptor;
            }

        }
        #endregion

        #endregion

        #region Environment ribbon group

        #region public NAEMOEnvironmentDescriptors NAEMOEnvironmentDescriptors { get; set; }

        public NAEMOEnvironmentDescriptors NAEMOEnvironmentDescriptors
        {
            get { return _naemoEnvironmentDescriptors; }
            set
            {
                if (_naemoEnvironmentDescriptors == value) return;
                _naemoEnvironmentDescriptors = value;
                NotifyPropertyChanged(NAEMOEnvironmentDescriptorsChangedEventArgs);
                NotifyPropertyChanged(IsEnvironmentListReadyChangedEventArgs);
                SelectedEnvironmentDescriptor = NAEMOEnvironmentDescriptors != null && NAEMOEnvironmentDescriptors.Count > 0 ? NAEMOEnvironmentDescriptors[0].Value : null;
            }
        }

        static readonly PropertyChangedEventArgs NAEMOEnvironmentDescriptorsChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.NAEMOEnvironmentDescriptors);
        NAEMOEnvironmentDescriptors _naemoEnvironmentDescriptors;

        #endregion

        #region public NAEMOEnvironmentDescriptor SelectedEnvironmentDescriptor { get; set; }

        public NAEMOEnvironmentDescriptor SelectedEnvironmentDescriptor
        {
            get { return _selectedEnvironmentDescriptor; }
            set
            {
                _selectedEnvironmentDescriptor = value;
                NotifyPropertyChanged(SelectedEnvironmentDescriptorChangedEventArgs);
                if (_selectedEnvironmentDescriptor != null) SelectedEnvironmentInfo = string.Format("Name: {0}\nTime Period: {1}\nSource Overlay: {2}", Path.GetFileNameWithoutExtension(_selectedEnvironmentDescriptor.DataFilename), _selectedEnvironmentDescriptor.Metadata.TimePeriod, _selectedEnvironmentDescriptor.Metadata.OverlayFilename ?? "[Unknown]");
                IsEnvironmentFileSelected = _selectedEnvironmentDescriptor != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentDescriptorChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironmentDescriptor);
        NAEMOEnvironmentDescriptor _selectedEnvironmentDescriptor;

        #endregion

        #region public string SelectedEnvironmentInfo { get; set; }

        public string SelectedEnvironmentInfo
        {
            get { return _selectedEnvironmentInfo; }
            set
            {
                if (_selectedEnvironmentInfo == value) return;
                _selectedEnvironmentInfo = value;
                NotifyPropertyChanged(SelectedEnvironmentNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedEnvironmentNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedEnvironmentInfo);
        string _selectedEnvironmentInfo;

        #endregion

        #region public bool IsEnvironmentFileSelected { get; set; }

        public bool IsEnvironmentFileSelected
        {
            get { return _isEnvironmentFileSelected; }
            set
            {
                if (_isEnvironmentFileSelected == value) return;
                _isEnvironmentFileSelected = value;
                NotifyPropertyChanged(IsEnvironmentFileSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentFileSelectedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEnvironmentFileSelected);
        bool _isEnvironmentFileSelected;

        #endregion

        #region public bool IsEnvironmentListReady { get; set; }

        public bool IsEnvironmentListReady
        {
            get { return _naemoEnvironmentDescriptors != null; }
        }

        static readonly PropertyChangedEventArgs IsEnvironmentListReadyChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsEnvironmentListReady);

        #endregion

        #region NewEnvironmentCommand
        public SimpleCommand<object, object> NewEnvironmentCommand
        {
            get
            {
                return _newEnvironment ??
                       (_newEnvironment =
                        new SimpleCommand<object, object>(delegate { return IsRangeComplexSelected; },
                                                          delegate { NewEnvironmentHandler(); }));
            }
        }

        SimpleCommand<object, object> _newEnvironment;

        void NewEnvironmentHandler()
        {
            var vm = new EnvironmentExtractionViewModel(Path.GetFileNameWithoutExtension(SelectedOverlayDescriptor.DataFilename));
            var result = _visualizerService.ShowDialog("EnvironmentExtractionView", vm);
            if ((result.HasValue) && (result.Value))
            {
            }
        }

        #endregion

        #region EnvironmentPropertiesCommand
        public SimpleCommand<object, object> EnvironmentPropertiesCommand
        {
            get
            {
                return _environmentProperties ??
                       (_environmentProperties =
                        new SimpleCommand<object, object>(delegate { return IsEnvironmentFileSelected; },
                                                          delegate { EnvironmentPropertiesHandler(); }));
            }
        }

        SimpleCommand<object, object> _environmentProperties;

        void EnvironmentPropertiesHandler()
        {
            var vm = new MetadataPropertiesViewModel(null,null,SelectedEnvironmentDescriptor.Metadata);
            var result = _visualizerService.ShowDialog("MetadataPropertiesView", vm);
            if ((result.HasValue) && (result.Value))
            {
                SelectedEnvironmentDescriptor = _selectedEnvironmentDescriptor;
            }
        }
        #endregion

        #endregion
    }
}
