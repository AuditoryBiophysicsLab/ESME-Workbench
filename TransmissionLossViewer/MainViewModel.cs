using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.Services;
using ESME.Views.TransmissionLossViewer;
using HRC.Aspects;
using HRC.Services;
using MEFedMVVM.ViewModelLocator;
using ESME.Locations;


namespace TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossViewerMainViewModel")]
    [NotifyPropertyChanged]
    class MainViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Database");
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;
        readonly IHRCOpenFileService _openFileService;
        readonly IViewParameterService _viewParameterService;
        readonly IMessageBoxService _messageBoxService;
        readonly IUIVisualizerService _visualizerService;
        public MasterDatabaseService Database { get; private set; }
        private Scenario _selectedScenario;
        public Scenario SelectedScenario
        {
            get { return _selectedScenario; }
            set
            {
                _selectedScenario = value;
                Platforms.Clear();
                if (_selectedScenario == null) return;
                foreach (var platform in from p in Database.Context.Platforms
                                         where p.Scenario.Guid == _selectedScenario.Guid
                                         select p)
                    Platforms.Add(platform);
            }
        }

        private Platform _selectedPlatform;
        public Platform SelectedPlatform
        {
            get { return _selectedPlatform; }
            set { 
                _selectedPlatform = value;
                Sources.Clear();
                if (_selectedPlatform == null) return;

                foreach (var source in from s in Database.Context.Sources
                                       where s.Platform.Guid == _selectedPlatform.Guid
                                       select s)
                    Sources.Add(source);
            }
        }

        private Source _selectedSource;
        public Source SelectedSource
        {
            get { return _selectedSource; }
            set
            {
                _selectedSource = value;
                Modes.Clear();
                if (_selectedSource == null) return;

                foreach (var mode in from m in Database.Context.Modes
                                     where m.Source.Guid == _selectedSource.Guid
                                     select m)
                    Modes.Add(mode);
            }
        }

        private Mode _selectedMode;
        public Mode SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                _selectedMode = value;
                Radials.Clear();
                if (_selectedMode == null) return;
                foreach (var radial in from r in Database.Context.Radials
                                       where
                                           r.IsCalculated &&
                                           r.TransmissionLoss.Mode.Guid == _selectedMode.Guid
                                       select r)
                    Radials.Add(radial);
            }
        }

        private Radial _selectedRadial;
        public Radial SelectedRadial
        {
            get { return _selectedRadial; }
            set
            {
                _selectedRadial = value;
                
                if (_selectedRadial == null) return;
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, _selectedRadial);
            }
        }

        [Initialize]
        public ObservableCollection<Radial> Radials { get; set; }
        [Initialize]
        public ObservableCollection<Platform> Platforms { get; set; }
        [Initialize]
        public ObservableCollection<Source> Sources { get; set; }
        [Initialize]
        public ObservableCollection<Mode> Modes { get; set; }   

        #region public constructor
        [ImportingConstructor]
        public MainViewModel(IHRCSaveFileService saveFileService, IHRCOpenFileService openFileService, IViewParameterService viewParameterService, IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IUIVisualizerService visualizerService, MasterDatabaseService database)
        {
            RegisterMediator();
            Database = database;
            _saveFileService = saveFileService;
            _openFileService = openFileService;
            _messageBoxService = messageBoxService;
            _visualizerService = visualizerService;
            _viewParameterService = viewParameterService;
            _viewParameterService.TransmissionLayersWidth = Properties.Settings.Default.TransmissionLayersWidth;
            _viewParameterService.PropertyChanged += (s, e) =>
                                                     {
                                                         switch (e.PropertyName)
                                                         {
                                                             case "TransmissionLayersWidth":
                                                                 Properties.Settings.Default.TransmissionLayersWidth = _viewParameterService.TransmissionLayersWidth;
                                                                 break;
                                                         }
                                                     };
            _viewAwareStatus = viewAwareStatus;
            
            //this will change...
            Database.MasterDatabaseDirectory = _databaseDirectory;
            
        }

        



        #endregion

        #region public double SelectedRadialBearing { get; set; }

        public double SelectedRadialBearing
        {
            get { return _selectedRadialBearing; }
            set
            {
                if (_selectedRadialBearing == value) return;
                _selectedRadialBearing = value;
                NotifyPropertyChanged(SelectedRadialBearingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedRadialBearingChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadialBearing);
        double _selectedRadialBearing;

        #endregion

        #region public string SelectedTransmissionLossFieldName { get; set; }

        public string SelectedTransmissionLossFieldName
        {
            get { return _selectedTransmissionLossFieldName; }
            set
            {
                if (_selectedTransmissionLossFieldName == value) return;
                _selectedTransmissionLossFieldName = value;
                NotifyPropertyChanged(SelectedTransmissionLossFieldNameChangedEventArgs);



            }
        }

        static readonly PropertyChangedEventArgs SelectedTransmissionLossFieldNameChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.SelectedTransmissionLossFieldName);
        string _selectedTransmissionLossFieldName;

        #endregion

        #region public string  OutputFileName { get; set; }

        public string OutputFileName
        {
            get
            {
                //update the default ouput file string
                //todo:possible race condition ?
                lock (this)
                {
                    if (SelectedTransmissionLossFieldName == null) return null;
                    var fieldName = SelectedTransmissionLossFieldName.Replace('|', ' ');
                    return Path.Combine(Properties.Settings.Default.ExperimentReportDirectory, fieldName + string.Format(" radial {0} degrees", SelectedRadialBearing));
                }


            }

        }

        #endregion

        #region public double TransmissionLayersWidth { get; set; }

        public double TransmissionLayersWidth
        {
            get { return Properties.Settings.Default.TransmissionLayersWidth; }
            set
            {
                Properties.Settings.Default.TransmissionLayersWidth = value;
                NotifyPropertyChanged(TransmissionLayersWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLayersWidthChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.TransmissionLayersWidth);

        #endregion

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    var ea = (CancelEventArgs)vcArgs.EventArgs;
                    Properties.Settings.Default.Save();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

#if false
        #region CloseWindowCommand

        SimpleCommand<object, object> _closeWindow;

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get
            {
                return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate
                                                                                         {
                                                                                             ((MainView)_viewAwareStatus.View).Close();

                                                                                         }));
            }
        }

        #endregion

        #region SaveAsCommand

        SimpleCommand<object, object> _saveAs;

        public SimpleCommand<object, object> SaveAsCommand
        {
            get
            {
                return _saveAs ?? (_saveAs = new SimpleCommand<object, object>(delegate
                {

                    _saveFileService.Filter = "Portable Network Graphics (*.png)|*.png| JPEG (*.jpeg)|*.jpg|Bitmap (*.bmp)|*.bmp";
                    _saveFileService.OverwritePrompt = true;
                    //if (OutputFileName == null) return;
                    _saveFileService.FileName = OutputFileName;

                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        MediatorMessage.Send(MediatorMessage.SaveRadialBitmap, _saveFileService.FileName);
                    }
                    //MediatorMessage.Send(MediatorMessage.ResetSelectedField, true);
                    Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        #endregion

        #region ExportAsCommand

        public SimpleCommand<object, object> ExportAsCommand
        {
            get
            {
                return _exportAs ?? (_exportAs = new SimpleCommand<object, object>(delegate
                {
                    _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = OutputFileName;

                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        MediatorMessage.Send(MediatorMessage.SaveRadialAsCSV, _saveFileService.FileName);
                    }
                    //MediatorMessage.Send(MediatorMessage.ResetSelectedField,true);
                    Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        #region OpenCommand

        public SimpleCommand<object, object> OpenCommand
        {
            get
            {
                return _open ?? (_open = new SimpleCommand<object, object>(
                    delegate
                    {
                        _openFileService.Filter = "Bellhop Shades (*.shd)|*.shd|All Files(*.*)|*.*"; //CASS Output (*.bin)|*.bin|All Files (*.*)|*.*";
                        _openFileService.InitialDirectory = Properties.Settings.Default.ExperimentReportDirectory;
                        _openFileService.Title = "Select a Transmission Loss file to view";

                        var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
                        if (result.HasValue && result.Value)
                        {
                            //OpenCASSFile(_openFileService.FileName);
                            TransmissionLossRadial = new TransmissionLossRadial(0, new BellhopOutput(_openFileService.FileName));
                        }
                    }));
            }
        }

        SimpleCommand<object, object> _open;

        #endregion

        #region AboutCommand

        public SimpleCommand<object, object> AboutCommand
        {
            get { return _about ?? (_about = new SimpleCommand<object, object>(arg => ShowAboutView())); }
        }

        SimpleCommand<object, object> _about;

        #endregion

        void ShowAboutView()
        {
            var aboutViewModel = new AboutViewModel();
            _visualizerService.ShowDialog("TLVAboutView", aboutViewModel);
        } 
#endif

        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldChanged)]
        void TransmissionLossFieldChanged(TransmissionLossField transmissionLossField) { lock (this) SelectedTransmissionLossFieldName = transmissionLossField.Name; }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
            //_iAmInitialized = true;
        }

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nMainViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

#if false
        #region public CASSOutput CASSOutput { get; set; }

        public CASSOutput CASSOutput
        {
            get { return _cassOutput; }
            set
            {
                if (_cassOutput == value) return;
                _cassOutput = value;
                NotifyPropertyChanged(CASSOutputChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CASSOutputChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.CASSOutput);
        CASSOutput _cassOutput;

        #endregion

        void OpenCASSFile(string filename)
        {
            CASSOutput = CASSOutput.FromBinaryFile(filename, false);
            var tlf = TransmissionLossField.FromCASS(CASSOutput);
            MediatorMessage.Send(MediatorMessage.TransmissionLossFieldChanged, tlf);
        }
        
#endif
    }
}
