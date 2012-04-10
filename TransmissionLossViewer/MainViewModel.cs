using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
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
using HRC.Utility;
using MEFedMVVM.ViewModelLocator;
using ESME.Locations;
using AnalysisPoint = ESME.Scenarios.AnalysisPoint;


namespace TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossViewerMainViewModel")]
    [NotifyPropertyChanged]
    class MainViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly string _databaseDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Database");
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
               AnalysisPoints.Clear();
                AnalysisPoints.AddRange(from p in Database.Context.AnalysisPoints
                                        where p.Scenario.Guid == _selectedScenario.Guid
                                        select p);

            }
        }
        
        private Radial _selectedRadial;
        public Radial SelectedRadial
        {
            get { return _selectedRadial; }
            set
            {
                _selectedRadial = value;
                if (_selectedRadial == null)
                {
                    TitleString = "Transmission Loss Viewer: <no radial selected>";
                    return;
                }
                TitleString = string.Format("Transmission Loss Viewer: radial bearing {0:000.0} degrees", _selectedRadial.Bearing);
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, _selectedRadial);
            }
        }
        
        private AnalysisPoint _selectedAnalysisPoint;
        public AnalysisPoint SelectedAnalysisPoint
        {
            get { return _selectedAnalysisPoint; }
            set
            {
                _selectedAnalysisPoint = value;
                if (_selectedAnalysisPoint == null) return;
#if false
                Modes.Clear();
                Modes.AddRange((from m in Modes
                                where m.Source.Platform.Scenario.Guid == _selectedAnalysisPoint.Scenario.Guid
                                where m
                                select m).Distinct()); 
#endif
                AnalysisPointModes.Clear();
                foreach (var tl in from t in Database.Context.TransmissionLosses
                         where t.AnalysisPoint.Guid == _selectedAnalysisPoint.Guid
                         select t)
                {
                    AnalysisPointModes.Add(Tuple.Create(string.Format("{0}:{1}:{2}",tl.Mode.Source.Platform.PlatformName,tl.Mode.Source.SourceName,tl.Mode.ModeName),tl));
                }
              //  Radials.Clear();
             //   Radials.AddRange(from r in Database.Context.Radials
             //                        where r.TransmissionLoss.AnalysisPoint.Guid == _selectedAnalysisPoint.Guid
             //                        select r);
            }
        }

        private Tuple<string, TransmissionLoss> _selectedMode;
        public Tuple<string,TransmissionLoss> SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                _selectedMode = value;
                if (_selectedMode == null) return;
                Radials.Clear();
                Radials.AddRange(from r in Database.Context.Radials
                                 where r.TransmissionLoss.Guid == _selectedMode.Item2.Guid
                                 select r);
                RadialCount = Radials.Count;
                SelectedRadialIndex = 0;
                SelectedRadial = Radials.First();
                
                
            }
        }

        [Initialize]
        public ObservableList<Radial> Radials { get; set; }
        public int RadialCount { get; set; }
        [Initialize]    
        public ObservableList<AnalysisPoint> AnalysisPoints { get; set; }

        [Initialize]
        public ObservableList<Tuple<string, TransmissionLoss>> AnalysisPointModes { get; set; }
       

        private int _selectedRadialIndex;
        public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                _selectedRadialIndex = value;
                SelectedRadial = Radials[_selectedRadialIndex];
            }
        }

        public string SelectedAnalysisPointDisplayString { get; set; }
        public string TitleString { get; set; }

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
            TitleString = "Transmission Loss Viewer: <no radial selected>";

        }

        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    var ea = (CancelEventArgs)vcArgs.EventArgs;
                    Properties.Settings.Default.Save();
                    Database.Dispose();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        #region SaveAsCommand

        private string _outputFileName;
        public string OutputFileName
        {
            get
            {
                lock (this)
                {
                    if (SelectedRadial == null) return null;
                    var fieldName = SelectedRadial.TransmissionLoss.Mode.ModeName;
                    return Path.Combine(Properties.Settings.Default.ExperimentReportDirectory, fieldName + string.Format(" radial {0} degrees", SelectedRadial.Bearing));
                }
                return _outputFileName;
            }
            set { _outputFileName = value; }
        }

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
                    MediatorMessage.Send(MediatorMessage.ResetSelectedField, true);
                    //Keyboard.Focus((Window)_viewAwareStatus.View);
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
                        MediatorMessage.Send(MediatorMessage.SaveRadial, _saveFileService.FileName);

                    }
                    MediatorMessage.Send(MediatorMessage.ResetSelectedField, true);
                    //Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        #region public string SelectedBearingGeometry { get; set; }

        public string SelectedBearingGeometry
        {
            get
            {
                if (SelectedRadial == null) return null;
                var sb = new StringBuilder();
                const double radius = 8;
                double x, y;
                for (double angle = 0; angle <= 2 * Math.PI; angle += Math.PI / 32.0)
                {
                    sb.Append(sb.Length == 0 ? "M" : "L");
                    x = (Math.Sin(angle) * radius) + radius;
                    y = (Math.Cos(angle) * radius) + radius;
                    sb.Append(string.Format(" {0:0.###},{1:0.###} ", x, y));
                }
                sb.Append(string.Format("M {0:0.###}, {0:0.###} ", radius));
                x = (Math.Sin(SelectedRadial.Bearing * (Math.PI / 180)) * radius) + radius;
                y = (-Math.Cos(SelectedRadial.Bearing * (Math.PI / 180)) * radius) + radius;
                sb.Append(string.Format("L {0:0.###},{1:0.###} ", x, y));
                return sb.ToString();
            }
        }

        static readonly PropertyChangedEventArgs SelectedBearingGeometryChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedBearingGeometry);

        #endregion

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
    }
}
