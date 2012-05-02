using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME;
using ESME.Scenarios;
using ESME.Views.TransmissionLossViewer;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;
using ESME.Locations;

namespace TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossViewerMainViewModel")]
    class MainViewModel : ViewModelBase
    {
        readonly string _databaseDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"ESME Workbench\Database");
        readonly IHRCSaveFileService _saveFileService;
        public MasterDatabaseService Database { get; private set; }
        [Initialize]
        public ObservableList<AnalysisPoint> AnalysisPoints { get; set; }
        [Initialize]
        public ObservableList<Tuple<string, TransmissionLoss>> AnalysisPointModes { get; set; }
        [Initialize]
        public ObservableList<Radial> Radials { get; set; }

        public TransmissionLossRadialViewModel TransmissionLossRadialViewModel { get; set; }

        #region public Scenario SelectedScenario {get; set;}
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
        #endregion

        #region public AnalysisPoint SelectedAnalysisPoint {get; set;}
        private AnalysisPoint _selectedAnalysisPoint;
        public AnalysisPoint SelectedAnalysisPoint
        {
            get { return _selectedAnalysisPoint; }
            set
            {
                _selectedAnalysisPoint = value;
                if (_selectedAnalysisPoint == null) return;
                AnalysisPointModes.Clear();
                foreach (var tl in from t in Database.Context.TransmissionLosses
                                   where t.AnalysisPoint.Guid == _selectedAnalysisPoint.Guid
                                   select t)
                {
                    AnalysisPointModes.Add(Tuple.Create(string.Format("{0}:{1}:{2}", tl.Mode.Source.Platform.PlatformName, tl.Mode.Source.SourceName, tl.Mode.ModeName), tl));
                }
            }
        }
        #endregion

        #region public Tuple<string,TransmissionLoss> SelectedMode {get;set;}
        private Tuple<string, TransmissionLoss> _selectedMode;
        public Tuple<string, TransmissionLoss> SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                _selectedMode = value;
                if (_selectedMode == null)
                {
                    SelectedRadial = null;
                    return;
                }
                Radials.Clear();
                Radials.AddRange(from r in Database.Context.Radials
                                 where r.TransmissionLoss.Guid == _selectedMode.Item2.Guid
                                 orderby r.Bearing
                                 select r);
                RadialCount = Radials.Count - 1;
                SelectedRadial = Radials.First();
                SelectedRadialIndex = 0;
            }
        }
        #endregion

        #region public string SelectedBearingGeometry { get; }
        public string SelectedBearingGeometry
        {
            get
            {
                Debug.WriteLine("SelectedBearingGeometry Updated");
                var sb = new StringBuilder();
                var bearing = 0.0;
                if (SelectedRadial != null) bearing = SelectedRadial.Bearing;
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
                x = (Math.Sin(bearing * (Math.PI / 180)) * radius) + radius;
                y = (-Math.Cos(bearing * (Math.PI / 180)) * radius) + radius;
                sb.Append(string.Format("L {0:0.###},{1:0.###} ", x, y));
                return sb.ToString();
            }
        }
        #endregion

        #region public Radial SelectedRadial {get; set;}
        private Radial _selectedRadial;
        [Affects("SelectedBearingGeometry")]
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
                TransmissionLossRadialViewModel.Radial = _selectedRadial;
            }
        }
        #endregion

        #region public int SelectedRadialIndex {get; set;}
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
        #endregion

        public string TitleString { get; set; }
        public int RadialCount { get; set; }

        [ImportingConstructor]
        public MainViewModel(IHRCSaveFileService saveFileService, IViewAwareStatus viewAwareStatus, MasterDatabaseService database)
        {
            RegisterMediator();
            Database = database;
            Database.MasterDatabaseDirectory = _databaseDirectory;
            _saveFileService = saveFileService;
            viewAwareStatus.ViewLoaded += () => { TransmissionLossRadialViewModel = new TransmissionLossRadialViewModel(((MainView)viewAwareStatus.View).TransmissionLossRadialView); };
        }

        #region Commands
        #region ViewClosingCommand

        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    Properties.Settings.Default.Save();
                    Database.Dispose();
                }));
            }
        }

        SimpleCommand<object, EventToCommandArgs> _viewClosing;

        #endregion

        #region SaveAsCommand


        public string OutputFileName
        {
            get
            {
                lock (this)
                {
                    if (SelectedRadial == null) return null;
                    return Path.Combine(Properties.Settings.Default.ExperimentReportDirectory, string.Format("{0} {1} {2} bearing {3} degrees", SelectedRadial.TransmissionLoss.Mode.ModeName, SelectedRadial.TransmissionLoss.Mode.Source.SourceName, SelectedRadial.TransmissionLoss.Mode.Source.Platform.PlatformName, SelectedRadial.Bearing));
                }

            }
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

                    var result = _saveFileService.ShowDialog();
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        TransmissionLossRadialViewModel.SaveRadialBitmap(_saveFileService.FileName);
                    }
                    
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

                    var result = _saveFileService.ShowDialog();
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        TransmissionLossRadialViewModel.SaveAsCSV(_saveFileService.FileName);
                    }
                    
                    //Keyboard.Focus((Window)_viewAwareStatus.View);
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion
        #endregion

        #region aging viewaware and mediator registrations
        //public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService) { _viewAwareStatus = viewAwareStatusService; }

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
        #endregion
    }
}