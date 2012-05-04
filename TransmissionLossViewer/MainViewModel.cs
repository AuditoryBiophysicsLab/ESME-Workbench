using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Windows;
using ESME.Scenarios;
using ESME.Views.TransmissionLossViewer;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.Common;
using MEFedMVVM.ViewModelLocator;
using ESME.Locations;

namespace TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossViewerMainViewModel")]
    class MainViewModel : ViewModelBase
    {
        readonly string _databaseDirectory = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"ESME Workbench\Database");
        readonly IHRCSaveFileService _saveFileService;
        public IMasterDatabaseService Database { get; private set; }
        [Initialize] public ObservableList<AnalysisPoint> AnalysisPoints { get; set; }
        [Initialize] public ObservableList<Tuple<string, TransmissionLoss>> AnalysisPointModes { get; set; }
        [Initialize] public ObservableList<Radial> Radials { get; set; }
        public string TitleString { get; set; }
        public int RadialCount { get; set; }
        [Initialize] public TransmissionLossViewModel TransmissionLossViewModel { get; set; }

        #region public Scenario Scenario { get; set; }
        private Scenario _scenario;
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                _scenario = value;
                if (_scenario != null) AnalysisPoint = _scenario.AnalysisPoints.FirstOrDefault();
            }
        } 
        #endregion

        #region public AnalysisPoint AnalysisPoint { get; set; }
        private AnalysisPoint _analysisPoint;
        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                _analysisPoint = value;
                if (_analysisPoint != null) TransmissionLossViewModel.TransmissionLoss = _analysisPoint.TransmissionLosses.First();
            }
        } 
        #endregion

        [ImportingConstructor]
        public MainViewModel(IHRCSaveFileService saveFileService, IViewAwareStatus viewAwareStatus, IMasterDatabaseService database)
        {
            Database = database;
            Database.MasterDatabaseDirectory = _databaseDirectory;
            _saveFileService = saveFileService;
            if (!Designer.IsInDesignMode)
            {
                viewAwareStatus.ViewLoaded += () => { TransmissionLossViewModel.RadialViewModel = new RadialViewModel(((Window) viewAwareStatus.View).FindChildren<RadialView>().First()); };
                viewAwareStatus.ViewActivated += () => Database.Refresh();
            }
        }

        #region Commands
        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand
        {
            get
            {
                return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(vcArgs =>
                {
                    Database.Context.SaveChanges();
                    Properties.Settings.Default.Save();
                    Database.Dispose();
                }));
            }
        }
        SimpleCommand<object, EventToCommandArgs> _viewClosing;
        #endregion

        #region SaveAsCommand
        SimpleCommand<object, object> _saveAs;

        public SimpleCommand<object, object> SaveAsCommand
        {
            get
            {
                return _saveAs ?? (_saveAs = new SimpleCommand<object, object>(delegate
                {
                    return Scenario != null;
                }, delegate
                {
                    _saveFileService.Filter = "Portable Network Graphics (*.png)|*.png|Bitmap (*.bmp)|*.bmp|GIF (*.gif)|*.gif|JPEG (*.jpg)|*.jpg|TIFF (*.tiff)|*.tiff";
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = TransmissionLossViewModel.RadialViewModel.OutputFileName;

                    var result = _saveFileService.ShowDialog();
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        TransmissionLossViewModel.RadialViewModel.SaveAsImage(_saveFileService.FileName);
                    }
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
                    return Scenario != null;
                }, delegate
                {
                    _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = TransmissionLossViewModel.RadialViewModel.OutputFileName;
                    var result = _saveFileService.ShowDialog();
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        TransmissionLossViewModel.RadialViewModel.SaveAsCSV(_saveFileService.FileName);
                    }
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        #region CopyTextToClipboardCommand

        public SimpleCommand<object, object> CopyTextToClipboardCommand
        {
            get
            {
                return _copyTextToClipboard ??
                       (_copyTextToClipboard =
                        new SimpleCommand<object, object>(delegate
                                                              {
                                                                  return Scenario !=null;
                                                              },
                                                          delegate { Clipboard.SetText(TransmissionLossViewModel.RadialViewModel.ToCSV()); }));
            }
        }

        private SimpleCommand<object, object> _copyTextToClipboard;
        #endregion

        #region CopyImageToClipboardCommand

        public SimpleCommand<object, object> CopyImageToClipboardCommand
        {
            get
            {
                return _copyImageToClipboard ??
                       (_copyImageToClipboard =
                        new SimpleCommand<object, object>(delegate { return Scenario !=null; },
                                                          delegate { Clipboard.SetImage(TransmissionLossViewModel.RadialViewModel.ToBitmapSource()); }));
            }
        }

        private SimpleCommand<object, object> _copyImageToClipboard;
        #endregion

        #region CloseCommand
        public SimpleCommand<object, object> CloseCommand
        {
            get
            {
                return _close ??
                       (_close =
                        new SimpleCommand<object, object>(o=>Application.Current.Shutdown()));
            }
        }
        private SimpleCommand<object, object> _close;
        #endregion
        #endregion
    }
}