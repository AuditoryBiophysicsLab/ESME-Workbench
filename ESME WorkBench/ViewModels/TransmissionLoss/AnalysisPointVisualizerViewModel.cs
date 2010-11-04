using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Views;
using HRC.Services;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    class AnalysisPointVisualizerViewModel: ViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        readonly IHRCSaveFileService _saveFileService;

        #region public constructor

        public AnalysisPointVisualizerViewModel(AnalysisPoint analysisPoint, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            _saveFileService = saveFileService;

            MediatorMessage.Send(MediatorMessage.AnalysisPointChanged, analysisPoint);
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

        #region CloseWindowCommand

        SimpleCommand<object, object> _closeWindow;

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get { return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate { ((AnalysisPointVisualizerView)_viewAwareStatus.View).Close(); })); }
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
                    BitmapEncoder encoder = null;
                    _saveFileService.Filter = "Portable Network Graphics (*.png)|*.png| JPEG (*.jpg)|*.jpg|Bitmap (*.bmp)|*.bmp";
                    //_saveFileService.Filter = "Portable Network Graphics (*.png)|*.png";
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = null;
                    _saveFileService.InitialDirectory = Properties.Settings.Default.LastImageExportFileDirectory;
                    _saveFileService.FileName = null;
                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);

#if true //todo graham turn this back on when it works.)
                        switch (Path.GetExtension(_saveFileService.FileName).ToLower())
                        {
                            case ".jpg":
                            case ".jpeg":
                                encoder = new JpegBitmapEncoder();
                                break;
                            case ".png":
                                encoder = new PngBitmapEncoder();
                                break;
                            case ".bmp":
                                encoder = new BmpBitmapEncoder();
                                break;
                        }
#endif

                        if (encoder == null) return;

                        var theView = ((TransmissionLossFieldView)_viewAwareStatus.View).RadialView;
                        var bmp = new RenderTargetBitmap((int)theView.ActualWidth, (int)theView.ActualHeight, 96, 96, PixelFormats.Pbgra32);
                        bmp.Render(theView);
                        encoder.Frames.Add(BitmapFrame.Create(bmp));
                        using (var stream = new FileStream(_saveFileService.FileName, FileMode.Create)) encoder.Save(stream);
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
                    _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                    _saveFileService.FileName = null;
                    _saveFileService.InitialDirectory = Properties.Settings.Default.LastCSVExportFileDirectory;
                    _saveFileService.OverwritePrompt = true;
                    _saveFileService.FileName = null;
                    var result = _saveFileService.ShowDialog((Window)_viewAwareStatus.View);
                    if (result.HasValue && result.Value)
                    {
                        Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
                        MediatorMessage.Send(MediatorMessage.SaveRadialAsCSV, _saveFileService.FileName);
                    }
                }));
            }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        [MediatorMessageSink(MediatorMessage.SetSelectedRadialBearing)]
        void SetSelectedRadialBearing(double selectedRadialBearing) { SelectedRadialBearing = selectedRadialBearing; }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService) 
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }
        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAnalysisPointVisualizerViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }
    }
}
