using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Views;
using HRC.Services;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossFieldViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        static readonly PropertyChangedEventArgs SelectedRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadial);
        
        readonly IHRCSaveFileService _saveFileService;

        bool _fieldInitialized,
             _radialInitialized;

        int _selectedRadial;
        IViewAwareStatus _viewAwareStatus;

        public TransmissionLossFieldViewModel(string fileName, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            TransmissionLossField = TransmissionLossField.Load(fileName);

            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            SelectedRadial = 1;
        }

        public TransmissionLossFieldViewModel(TransmissionLossField transmissionLossField, IHRCSaveFileService saveFileService)
        {
            RegisterMediator();
            TransmissionLossField = transmissionLossField;
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            //SelectedRadial = 1;
        }

        public TransmissionLossField TransmissionLossField { get; private set; }

        public int RadialCount
        {
            get { return TransmissionLossField.Radials.Length; }
        }

        public int SelectedRadial
        {
            get { return _selectedRadial; }
            set
            {
                if (value == _selectedRadial) return;
                _selectedRadial = value;
                NotifyPropertyChanged(SelectedRadialChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, TransmissionLossField.Radials[_selectedRadial - 1]);
                SelectedRadialBearing = TransmissionLossField.Radials[_selectedRadial - 1].BearingFromSource;
                //TransmissionLossRadialViewModel.TransmissionLossRadial = TransmissionLossField.Radials[_selectedRadial - 1];
            }
        }

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
                                                                                       
                                                                                       var theView = ((TransmissionLossView) _viewAwareStatus.View).RadialView;
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
            get { return _exportAs ?? (_exportAs = new SimpleCommand<object, object>(delegate
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
                                                                                             TransmissionLossField.Radials[SelectedRadial].SaveAsCSV(_saveFileService.FileName, TransmissionLossField);
                                                                                         }
                                                                                     })); }
        }

        SimpleCommand<object, object> _exportAs;

        #endregion

        #region CloseWindowCommand

        SimpleCommand<object, object> _closeWindow;

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get { return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate { ((TransmissionLossView) _viewAwareStatus.View).Close(); })); }
        }

        #endregion

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.ColorMapViewModel);
        ColorMapViewModel _colorMapViewModel;

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel; }
            set
            {
                if (_colorMapViewModel == value) return;
                _colorMapViewModel = value;
                // TransmissionLossRadialViewModel.ColorMapViewModel = ColorMapViewModel;
                NotifyPropertyChanged(ColorMapViewModelChangedEventArgs);
            }
        }

        #endregion

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
        }

        #endregion

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTransmissionLossFieldViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossFieldViewInitialized)]
        void TransmissionLossFieldViewInitialized(bool dummy)
        {
            _fieldInitialized = true;
            InitializeIfViewModelsReady();
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialViewInitialized)]
        void TransmissionLossRadialViewInitialized(bool dummy)
        {
            _radialInitialized = true;
            InitializeIfViewModelsReady();
        }

        void InitializeIfViewModelsReady()
        {
            if (_fieldInitialized && _radialInitialized)
            {
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialColorMapChanged, ColorMapViewModel);
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, TransmissionLossField.Radials[0]);
            }
        }
    }
}