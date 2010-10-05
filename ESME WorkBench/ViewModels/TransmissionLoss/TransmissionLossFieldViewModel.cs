using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESMEWorkBench.Properties;
using ESMEWorkBench.Views;
using MEFedMVVM.Services.Contracts;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class TransmissionLossFieldViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        private readonly ISaveFileService _saveFileService;
        IViewAwareStatus _viewAwareStatus;
        bool _fieldInitialized,
       _radialInitialized;
        
        public TransmissionLossFieldViewModel(string fileName, ISaveFileService saveFileService )
        {
            RegisterMediator();
            TransmissionLossField = TransmissionLossField.Load(fileName);
            
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            SelectedRadial = 1;
        }

        public TransmissionLossFieldViewModel(TransmissionLossField transmissionLossField, ISaveFileService saveFileService )
        {
            RegisterMediator();
            TransmissionLossField = transmissionLossField;
            ColorMapViewModel = ColorMapViewModel.Default;
            _saveFileService = saveFileService;
            SelectedRadial = 1;
        }

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

        public TransmissionLossField TransmissionLossField { get; private set; }

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

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

        static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.ColorMapViewModel);
        ColorMapViewModel _colorMapViewModel;

        #endregion

        public int RadialCount { get { return TransmissionLossField.Radials.Length; } }

        int _selectedRadial;
        static readonly PropertyChangedEventArgs SelectedRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.SelectedRadial);
        public int SelectedRadial
        { 
            get { return _selectedRadial; } 
            set
            {
                if (value == _selectedRadial) return;
                _selectedRadial = value;
                NotifyPropertyChanged(SelectedRadialChangedEventArgs);
                MediatorMessage.Send(MediatorMessage.TransmissionLossRadialChanged, TransmissionLossField.Radials[_selectedRadial -1]);
                //TransmissionLossRadialViewModel.TransmissionLossRadial = TransmissionLossField.Radials[_selectedRadial - 1];
               
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossRadialViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossFieldViewModel>(x => x.TransmissionLossRadialViewModel);
        TransmissionLossRadialViewModel _transmissionLossRadialViewModel;
        public TransmissionLossRadialViewModel TransmissionLossRadialViewModel {
            get { return _transmissionLossRadialViewModel; } 
            private set
            {
                if (value == _transmissionLossRadialViewModel) return;
                _transmissionLossRadialViewModel = value;
                NotifyPropertyChanged(TransmissionLossRadialViewModelChangedEventArgs);
                ColorMapViewModel.MaxValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadialViewModel.TransmissionLossRadial.StatMin;
            }
        }

        #region SaveAsCommand

        public SimpleCommand<object, object> SaveAsCommand
        {
            get
            {
                return _saveAs ??
                       (_saveAs =
                        new SimpleCommand<object, object>(
                            delegate
                            {
                                BitmapEncoder encoder = null;
                                _saveFileService.Filter = "Portable Network Graphics (*.png)|*.png| JPEG (*.jpg)|*.jpg|Bitmap (*.bmp)|*.bmp";
                                _saveFileService.OverwritePrompt = true;
                                var result = _saveFileService.ShowDialog((Window) _viewAwareStatus.View);
                                if (result.HasValue && result.Value)
                                {
                                    //_saveFileService.FileName
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
                                    
                                    if(encoder == null) return;
                                    var bmp = new RenderTargetBitmap(1024,768,96,96,PixelFormats.Pbgra32);
                                    bmp.Render(((TransmissionLossView)_viewAwareStatus.View).RadialView);
                                    encoder.Frames.Add(BitmapFrame.Create(bmp));
                                    using (var stream = new FileStream(_saveFileService.FileName, FileMode.Create)) encoder.Save(stream);
                                }
                                
                            }));
            }
        }
        

        private SimpleCommand<object, object> _saveAs;

        #endregion

        #region CloseWindowCommand

        public SimpleCommand<object, object> CloseWindowCommand
        {
            get { return _closeWindow ?? (_closeWindow = new SimpleCommand<object, object>(delegate 
                {
                    ((TransmissionLossView) _viewAwareStatus.View).Close();
                }
            )); }
        }

        SimpleCommand<object, object> _closeWindow;

        #endregion
        
        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService) 
        {
            _viewAwareStatus = viewAwareStatusService;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossFieldViewInitialized, true);
        }

    }
}