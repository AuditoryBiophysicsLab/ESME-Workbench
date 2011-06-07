using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.Environment;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.TransmissionLossViewer;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("TwoDimensionColorMapViewModel")]
    public class TwoDimensionColorMapViewModel : ViewModelBase
    {
          static readonly PropertyChangedEventArgs WriteableBitmapChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.WriteableBitmap);

        #region public float RangeMin { get; set; }

        static readonly PropertyChangedEventArgs RangeMinChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.RangeMin);
        float _rangeMin;

        public float RangeMin
        {
            get { return _rangeMin; }
            set
            {
                if (_rangeMin == value) return;
                _rangeMin = value;
                NotifyPropertyChanged(RangeMinChangedEventArgs);
                //CenterX = (RangeMax - RangeMin) / 2;
            }
        }

        #endregion

        #region public float  RangeMax { get; set; }

        static readonly PropertyChangedEventArgs RangeMaxChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.RangeMax);
        float _rangeMax;

        public float RangeMax
        {
            get { return _rangeMax; }
            set
            {
                if (_rangeMax == value) return;
                _rangeMax = value;
                NotifyPropertyChanged(RangeMaxChangedEventArgs);
                //CenterX = (RangeMax - RangeMin) / 2;
            }
        }

        #endregion
        
        #region public float DepthMin { get; set; }

        static readonly PropertyChangedEventArgs DepthMinChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.DepthMin);
        float _depthMin;

        public float DepthMin
        {
            get { return _depthMin; }
            set
            {
                if (_depthMin == value) return;
                _depthMin = value;
                NotifyPropertyChanged(DepthMinChangedEventArgs);
                //CenterY = (DepthMax - DepthMin) / 2;
            }
        }

        #endregion

        #region public float  DepthMax { get; set; }

        static readonly PropertyChangedEventArgs DepthMaxChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.DepthMax);
        float _depthMax;

        public float DepthMax
        {
            get { return _depthMax; }
            set
            {
                if (_depthMax == value) return;
                _depthMax = value;
                NotifyPropertyChanged(DepthMaxChangedEventArgs);
               // CenterY = (DepthMax - DepthMin) / 2;
            }
        }

        #endregion

        #region public TransmissionLossField TransmissionLossField { get; set; }

        public TransmissionLossField TransmissionLossField
        {
            get { return _transmissionLossField; }
            set
            {
                if (_transmissionLossField == value) return;
                _transmissionLossField = value;
                NotifyPropertyChanged(TransmissionLossFieldChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs = ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.TransmissionLossField);
        TransmissionLossField _transmissionLossField;

        #endregion

        
        readonly Dispatcher _dispatcher;
        readonly IViewAwareStatus _viewAwareStatus;
        bool _iAmInitialized;
        bool _isRendered;
        TransmissionLossRadial _tempRadial;
        WriteableBitmap _writeableBitmap;
        EarthCoordinate _location;


        [ImportingConstructor]
        public TwoDimensionColorMapViewModel(IViewAwareStatus viewAwareStatus)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            _dispatcher = Dispatcher.CurrentDispatcher;
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossRadialColorMapChanged, ColorMapViewModel.Default);
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.TransmissionLossRadialViewInitialized, true);
            _viewAwareStatus.ViewLoaded += () => MediatorMessage.Send(MediatorMessage.RequestTransmissionLossBathymetry, true);
            
        }

        public WriteableBitmap WriteableBitmap
        {
            get
            {
                if (!_isRendered) RenderBitmap();
                return _writeableBitmap;
            }
        }

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTwoDimensionColorMapViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialColorMapChanged)]
        void TransmissionLossRadialColorMapChanged(ColorMapViewModel colorMapViewModel) { ColorMapViewModel = colorMapViewModel; }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialEarthCoordinate)]
        void TransmissionLossRadialEarthCoordinate(EarthCoordinate location) { _location = location; }
        
        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialChanged)]
        void TransmissionLossRadialChanged(TransmissionLossRadial transmissionLossRadial)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("TwoDimensionColorMapViewModel: Initializing transmission loss radial");
               // TransmissionLossRadial = transmissionLossRadial;
                //if (_bathymetry == null) 
                MediatorMessage.Send(MediatorMessage.RequestTransmissionLossBathymetry,true);
            }
            else
            {
                Debug.WriteLine("TwoDimensionColorMapViewModel: Deferring initialization of transmission loss radial");
                _tempRadial = transmissionLossRadial;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialViewInitialized)]
        void TransmissionLossRadialViewInitialized(bool dummy)
        {
            _iAmInitialized = true;
            if (_tempRadial != null)
            {
               // TransmissionLossRadial = _tempRadial;
                Debug.WriteLine("TwoDimensionColorMapViewModel: Deferred initialization of transmission loss field radial");
                MediatorMessage.Send(MediatorMessage.RequestTransmissionLossBathymetry,true);
                
            }
        }

        [MediatorMessageSink(MediatorMessage.SaveRadialBitmap)]
        void SaveRadialBitmap(string fileName)
        {
            BitmapEncoder encoder = null;

            switch (Path.GetExtension(fileName).ToLower())
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


            if (encoder == null) return;


            var theView = ((TwoDimensionColorMapView)_viewAwareStatus.View);
            var bmp = new RenderTargetBitmap((int)theView.ActualWidth, (int)theView.ActualHeight, 96, 96, PixelFormats.Pbgra32);
            bmp.Render(theView);
            encoder.Frames.Add(BitmapFrame.Create(bmp));
            using (var stream = new FileStream(fileName, FileMode.Create)) encoder.Save(stream);
        }

        float[,] TransmissionLoss { get; set; }

        void RenderBitmap()
        {
            if (TransmissionLossField == null || ColorMapViewModel == null) return;

           // var width = TransmissionLossRadial.Ranges.Length;
            //var height = TransmissionLossRadial.Depths.Length;
            var radius = TransmissionLossField.Ranges.Length;
            if (_writeableBitmap == null) _writeableBitmap = new WriteableBitmap(radius, radius, 96, 96, PixelFormats.Bgr32, null);

            _writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)_writeableBitmap.BackBuffer;
                for (int y = 0; y < radius; y++)
                {
                    for (int x = 0; x < radius; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        var curColor = _colorMapViewModel.Lookup(TransmissionLoss[y, x]);
                        *((int*)curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                        curOffset += sizeof(Int32);
                    }
                }
            }
            _writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, radius, radius));
            _writeableBitmap.Unlock();
            _isRendered = true;
            _dispatcher.BeginInvoke(new VoidDelegate(RenderFinished), DispatcherPriority.ApplicationIdle);
        }

        void RenderFinished() { NotifyPropertyChanged(WriteableBitmapChangedEventArgs); }

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.ColorMapViewModel);
        ColorMapViewModel _colorMapViewModel;

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel; }
            set
            {
                if (_colorMapViewModel == value) return;
                _colorMapViewModel = value;
                _colorMapViewModel.PropertyChanged += ColorMapViewModelPropertyChanged;
                NotifyPropertyChanged(ColorMapViewModelChangedEventArgs);
            }
        }

        void ColorMapViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            switch (e.PropertyName)
            {
                case "CurMinValue":
                case "CurMaxValue":
                    _isRendered = false;
                    NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
                    break;
            }
        }

        #endregion
        
        #region Nested type: VoidDelegate

        delegate void VoidDelegate();

        #endregion
  
    }
}