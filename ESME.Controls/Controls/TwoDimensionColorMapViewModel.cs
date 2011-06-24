using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using ESME.Views.TransmissionLossViewer;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("TwoDimensionColorMapViewModel")]
    public class TwoDimensionColorMapViewModel : ViewModelBase
    {
        private static readonly PropertyChangedEventArgs WriteableBitmapChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.WriteableBitmap);

        #region public float RangeMin { get; set; }

        private static readonly PropertyChangedEventArgs RangeMinChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.RangeMin);

        private float _rangeMin;

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

        private static readonly PropertyChangedEventArgs RangeMaxChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.RangeMax);

        private float _rangeMax;

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

        private static readonly PropertyChangedEventArgs DepthMinChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.DepthMin);

        private float _depthMin;

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

        private static readonly PropertyChangedEventArgs DepthMaxChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.DepthMax);

        private float _depthMax;

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

        private static readonly PropertyChangedEventArgs TransmissionLossFieldChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.TransmissionLossField);

        private TransmissionLossField _transmissionLossField;

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

        #endregion

        private readonly Dispatcher _dispatcher;
        private readonly IViewAwareStatus _viewAwareStatus;
        private bool _iAmInitialized;
        private bool _isRendered;
        private EarthCoordinate _location;
        private TransmissionLossRadial _tempRadial;
        private WriteableBitmap _writeableBitmap;


        [ImportingConstructor]
        public TwoDimensionColorMapViewModel(IViewAwareStatus viewAwareStatus)
        {
            RegisterMediator();
            _viewAwareStatus = viewAwareStatus;
            _dispatcher = Dispatcher.CurrentDispatcher;
        }

        public WriteableBitmap WriteableBitmap
        {
            get
            {
                if (!_isRendered) RenderBitmap();
                return _writeableBitmap;
            }
        }

        private void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nTwoDimensionColorMapViewModel: Mediator registration failed: " +
                                ex.Message + "\n***********");
                throw;
            }
        }

        [MediatorMessageSink(MediatorMessage.SaveRadialBitmap)]
        private void SaveRadialBitmap(string fileName)
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


            var theView = ((TwoDimensionColorMapView) _viewAwareStatus.View);
            var bmp = new RenderTargetBitmap((int) theView.ActualWidth, (int) theView.ActualHeight, 96, 96,
                                             PixelFormats.Pbgra32);
            bmp.Render(theView);
            encoder.Frames.Add(BitmapFrame.Create(bmp));
            using (var stream = new FileStream(fileName, FileMode.Create)) encoder.Save(stream);
        }

        private void RenderBitmap()
        {
            if (TransmissionLossField == null || ColorMapViewModel == null) return;

            // var width = TransmissionLossRadial.Ranges.Length;
            //var height = TransmissionLossRadial.Depths.Length;
            int radius = TransmissionLossField.Ranges.Count;
            if (_writeableBitmap == null)
                _writeableBitmap = new WriteableBitmap(radius, radius, 96, 96, PixelFormats.Bgr32, null);

            _writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int) _writeableBitmap.BackBuffer;
                for (int y = 0; y < radius; y++)
                {
                    for (int x = 0; x < radius; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        Color curColor = _colorMapViewModel.Lookup(SliceData[y, x]);
                        *((int*) curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) |
                                               (curColor.B));
                        curOffset += sizeof (Int32);
                    }
                }
            }
            _writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, radius, radius));
            _writeableBitmap.Unlock();
            _isRendered = true;
            _dispatcher.BeginInvoke(new VoidDelegate(RenderFinished), DispatcherPriority.ApplicationIdle);
        }

        private void RenderFinished()
        {
            NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
        }

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        private static readonly PropertyChangedEventArgs ColorMapViewModelChangedEventArgs =
            ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.ColorMapViewModel);

        private ColorMapViewModel _colorMapViewModel;

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

        private void ColorMapViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
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

        #region public float[,] SliceData { get; set; }

        private static readonly PropertyChangedEventArgs SliceDataChangedEventArgs =
            ObservableHelper.CreateArgs<TwoDimensionColorMapViewModel>(x => x.SliceData);

        private float[,] _sliceData;

        public float[,] SliceData
        {
            get { return _sliceData; }
            set
            {
                _sliceData = value;
                _isRendered = false;
                NotifyPropertyChanged(SliceDataChangedEventArgs);
            }
        }

        #endregion

        #region Nested type: VoidDelegate

        private delegate void VoidDelegate();

        #endregion
    }
}