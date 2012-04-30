using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using ESME.TransmissionLoss;
using HRC.Aspects;
using HRC.Services;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Controls
{
    [ExportViewModel("TwoDimensionColorMapViewModel")]
    public class TwoDimensionColorMapViewModel : ViewModelBase
    {
        #region public float RangeMin { get; set; }
        public float RangeMin { get; set; }
        #endregion

        #region public float  RangeMax { get; set; }
        public float RangeMax { get; set; }
        #endregion

        #region public float DepthMin { get; set; }
        public float DepthMin { get; set; }
        #endregion

        #region public float  DepthMax { get; set; }
        public float DepthMax { get; set; }
        #endregion

        #region public TransmissionLossField TransmissionLossField { get; set; }
        public TransmissionLossField TransmissionLossField { get; set; }
        #endregion

        private readonly Dispatcher _dispatcher;
        private readonly IViewAwareStatus _viewAwareStatus;
        //private bool _iAmInitialized;
        private bool _isRendered;
        //private Geo _location;
        //private TransmissionLossRadial _tempRadial;
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
            var radius = TransmissionLossField.Ranges.Count;
            if (_writeableBitmap == null) _writeableBitmap = new WriteableBitmap(radius, radius, 96, 96, PixelFormats.Bgr32, null);

            _writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int) _writeableBitmap.BackBuffer;
                for (var y = 0; y < radius; y++)
                {
                    for (var x = 0; x < radius; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        var curColor = _colorMapViewModel.Lookup(SliceData[y, x]);
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
            OnPropertyChanged("WritableBitmap");
        }

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        private ColorMapViewModel _colorMapViewModel;

        public ColorMapViewModel ColorMapViewModel
        {
            get { return _colorMapViewModel; }
            set
            {
                _colorMapViewModel = value;
                _colorMapViewModel.PropertyChanged += (s, e) =>
                {
                    switch (e.PropertyName)
                    {
                        case "CurMinValue":
                        case "CurMaxValue":
                            _isRendered = false;
                            OnPropertyChanged("WritableBitmap");
                            break;
                    }
                };
            }
        }
        #endregion

        #region public float[,] SliceData { get; set; }

        private float[,] _sliceData;

        public float[,] SliceData
        {
            get { return _sliceData; }
            set
            {
                _sliceData = value;
                _isRendered = false;
            }
        }

        #endregion

        #region Nested type: VoidDelegate

        private delegate void VoidDelegate();

        #endregion
    }
}