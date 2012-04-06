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
using ESME.Views.Controls;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossRadialViewModel")]
    public class TransmissionLossRadialViewModel : ViewModelBase
    {
        static readonly PropertyChangedEventArgs WriteableBitmapChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.WriteableBitmap);

        #region public float RangeMin { get; set; }

        static readonly PropertyChangedEventArgs RangeMinChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.RangeMin);
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

        static readonly PropertyChangedEventArgs RangeMaxChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.RangeMax);
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

        static readonly PropertyChangedEventArgs DepthMinChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.DepthMin);
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

        static readonly PropertyChangedEventArgs DepthMaxChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.DepthMax);
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

        #region public Bathymetry Bathymetry { get; set; }

        static readonly PropertyChangedEventArgs BathymetryChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.Bathymetry);
        Bathymetry _bathymetry;

        public Bathymetry Bathymetry
        {
            get { return _bathymetry; }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                NotifyPropertyChanged(BathymetryChangedEventArgs);
            }
        }

        #endregion

        #region GridSizeChangedCommand

        SimpleCommand<object, object> _gridSizeChanged;

        public SimpleCommand<object, object> GridSizeChangedCommand
        {
            get
            {
                return _gridSizeChanged ?? (_gridSizeChanged = new SimpleCommand<object, object>(delegate
                                                                                                 {
                                                                                                     if(_bathymetry == null) MediatorMessage.Send(MediatorMessage.RequestTransmissionLossBathymetry,true);
                                                                                                     else
                                                                                                     {
                                                                                                         if (TransmissionLossRadial != null) CalculateBottomProfileGeometry();
                                                                                                     }
                                                                                                    
                                                                                                 }));
            }
        }

        #endregion

        readonly Dispatcher _dispatcher;
        readonly IViewAwareStatus _viewAwareStatus;
        bool _iAmInitialized;
        bool _isRendered;
        TransmissionLossRadial _tempRadial;
        WriteableBitmap _writeableBitmap;
        Geo _location;


        [ImportingConstructor]
        public TransmissionLossRadialViewModel(IViewAwareStatus viewAwareStatus)
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
                Debug.WriteLine("***********\nTransmissionLossRadialViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialColorMapChanged)]
        void TransmissionLossRadialColorMapChanged(ColorMapViewModel colorMapViewModel) { ColorMapViewModel = colorMapViewModel; }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialEarthCoordinate)]
        void TransmissionLossRadialEarthCoordinate(Geo location) { _location = location; }

        [MediatorMessageSink(MediatorMessage.SetTransmissionLossBathymetry)]
        void SetTransmissionLossBathymetry(Bathymetry bathymetry)
        {
            _bathymetry = bathymetry;
            if (TransmissionLossRadial != null) CalculateBottomProfileGeometry();
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialChanged)]
        void TransmissionLossRadialChanged(TransmissionLossRadial transmissionLossRadial)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
                TransmissionLossRadial = transmissionLossRadial;
                //if (_bathymetry == null) 
                MediatorMessage.Send(MediatorMessage.RequestTransmissionLossBathymetry,true);
            }
            else
            {
                Debug.WriteLine("TransmissionLossRadialViewModel: Deferring initialization of transmission loss radial");
                _tempRadial = transmissionLossRadial;
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialViewInitialized)]
        void TransmissionLossRadialViewInitialized(bool dummy)
        {
            _iAmInitialized = true;
            if (_tempRadial != null)
            {
                TransmissionLossRadial = _tempRadial;
                Debug.WriteLine("TransmissionLossRadialViewModel: Deferred initialization of transmission loss field radial");
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


            var theView = ((TransmissionLossRadialView)_viewAwareStatus.View);
            var bmp = new RenderTargetBitmap((int)theView.ActualWidth, (int)theView.ActualHeight, 96, 96, PixelFormats.Pbgra32);
            bmp.Render(theView);
            encoder.Frames.Add(BitmapFrame.Create(bmp));
            using (var stream = new FileStream(fileName, FileMode.Create)) encoder.Save(stream);
        }

        void RenderBitmap()
        {
            if (TransmissionLossRadial == null || ColorMapViewModel == null) return;

            var width = TransmissionLossRadial.Ranges.Count;
            var height = TransmissionLossRadial.Depths.Count;

            if (_writeableBitmap == null) _writeableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null);
            var infinityColor = _colorMapViewModel.Colors[0];
            _writeableBitmap.Lock();
            unsafe
            {
                var curOffset = (int)_writeableBitmap.BackBuffer;
                for (var y = 0; y < height; y++)
                {
                    for (var x = 0; x < width; x++)
                    {
                        // Draw from the bottom up, which matches the default render order.  This may change as the UI becomes
                        // more fully implemented, especially if we need to flip the canvas and render from the top.  Time will tell.
                        var curValue = TransmissionLossRadial.TransmissionLoss[y, x];
                        var curColor = float.IsInfinity(curValue) ? infinityColor : _colorMapViewModel.Lookup(curValue);
                        *((int*)curOffset) = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                        curOffset += sizeof(Int32);
                    }
                }
            }
            _writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
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

        #region public TransmissionLossRadial TransmissionLossRadial { get; set; }

        static readonly PropertyChangedEventArgs TransmissionLossRadialChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.TransmissionLossRadial);
        TransmissionLossRadial _transmissionLossRadial;

        public TransmissionLossRadial TransmissionLossRadial
        {
            get { return _transmissionLossRadial; }
            set
            {
                if (_transmissionLossRadial == value) return;
                _transmissionLossRadial = value;
                _isRendered = false;
                _writeableBitmap = null;
                RangeMin = TransmissionLossRadial.Ranges.First();
                RangeMax = TransmissionLossRadial.Ranges.Last();
                DepthMin = TransmissionLossRadial.Depths.First();
                DepthMax = TransmissionLossRadial.Depths.Last();
                // ColorMapViewModel.MinValue = TransmissionLossRadial.StatMin;
                ColorMapViewModel.MaxValue = TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadial.StatMin;
                NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs);
                RenderBitmap();
            }
        }

        #endregion

        #region public string BottomProfileGeometry { get; set; }

        static readonly PropertyChangedEventArgs BottomProfileGeometryChangedEventArgs = ObservableHelper.CreateArgs<TransmissionLossRadialViewModel>(x => x.BottomProfileGeometry);
        string _bottomProfileGeometry = "M 0,0";

        public string BottomProfileGeometry
        {
            get { return _bottomProfileGeometry; }
            private set
            {
                if (_bottomProfileGeometry == value) return;
                _bottomProfileGeometry = value;
                NotifyPropertyChanged(BottomProfileGeometryChangedEventArgs);
            }
        }

        void CalculateBottomProfileGeometry()
        {
            if (_viewAwareStatus == null) return;
            var actualControlHeight = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualHeight;
            var actualControlWidth = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;

            //var transect = new Transect("", _location, _transmissionLossRadial.BearingFromSource, _transmissionLossRadial.Ranges.Last());
            var radialSegment = new GeoSegment(_location, Geo.KilometersToRadians(_transmissionLossRadial.Ranges.Last() / 1000f), Geo.DegreesToRadians(_transmissionLossRadial.BearingFromSource));

            var profile = new BottomProfile(_transmissionLossRadial.Ranges.Count, radialSegment, _bathymetry);
            //todo ; later try to subtract half a depth cell from each depth (off-by-1/2 error on display)
            //todo: Dave changed the bottom profile format on 13 Aug 2011.  New format is a list of range/depth pairs where depth changes by more than 1cm
            // this algorithm will need to change if it's ever used again
            var depths = profile.Profile.Select(point => point.Depth * (actualControlHeight / _transmissionLossRadial.Depths.Last())).ToList();
            var pixelsPerRange = (actualControlWidth / _transmissionLossRadial.Ranges.Count);
            var sb = new StringBuilder();

            sb.Append(string.Format("M 0,{0} ", depths[0]));
            for (var index = 0; index < depths.Count; index++)
            {
                var depth = depths[index];
                sb.Append(string.Format("L {0},{1} ", index * pixelsPerRange, depth));
            }
            sb.Append(string.Format("L {0},{1} ", depths.Count * pixelsPerRange, depths.Last()));

            BottomProfileGeometry = sb.ToString();
            
        }

        #endregion
        
        #region Nested type: VoidDelegate

        delegate void VoidDelegate();

        #endregion

        //public TransmissionLossRadial TransmissionLossRadial { get; private set; }
        //public float RangeMin { get; private set; }
        //public float RangeMax { get; private set; }
        //public float DepthMin { get; private set; }
        //public float DepthMax { get; private set; }
    }
}