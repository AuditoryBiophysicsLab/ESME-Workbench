using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using Cinch;
using ESME.Environment;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossRadialViewModel")]
    [NotifyPropertyChanged]
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

        #region GridSizeChangedCommand

        SimpleCommand<object, object> _gridSizeChanged;

        public SimpleCommand<object, object> GridSizeChangedCommand
        {
            get
            {
                return _gridSizeChanged ?? (_gridSizeChanged = new SimpleCommand<object, object>(delegate
                                                                                                 {
                                                                                                     
                                                                                                         if (TransmissionLossRadial != null) CalculateBottomProfileGeometry();
                                                                                                     
                                                                                                    
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
            _viewAwareStatus.ViewLoaded += () => ((Control)_viewAwareStatus.View).SizeChanged += (s, e) => CalculateBottomProfileGeometry();
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

       
        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME.AnalysisPoint Tests\Database"); //todo
        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialChanged)]
        void TransmissionLossRadialChanged(Radial radial)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
                Radial = radial;
                _isRendered = false;
                _writeableBitmap = null;
                NotifyPropertyChanged(WriteableBitmapChangedEventArgs);
                if (Radial == null) return;
                RangeMin = Radial.Ranges.First();
                RangeMax = Radial.Ranges.Last();
                DepthMin = Radial.Depths.First();
                DepthMax = Radial.Depths.Last();
                TransmissionLossRadial = new TransmissionLossRadial((float)Radial.Bearing, new BellhopOutput(Path.Combine(_databaseDirectory, Radial.TransmissionLoss.AnalysisPoint.Scenario.StorageDirectory,
                                            Radial.Filename)));
                ColorMapViewModel.MaxValue =TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue =TransmissionLossRadial.StatMin;
                NotifyPropertyChanged(TransmissionLossRadialChangedEventArgs);
                CalculateBottomProfileGeometry();
                RenderBitmap();
            }
            else
            {
                Debug.WriteLine("TransmissionLossRadialViewModel: Deferring initialization of transmission loss radial");
                //_tempRadial = transmissionLossRadial;
                throw new NotImplementedException("waugh!");
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
            var height =TransmissionLossRadial.Depths.Count;

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
        private TransmissionLossRadial TransmissionLossRadial { get; set; }
        
        public Radial Radial { get; set; }

        

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
            if (_viewAwareStatus == null || Radial == null) return;
            var actualControlHeight = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualHeight;
            var actualControlWidth = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            
            var profile = Radial.BottomProfile;
            var maxDepth = Radial.Depths.Last();
            var maxRange = Radial.Ranges.Last();
            var sb = new StringBuilder();
            foreach (var point in profile)
            {
                var y = point.Depth*(actualControlHeight/maxDepth);
                var x = point.Range*1000*(actualControlWidth/maxRange);
                sb.Append(sb.Length == 0 ? string.Format("M 0,{0} ", y) : string.Format("L {0},{1} ", x, y));
            }
            BottomProfileGeometry = sb.ToString();
            //todo ; later try to subtract half a depth cell from each depth (off-by-1/2 error on display)
            //todo: Dave changed the bottom profile format on 13 Aug 2011.  New format is a list of range/depth pairs where depth changes by more than 1cm
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