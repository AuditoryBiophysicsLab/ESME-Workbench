using System;
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
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.Controls;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.TransmissionLossViewer
{
    [ExportViewModel("TransmissionLossRadialViewModel")]
    public class TransmissionLossRadialViewModel : ViewModelBase
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

        readonly string _databaseDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), @"ESME Workbench\Database"); //todo
        
        [MediatorMessageSink(MediatorMessage.TransmissionLossRadialChanged)]
        void TransmissionLossRadialChanged(Radial radial)
        {
            if (_iAmInitialized)
            {
                Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
                Radial = radial;
                _isRendered = false;
                _writeableBitmap = null;
                if (Radial == null)
                {
                    OnPropertyChanged("WriteableBitmap");
                    return;
                }
                RangeMin = Radial.Ranges.First();
                RangeMax = Radial.Ranges.Last();
                DepthMin = Radial.Depths.First();
                DepthMax = Radial.Depths.Last();
                TransmissionLossRadial = new TransmissionLossRadial((float)Radial.Bearing, new BellhopOutput(Path.Combine(_databaseDirectory, Radial.TransmissionLoss.AnalysisPoint.Scenario.StorageDirectory,
                                            Radial.Filename)));
                ColorMapViewModel.MaxValue =TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue =TransmissionLossRadial.StatMin;
                OnPropertyChanged("TransmissionLossRadial");
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
        [MediatorMessageSink(MediatorMessage.SaveRadial)]
        public void SaveAsCSV(string fileName)
        {
            using (var sw = new StreamWriter(fileName))
            {
                // Write the X axis values out first
                sw.WriteLine("Vertical Transmission Loss (dB)");
                sw.Write(",Range (m),");

                foreach (var t in TransmissionLossRadial.Ranges) sw.Write(t + ","); //write out the X axis values.
                sw.WriteLine(); // Terminate the line
                sw.WriteLine("Depth (m)");
                // Write the slice data
                for (var i = 0; i < TransmissionLossRadial.Depths.Count; i++)
                {
                    // Write out the Y axis value
                    sw.Write(TransmissionLossRadial.Depths[i] + ",,");
                    for (var j = 0; j < TransmissionLossRadial.Ranges.Count; j++)
                        sw.Write(TransmissionLossRadial.TransmissionLoss[i, j] + ",");
                    sw.WriteLine(); // Terminate the line
                } // for i
            } // using sw
        }
        
        [MediatorMessageSink(MediatorMessage.SaveRadialBitmap)]
        void SaveRadialBitmap(string fileName)
        {
#if false
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
            encoder.Frames.Add(BitmapFrame.Create(((TransmissionLossRadialView)_viewAwareStatus.View).ToBitmapSource()));
            using (var stream = new FileStream(fileName, FileMode.Create)) encoder.Save(stream); 
#endif
            ((TransmissionLossRadialView)_viewAwareStatus.View).ToImageFile(fileName);
        }

        #region RenderBitmap
        void RenderBitmap()
        {
            if (TransmissionLossRadial == null || ColorMapViewModel == null) return;

            var width = TransmissionLossRadial.Ranges.Count;
            var height = TransmissionLossRadial.Depths.Count;

            var m = PresentationSource.FromVisual(Application.Current.MainWindow).CompositionTarget.TransformToDevice;
            var dx = m.M11;
            var dy = m.M22;

            if (_writeableBitmap == null) _writeableBitmap = new WriteableBitmap(width, height, dx, dy, PixelFormats.Bgr32, null);
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
                        var curColor = float.IsInfinity(curValue) ? infinityColor : ColorMapViewModel.Lookup(curValue);
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
        void RenderFinished() { OnPropertyChanged("WriteableBitmap"); } 
        #endregion

        #region public ColorMapViewModel ColorMapViewModel { get; set; }

        ColorMapViewModel _colorMapViewModel;

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
                            OnPropertyChanged("WriteableBitmap");
                            break;
                    }
                };
            }
        }

        #endregion

        #region public TransmissionLossRadial TransmissionLossRadial { get; set; }

        private TransmissionLossRadial TransmissionLossRadial { get; set; }
        
        public Radial Radial { get; set; }

        

        #endregion

        #region public string BottomProfileGeometry { get; set; }

        string _bottomProfileGeometry = "M 0,0";

        public string BottomProfileGeometry
        {
            get { return _bottomProfileGeometry; }
            private set
            {
                _bottomProfileGeometry = value;
            }
        }

        void CalculateBottomProfileGeometry()
        {
            if (_viewAwareStatus == null || Radial == null) return;
            var actualControlHeight = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualHeight;
            var actualControlWidth = ((TransmissionLossRadialView)_viewAwareStatus.View).OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            
            var profile = Radial.BottomProfile;
            var maxDepth = Radial.Depths.Max();
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