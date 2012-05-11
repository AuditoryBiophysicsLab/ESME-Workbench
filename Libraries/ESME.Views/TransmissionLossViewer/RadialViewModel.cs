using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class RadialViewModel : ViewModelBase
    {
        readonly RadialView _view;
        readonly Dispatcher _dispatcher;
        TransmissionLossRadial TransmissionLossRadial { get; set; }
        bool _isRendered;

        public float RangeMin { get; set; }
        public float RangeMax { get; set; }
        public float DepthMin { get; set; }
        public float DepthMax { get; set; }
        public string WaitToRenderText { get; set; }

        #region public WriteableBitmap WriteableBitmap {get; }
        WriteableBitmap _writeableBitmap;

        public WriteableBitmap WriteableBitmap
        {
            get
            {
                if (!_isRendered) RenderBitmap();
                return _writeableBitmap;
            }
        }
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

        #region public Radial Radial { get; set; }
        Radial _radial;

        public Radial Radial
        {
            get { return _radial; }
            set
            {
                _radial = value;
                Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
                _isRendered = false;
                _writeableBitmap = null;
                if (_radial == null)
                {
                    OnPropertyChanged("WriteableBitmap");
                    return;
                }
                TransmissionLossRadial = new TransmissionLossRadial((float)_radial.Bearing, new BellhopOutput(_radial.BasePath + ".shd"));
                RangeMin = _radial.Ranges.First();
                RangeMax = _radial.Ranges.Last();
                DepthMin = _radial.Depths.First();
                DepthMax = _radial.Depths.Last();
                ColorMapViewModel.MaxValue = TransmissionLossRadial.StatMax;
                ColorMapViewModel.MinValue = TransmissionLossRadial.StatMin;
                OnPropertyChanged("TransmissionLossRadial");
                CalculateBottomProfileGeometry();
                RenderBitmap();
            }
        }
        #endregion

        #region public string BottomProfileGeometry { get; set; }
        string _bottomProfileGeometry = "M 0,0";

        public string BottomProfileGeometry { get { return _bottomProfileGeometry; } private set { _bottomProfileGeometry = value; } }

        void CalculateBottomProfileGeometry()
        {
            if (Radial == null) return;
            var actualControlHeight = _view.OverlayCanvas.ActualHeight;
            var actualControlWidth = _view.OverlayCanvas.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            var profile = Radial.BottomProfile;
            var maxDepth = Radial.Depths.Max();
            var maxRange = Radial.Ranges.Last();
            var sb = new StringBuilder();
            var lineFunc = PlotHelpers.GetGlyphRenderFunc(GlyphStyle.Line);
            foreach (var point in profile)
            {
                var y = point.Depth * (actualControlHeight / maxDepth);
                var x = point.Range * 1000 * (actualControlWidth / maxRange);
                sb.Append(sb.Length == 0 ? string.Format("M 0,{0} ", y) : lineFunc(x, y, 1));
            }
            BottomProfileGeometry = sb.ToString();
            //todo ; later try to subtract half a depth cell from each depth (off-by-1/2 error on display)
            //todo: Dave changed the bottom profile format on 13 Aug 2011.  New format is a list of range/depth pairs where depth changes by more than 1cm
        }
        #endregion

        #region public string OutputFileName { get; }
        public string OutputFileName
        {
            get
            {
                lock (this)
                    return Radial == null
                               ? null
                               : Path.Combine(Properties.Settings.Default.ExperimentReportDirectory,
                                              string.Format("{0} {1} {2} bearing {3} degrees",
                                                            Radial.TransmissionLoss.Mode.ModeName,
                                                            Radial.TransmissionLoss.Mode.Source.SourceName,
                                                            Radial.TransmissionLoss.Mode.Source.Platform.PlatformName,
                                                            Radial.Bearing));
            }
        }
        #endregion

        [Initialize("Range: N/A")]
        public string MouseRange { get; set; }
        [Initialize("Depth: N/A")]
        public string MouseDepth { get; set; }
        [Initialize("Transmission Loss: N/A")]
        public string MouseTLInfo { get; set; }
        [Initialize("Sound Pressure: N/A")]
        public string MouseSPLInfo { get; set; }

        [ImportingConstructor]
        public RadialViewModel(RadialView view)
        {
            _view = view;
            _dispatcher = Dispatcher.CurrentDispatcher;
            ColorMapViewModel = ColorMapViewModel.Default;
            view.SizeChanged += (s, e) => CalculateBottomProfileGeometry();
        }

        void RenderBitmap()
        {
            if (TransmissionLossRadial == null || ColorMapViewModel == null) return;

            var width = TransmissionLossRadial.Ranges.Count;
            var height = TransmissionLossRadial.Depths.Count;

            // ReSharper disable PossibleNullReferenceException
            var m = PresentationSource.FromVisual(Application.Current.MainWindow).CompositionTarget.TransformToDevice;
            // ReSharper restore PossibleNullReferenceException
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
            _dispatcher.InvokeIfRequired(RenderFinished, DispatcherPriority.ApplicationIdle);
        }

        void RenderFinished() { OnPropertyChanged("WriteableBitmap"); }

        public string ToCSV()
        {
            var sb = new StringBuilder();
            sb.AppendLine("Vertical Transmission Loss (dB)");
            sb.Append(",Range (m),");

            foreach (var t in TransmissionLossRadial.Ranges) sb.Append(t + ","); //write out the X axis values.
            sb.AppendLine(); // Terminate the line
            sb.AppendLine("Depth (m)");
            // Write the slice data
            for (var i = 0; i < TransmissionLossRadial.Depths.Count; i++)
            {
                // Write out the Y axis value
                sb.Append(TransmissionLossRadial.Depths[i] + ",,");
                for (var j = 0; j < TransmissionLossRadial.Ranges.Count; j++)
                {
                    var tl = TransmissionLossRadial.TransmissionLoss[i, j];
                    sb.Append(float.IsInfinity(tl) ? "," : tl + ",");
                }
                sb.AppendLine();
            }
            return sb.ToString();
        }

        public BitmapSource ToBitmapSource() { return _view.ToBitmapSource(); }

        public void SaveAsCSV(string fileName)
        {
            using (var sw = new StreamWriter(fileName))
            {
                sw.Write(ToCSV());
            }
        }

        public void SaveAsImage(string fileName) { _view.ToImageFile(fileName); }

        public void CopyTextToClipboard() { Clipboard.SetText(ToCSV()); }

        public void CopyImageToClipboard() { Clipboard.SetImage(_view.ToBitmapSource()); }

        #region GridSizeChangedCommand
        SimpleCommand<object, object> _gridSizeChanged;

        public SimpleCommand<object, object> GridSizeChangedCommand { get { return _gridSizeChanged ?? (_gridSizeChanged = new SimpleCommand<object, object>(delegate { if (TransmissionLossRadial != null) CalculateBottomProfileGeometry(); })); } }
        #endregion

        #region MouseMoveCommand
        public SimpleCommand<object, EventToCommandArgs> MouseMoveCommand { get { return _mouseMove ?? (_mouseMove = new SimpleCommand<object, EventToCommandArgs>(MouseMoveHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _mouseMove;

        void MouseMoveHandler(EventToCommandArgs args)
        {
            if (_view.OverlayCanvas.IsMouseOver && TransmissionLossRadial != null && Radial!=null)
            {
                var e = (MouseEventArgs)args.EventArgs;
                var point = e.MouseDevice.GetPosition(_view.OverlayCanvas);
                var px = point.X / _view.OverlayCanvas.ActualWidth;
                var py = point.Y / _view.OverlayCanvas.ActualHeight;
                var rangeIndex = Math.Min((int)(TransmissionLossRadial.Ranges.Count * px), TransmissionLossRadial.Ranges.Count - 1);
                var depthIndex = Math.Min((int)(TransmissionLossRadial.Depths.Count * py), TransmissionLossRadial.Depths.Count - 1);
                MouseRange = string.Format("Range: {0:0.0}m", TransmissionLossRadial.Ranges.Last() * px);
                MouseDepth = string.Format("Depth: {0:0.0}m", TransmissionLossRadial.Depths.Last() * py);
                if (float.IsInfinity(TransmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]))
                {
                    MouseTLInfo = "Transmission Loss: N/A";
                    MouseSPLInfo = "Sound Pressure: N/A";
                }
                else
                {
                    MouseTLInfo = string.Format("Transmission Loss: {0:0.0}dB", TransmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                    MouseSPLInfo = string.Format("Sound Pressure: {0:0.0}dB", Radial.TransmissionLoss.Mode.SourceLevel - TransmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                }
            }
            else
            {
                MouseRange = "Range: N/A";
                MouseDepth = "Depth: N/A";
                MouseTLInfo = "Transmission Loss: N/A";
                MouseSPLInfo = "Sound Pressure: N/A";
            }
        }
        #endregion
    }
}