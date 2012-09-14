﻿using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
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
using HRC.Plotting;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class RadialViewModel : ViewModelBase
    {
        readonly RadialView _view;
        readonly Dispatcher _dispatcher;
        readonly ImageSeriesViewModel _imageSeriesViewModel = new ImageSeriesViewModel();
        readonly LineSeriesViewModel _bottomProfileViewModel = new LineSeriesViewModel();
        TransmissionLossRadial TransmissionLossRadial { get; set; }
        public float RangeMin { get; set; }
        public float RangeMax { get; set; }
        public float DepthMin { get; set; }
        public float DepthMax { get; set; }
        public string WaitToRenderText { get; set; }
        [Initialize] public FourAxisSeriesViewModel AxisSeriesViewModel { get; set; }
        public WriteableBitmap WriteableBitmap { get; set; }

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
                            if (Radial != null) BeginRenderBitmap(Radial.Guid, TransmissionLossRadial);
                            else WaitToRenderText = "This radial has not yet been calculated";
                            break;
                    }
                };
            }
        }
        #endregion

        public Visibility WriteableBitmapVisibility { get; set; }
        #region public Radial Radial { get; set; }
        Radial _radial;

        public Radial Radial
        {
            get { return _radial; }
            set
            {
                _radial = value;
                Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
                WriteableBitmap = null;
                WriteableBitmapVisibility = Visibility.Collapsed;
                if (_radial == null || !_radial.IsCalculated)
                {
                    WriteableBitmapVisibility = Visibility.Collapsed;
                    BottomProfileGeometry = "M 0,0";
                    if (_radial == null) WaitToRenderText = "Please wait...";
                    else
                    {
                        if (_radial.Errors.Count == 0) WaitToRenderText = "This radial has not yet been calculated";
                        else
                        {
                            var sb = new StringBuilder();
                            sb.AppendLine("This radial was not calculated due to the following error(s):");
                            foreach (var error in _radial.Errors) sb.AppendLine(string.Format("  • {0}", error));
                            WaitToRenderText = sb.ToString();
                        }
                    }
                    return;
                }
                try
                {
                    TransmissionLossRadial = new TransmissionLossRadial((float)_radial.Bearing, new BellhopOutput(_radial.BasePath + ".shd"));
                    if (WriteableBitmap == null) WriteableBitmap = new WriteableBitmap(TransmissionLossRadial.Ranges.Count, TransmissionLossRadial.Depths.Count, 96, 96, PixelFormats.Bgr32, null);
                    ColorMapViewModel.StatisticalMaximum = TransmissionLossRadial.StatMax;
                    ColorMapViewModel.StatisticalMinimum = TransmissionLossRadial.StatMin;
                    _curMaxValue = _curMinValue = -1;
                    BeginRenderBitmap(_radial.Guid, TransmissionLossRadial);
                    RangeMin = _radial.Ranges.First();
                    RangeMax = _radial.Ranges.Last();
                    DepthMin = _radial.Depths.First();
                    DepthMax = _radial.Depths.Last();
                    AxisSeriesViewModel.XAxis.VisibleRange.Update(RangeMin, RangeMax);
                    AxisSeriesViewModel.YAxis.VisibleRange.Update(DepthMin, DepthMax);
                    AxisSeriesViewModel.XAxis.DataRange.Update(RangeMin, RangeMax);
                    AxisSeriesViewModel.YAxis.DataRange.Update(DepthMin, DepthMax);
                    _imageSeriesViewModel.Top = DepthMin;
                    _imageSeriesViewModel.Left = RangeMin;
                    _imageSeriesViewModel.Bottom = DepthMax;
                    _imageSeriesViewModel.Right = RangeMax;
                    OnPropertyChanged("TransmissionLossRadial");
                    AxisSeriesViewModel.XAxis.VisibleRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
                    AxisSeriesViewModel.YAxis.VisibleRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
                    CalculateBottomProfileGeometry();
                }
                catch (Exception e)
                {
                    WriteableBitmapVisibility = Visibility.Collapsed;
                    BottomProfileGeometry = "M 0,0";
                    WaitToRenderText = e.Message;
                    TransmissionLossRadial = null;
                }
            }
        }
        #endregion

        #region public string BottomProfileGeometry { get; set; }
        public string BottomProfileGeometry { get; private set; }

        void CalculateBottomProfileGeometry()
        {
            var profileData = Radial.BottomProfile.Select(bpp => new Point(bpp.Range * 1000, bpp.Depth)).ToList();
            profileData.Insert(0, new Point(profileData[0].X, AxisSeriesViewModel.YAxis.VisibleRange.Max));
            profileData.Add(new Point(profileData.Last().X, AxisSeriesViewModel.YAxis.VisibleRange.Max));
            profileData.Add(new Point(profileData.First().X, profileData.First().Y));
            _bottomProfileViewModel.SeriesData = profileData;
            _bottomProfileViewModel.ItemToPoint = p => (Point)p;
            _bottomProfileViewModel.LineStrokeThickness = 5.0;
            //_bottomProfileViewModel.LineStroke = Brushes.Black;
            _bottomProfileViewModel.LineFill = Brushes.Gray;
#if false
            if (Radial == null) return;
            var actualControlHeight = _view.OverlayCanvas.ActualHeight;
            var actualControlWidth = _view.OverlayCanvas.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            var profile = Radial.BottomProfile;
            var maxDepth = Math.Round(Radial.Depths.Max(), 4);
            var maxRange =  Math.Round(Radial.Ranges.Last(), 4);
            var sb = new StringBuilder();
            var lineFunc = PlotHelpers.GetGlyphRenderFunc(GlyphStyle.Line);
            try
            {
                foreach (var point in profile)
                {
                    var y = point.Depth * (actualControlHeight / maxDepth);
                    var x = point.Range * 1000 * (actualControlWidth / maxRange);
                    sb.Append(sb.Length == 0 ? string.Format("L 0,{0} ", y) : lineFunc(x, y, 1));
                }
                BottomProfileGeometry = string.Format("M 0,{0} {1} L {2},{0} Z", actualControlHeight, sb, actualControlWidth);
            }
            catch (Exception e)
            {
                Debug.WriteLine("{0}: Caught exception in CalculateBottomProfileGeometry: {1}", DateTime.Now, e.Message);
                BottomProfileGeometry = "M 0,0";
            }
            //todo ; later try to subtract half a depth cell from each depth (off-by-1/2 error on display)
            //todo: Dave changed the bottom profile format on 13 Aug 2011.  New format is a list of range/depth pairs where depth changes by more than 1cm
#endif
        }
        #endregion

        public Mode SelectedMode { get; set; }
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
                                                            SelectedMode.ModeName,
                                                            SelectedMode.Source.SourceName,
                                                            SelectedMode.Source.Platform.PlatformName,
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
            BottomProfileGeometry = "M 0,0";
            _view = view;
            _dispatcher = Dispatcher.CurrentDispatcher;
            ColorMapViewModel = ColorMapViewModel.Default;
            //view.SizeChanged += (s, e) => CalculateBottomProfileGeometry();
            AxisSeriesViewModel.DataSeriesCollection.Add(_imageSeriesViewModel);
            AxisSeriesViewModel.DataSeriesCollection.Add(_bottomProfileViewModel);
            AxisSeriesViewModel.XAxis.Label = "Range (m)";
            AxisSeriesViewModel.XAxisTicks = null;
            AxisSeriesViewModel.YAxis.IsInverted = true;
            AxisSeriesViewModel.YAxis.Label = "Depth (m)";
            AxisSeriesViewModel.YAxisTicks = null;
        }

        void BeginRenderBitmap(Guid guid, TransmissionLossRadial transmissionLoss)
        {
            if (transmissionLoss == null)
            {
                Debug.WriteLine("Skipping rendering at 1");
                WaitToRenderText = "This radial has not yet been calculated";
                return;
            }
            if (WriteableBitmap == null)
            {
                Debug.WriteLine("Skipping rendering at 2");
                return;
            }
            if (_isRendering)
            {
                Debug.WriteLine("Skipping rendering at 3");
                return;
            }
            lock (_lockObject)
            {
                if (_isRendering)
                {
                    Debug.WriteLine("Skipping rendering at 4");
                    return;
                }
                if (_curMinValue == ColorMapViewModel.CurMinValue && _curMaxValue == ColorMapViewModel.CurMaxValue)
                {
                    Debug.WriteLine("Skipping rendering at 5");
                    return;
                }
                Debug.WriteLine("Render starting");
                _isRendering = true;
                _curMinValue = ColorMapViewModel.CurMinValue;
                _curMaxValue = ColorMapViewModel.CurMaxValue;
            }
            var tokenSource = new CancellationTokenSource();
            var task = TaskEx.Run(() =>
            {
                lock (_lockObject) _isRendering = false;
                Debug.WriteLine("Render completed, checking for re-render...");
                RenderBitmapAsync(guid, transmissionLoss, tokenSource.Token);
            });
            task.ContinueWith(t => BeginRenderBitmap(guid, transmissionLoss));
        }

        readonly object _lockObject = new object();
        double _curMinValue, _curMaxValue;
        volatile bool _isRendering;
        void RenderBitmapAsync(Guid guid, TransmissionLossRadial transmissionLoss, CancellationToken token)
        {
            if (token.IsCancellationRequested) return;

            var width = transmissionLoss.Ranges.Count;
            var height = transmissionLoss.Depths.Count;
            var buffer = new int[width * height];

            var infinityColor = _colorMapViewModel.Colors[0];
            var yValues = Enumerable.Range(0, height).AsParallel();
            yValues.ForAll(y =>
            {
                var curOffset = y * width;
                for (var x = 0; x < width; x++)
                {
                    if (token.IsCancellationRequested) return;
                    var curValue = transmissionLoss[y, x];
                    var curColor = float.IsInfinity(curValue) ? infinityColor : ColorMapViewModel.Lookup(curValue);
                    buffer[curOffset++] = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            });
            _dispatcher.InvokeIfRequired(() =>
            {
                if (guid != Radial.Guid) return;
                var rect = new Int32Rect(0, 0, width, height);
                WriteableBitmap.Lock();
                WriteableBitmap.WritePixels(rect, buffer, WriteableBitmap.BackBufferStride, 0, 0);
                WriteableBitmap.AddDirtyRect(rect);
                OnPropertyChanged("WriteableBitmap");
                WriteableBitmap.Unlock();
                WriteableBitmapVisibility = Visibility.Visible;
                _imageSeriesViewModel.ImageSource = WriteableBitmap;
            }, DispatcherPriority.Render);
            //_writeableBitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            //_writeableBitmap.Unlock();
            //_dispatcher.InvokeIfRequired(RenderFinished, DispatcherPriority.ApplicationIdle);
        }

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
        //SimpleCommand<object, object> _gridSizeChanged;

        //public SimpleCommand<object, object> GridSizeChangedCommand { get { return _gridSizeChanged ?? (_gridSizeChanged = new SimpleCommand<object, object>(delegate { if (TransmissionLossRadial != null) CalculateBottomProfileGeometry(); })); } }
        #endregion

        #region MouseMoveCommand
        public SimpleCommand<object, EventToCommandArgs> MouseMoveCommand { get { return _mouseMove ?? (_mouseMove = new SimpleCommand<object, EventToCommandArgs>(MouseMoveHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _mouseMove;

        void MouseMoveHandler(EventToCommandArgs args)
        {
#if false
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
                    MouseSPLInfo = string.Format("Sound Pressure: {0:0.0}dB", SelectedMode.SourceLevel - TransmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                }
            }
            else
            {
                MouseRange = "Range: N/A";
                MouseDepth = "Depth: N/A";
                MouseTLInfo = "Transmission Loss: N/A";
                MouseSPLInfo = "Sound Pressure: N/A";
            }
#endif
        }
        #endregion

        //public void Dispose() { _writer.Close(); }
    }
}