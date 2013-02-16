using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;
using ESME.Scenarios;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESME.Views.Controls;
using HRC;
using HRC.Aspects;
using HRC.Plotting;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class RadialViewModel : ViewModelBase
    {
        public RadialViewModel() { }

        [ImportingConstructor]
        public RadialViewModel(RadialView view)
        {
            _view = view;
            _dispatcher = Dispatcher.CurrentDispatcher;
            ColorMapViewModel = ColorMapViewModel.Default;
            AxisSeriesViewModel.DataSeriesCollection.Add(_imageSeriesViewModel);
            AxisSeriesViewModel.DataSeriesCollection.Add(_bottomProfileViewModel);
            AxisSeriesViewModel.XAxis.Label = "Range (m)";
            AxisSeriesViewModel.XAxisTicks = null;
            AxisSeriesViewModel.YAxis.IsInverted = true;
            AxisSeriesViewModel.YAxis.Label = "Depth (m)";
            AxisSeriesViewModel.YAxisTicks = null;
            //AxisSeriesViewModel.XAxis.DataRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
            //AxisSeriesViewModel.YAxis.DataRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
            //AxisSeriesViewModel.XAxis.VisibleRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
            AxisSeriesViewModel.YAxis.VisibleRange.RangeChanged += (sender, args) => CalculateBottomProfileGeometry();
            _propertyObserver = new PropertyObserver<RadialViewModel>(this)
                .RegisterHandler(r => r.Radial, RadialChanged);
            _fourAxisSeriesObserver = new PropertyObserver<FourAxisSeriesViewModel>(AxisSeriesViewModel)
                .RegisterHandler(a => a.IsMouseOver, UpdateStatusProperties);
            _xAxisPropertyObserver = new PropertyObserver<DataAxisViewModel>(AxisSeriesViewModel.XAxis)
                .RegisterHandler(x => x.MouseDataLocation, UpdateStatusProperties);
            _yAxisPropertyObserver = new PropertyObserver<DataAxisViewModel>(AxisSeriesViewModel.YAxis)
                .RegisterHandler(y => y.MouseDataLocation, UpdateStatusProperties);
            ColorMapViewModel.CurrentRange.RangeChanged += (s, e) =>
            {
                if (Radial != null && _transmissionLossRadial != null) BeginRenderBitmap(Radial.Guid, _transmissionLossRadial);
                else WaitToRenderText = "This radial has not yet been calculated";
            };
            UpdateStatusProperties();
        }

        [UsedImplicitly] readonly PropertyObserver<RadialViewModel> _propertyObserver;
        [UsedImplicitly] readonly PropertyObserver<DataAxisViewModel> _xAxisPropertyObserver;
        [UsedImplicitly] readonly PropertyObserver<DataAxisViewModel> _yAxisPropertyObserver;
        [UsedImplicitly] readonly PropertyObserver<FourAxisSeriesViewModel> _fourAxisSeriesObserver;

        readonly RadialView _view;
        readonly Dispatcher _dispatcher;
        readonly ImageSeriesViewModel _imageSeriesViewModel = new ImageSeriesViewModel();
        readonly LineSeriesViewModel _bottomProfileViewModel = new LineSeriesViewModel();
        TransmissionLossRadial _transmissionLossRadial;

        public string WaitToRenderText { get; set; }
        [Initialize] public FourAxisSeriesViewModel AxisSeriesViewModel { get; set; }
        public WriteableBitmap WriteableBitmap { get; set; }
        public ColorMapViewModel ColorMapViewModel { get; set; }
        public Visibility WriteableBitmapVisibility { get; set; }
        public Radial Radial { get; set; }

        void RadialChanged()
        {
            //Debug.WriteLine("TransmissionLossRadialViewModel: Initializing transmission loss radial");
            WriteableBitmap = null;
            WriteableBitmapVisibility = Visibility.Collapsed;
            if (Radial == null || !Radial.IsCalculated)
            {
                WriteableBitmapVisibility = Visibility.Collapsed;
                if (Radial == null) WaitToRenderText = "Please wait...";
                else
                {
                    if (!Radial.HasErrors) WaitToRenderText = "This radial has not yet been calculated";
                    else
                    {
                        var sb = new StringBuilder();
                        sb.AppendLine("This radial was not calculated due to the following error(s):");
                        foreach (var error in Radial.Errors) sb.AppendLine(string.Format("  • {0}", error));
                        WaitToRenderText = sb.ToString();
                        WriteableBitmap = null;
                    }
                }
                return;
            }
            try
            {
                _transmissionLossRadial = new TransmissionLossRadial((float)Radial.Bearing, new BellhopOutput(Radial.BasePath + ".shd"));
                if (WriteableBitmap == null) WriteableBitmap = new WriteableBitmap(_transmissionLossRadial.Ranges.Count, _transmissionLossRadial.Depths.Count, 96, 96, PixelFormats.Bgr32, null);
                _currentRange = new Range();
                _imageSeriesViewModel.Top = Radial.Depths.First();
                _imageSeriesViewModel.Left = Radial.Ranges.First();
                _imageSeriesViewModel.Bottom = Radial.Depths.Last();
                _imageSeriesViewModel.Right = Radial.Ranges.Last();
                Debug.WriteLine(string.Format("Radial max depth: {0} max range: {1}", Radial.Depths.Last(), Radial.Ranges.Last()));
                ColorMapViewModel.StatisticalRange.ForceUpdate(_transmissionLossRadial.StatMin, _transmissionLossRadial.StatMax);
                ColorMapViewModel.CurrentRange.ForceUpdate(ColorMapViewModel.StatisticalRange);
                AxisSeriesViewModel.XAxis.DataRange.Update(_imageSeriesViewModel.Left, _imageSeriesViewModel.Right);
                AxisSeriesViewModel.YAxis.DataRange.Update(_imageSeriesViewModel.Top, _imageSeriesViewModel.Bottom);
                AxisSeriesViewModel.XAxis.VisibleRange.Update(_imageSeriesViewModel.Left, _imageSeriesViewModel.Right);
                AxisSeriesViewModel.YAxis.VisibleRange.ForceUpdate(_imageSeriesViewModel.Top, _imageSeriesViewModel.Bottom);
            }
            catch (Exception e)
            {
                WriteableBitmapVisibility = Visibility.Collapsed;
                WaitToRenderText = e.Message;
                _transmissionLossRadial = null;
            }
        }

        void CalculateBottomProfileGeometry()
        {
            var profileData = Radial.BottomProfile.Select(bpp => new Point(bpp.Range * 1000, Math.Max(0.0, bpp.Depth))).ToList();
            var deepestPoint = (from profile in profileData
                                where !double.IsNaN(profile.Y)
                                orderby profile.Y descending
                                select profile).FirstOrDefault();
            Debug.WriteLine(string.Format("CalculateBottomProfileGeometry: Deepest point: {0}m at range {1}m", deepestPoint.Y, deepestPoint.X));
            var yRange = AxisSeriesViewModel.YAxis.VisibleRange == null || AxisSeriesViewModel.YAxis.VisibleRange.IsEmpty ? AxisSeriesViewModel.YAxis.DataRange : (IRange)AxisSeriesViewModel.YAxis.VisibleRange;
            profileData.Insert(0, new Point(profileData[0].X, yRange.Max));
            profileData.Add(new Point(profileData.Last().X, yRange.Max));
            profileData.Add(new Point(profileData.First().X, profileData.First().Y));
            _bottomProfileViewModel.SeriesData = profileData;
            _bottomProfileViewModel.ItemToPoint = p => (Point)p;
            _bottomProfileViewModel.LineStrokeThickness = 5.0;
            //_bottomProfileViewModel.LineStroke = Brushes.Black;
            _bottomProfileViewModel.LineFill = Brushes.LightGray;
        }

        public Mode SelectedMode { get; set; }
        public string MouseRange { get; set; }
        public string MouseDepth { get; set; }
        public string MouseTransmissionLossInfo { get; set; }
        public string MouseSPLInfo { get; set; }

        void UpdateStatusProperties()
        {
            if (AxisSeriesViewModel.IsMouseOver && _transmissionLossRadial != null)
            {
                MouseRange = string.Format("Range: {0:0.0}m", AxisSeriesViewModel.XAxis.MouseDataLocation);
                MouseDepth = string.Format("Depth: {0:0.0}m", AxisSeriesViewModel.YAxis.MouseDataLocation);
                var px = AxisSeriesViewModel.MouseLocation.X / AxisSeriesViewModel.ActualWidth;
                var py = AxisSeriesViewModel.MouseLocation.Y / AxisSeriesViewModel.ActualHeight;
                var rangeIndex = Math.Max(0, Math.Min((int)(_transmissionLossRadial.Ranges.Count * px), _transmissionLossRadial.Ranges.Count - 1));
                var depthIndex = Math.Max(0, Math.Min((int)(_transmissionLossRadial.Depths.Count * py), _transmissionLossRadial.Depths.Count - 1)); MouseTransmissionLossInfo = string.Format("Transmission Loss: {0:0.0}dB", _transmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                if (float.IsInfinity(_transmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]))
                {
                    MouseTransmissionLossInfo = "Transmission Loss: N/A";
                    MouseSPLInfo = "Sound Pressure: N/A";
                }
                else
                {
                    MouseTransmissionLossInfo = string.Format("Transmission Loss: {0:0.0}dB", _transmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                    MouseSPLInfo = string.Format("Sound Pressure: {0:0.0}dB", SelectedMode.SourceLevel - _transmissionLossRadial.TransmissionLoss[depthIndex, rangeIndex]);
                }
            }
            else
            {
                MouseDepth = "Depth: N/A";
                MouseRange = "Range: N/A";
                MouseTransmissionLossInfo = "Transmission Loss: N/A";
                MouseSPLInfo = "Sound Pressure: N/A";
            }
        }

        readonly object _lockObject = new object();
        Range _currentRange;
        volatile bool _isRendering;
        void BeginRenderBitmap(Guid guid, TransmissionLossRadial transmissionLoss)
        {
            if (transmissionLoss == null)
            {
                //Debug.WriteLine("Skipping rendering at 1");
                WaitToRenderText = "This radial has not yet been calculated";
                return;
            }
            if (WriteableBitmap == null)
            {
                //Debug.WriteLine("Skipping rendering at 2");
                return;
            }
            if (_isRendering)
            {
                //Debug.WriteLine("Skipping rendering at 3");
                return;
            }
            lock (_lockObject)
            {
                if (_isRendering)
                {
                    //Debug.WriteLine("Skipping rendering at 4");
                    return;
                }
                //if (_curMinValue == ColorMapViewModel.CurMinValue && _curMaxValue == ColorMapViewModel.CurMaxValue)
                if (!_currentRange.IsEmpty && _currentRange == ColorMapViewModel.CurrentRange)
                {
                    //Debug.WriteLine("Skipping rendering at 5");
                    return;
                }
                //Debug.WriteLine("Render starting");
                _isRendering = true;
                //_curMinValue = ColorMapViewModel.CurMinValue;
                //_curMaxValue = ColorMapViewModel.CurMaxValue;
                _currentRange.Update(ColorMapViewModel.CurrentRange);
            }
            var tokenSource = new CancellationTokenSource();
            var task = TaskEx.Run(() =>
            {
                lock (_lockObject) _isRendering = false;
                RenderBitmapAsync(guid, transmissionLoss, tokenSource.Token);
            });
            task.ContinueWith(t =>
            {
                Debug.WriteLine("Render completed, checking for re-render...");
                BeginRenderBitmap(guid, transmissionLoss);
            });
        }

        void RenderBitmapAsync(Guid guid, TransmissionLossRadial transmissionLoss, CancellationToken token)
        {
            if (token.IsCancellationRequested) return;

            var width = transmissionLoss.Ranges.Count;
            var height = transmissionLoss.Depths.Count;
            var buffer = new int[width * height];

            var infinityColor = ColorMapViewModel.Colors[0];
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
        }

        void RenderBitmap(Guid guid, TransmissionLossRadial transmissionLoss, CancellationToken token)
        {
            if (token.IsCancellationRequested) return;

            var width = transmissionLoss.Ranges.Count;
            var height = transmissionLoss.Depths.Count;
            var buffer = new int[width * height];

            var infinityColor = ColorMapViewModel.Colors[0];
            for (var y = 0; y < height; y++)
            {
                var curOffset = y * width;
                for (var x = 0; x < width; x++)
                {
                    if (token.IsCancellationRequested) return;
                    var curValue = transmissionLoss[y, x];
                    var curColor = float.IsInfinity(curValue) ? infinityColor : ColorMapViewModel.Lookup(curValue);
                    buffer[curOffset++] = ((curColor.A << 24) | (curColor.R << 16) | (curColor.G << 8) | (curColor.B));
                }
            }
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
        }

        #region File and clipboard-oriented stuff
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

        public string ToCSV()
        {
            var sb = new StringBuilder();
            sb.AppendLine("Vertical Transmission Loss (dB)");
            sb.Append(",Range (m),");

            foreach (var t in _transmissionLossRadial.Ranges) sb.Append(string.Format(CultureInfo.InvariantCulture,"{0},",t)); //write out the X axis values.
            sb.AppendLine(); // Terminate the line
            sb.AppendLine("Depth (m)");
            // Write the slice data
            for (var i = 0; i < _transmissionLossRadial.Depths.Count; i++)
            {
                // Write out the Y axis value
                sb.Append(string.Format(CultureInfo.InvariantCulture,"{0},,",_transmissionLossRadial.Depths[i]));
                for (var j = 0; j < _transmissionLossRadial.Ranges.Count; j++)
                {
                    var tl = _transmissionLossRadial.TransmissionLoss[i, j];
                    sb.Append(float.IsInfinity(tl) ? "," : string.Format(CultureInfo.InvariantCulture,"{0},",tl));
                }
                sb.AppendLine();
            }
            return sb.ToString();
        }

        public void SaveAsCSV(string fileName)
        {
            using (var sw = new StreamWriter(fileName))
            {
                sw.Write(ToCSV());
            }
        }

        public BitmapSource ToBitmapSource() { return _view.ToBitmapSource(); }
        public void SaveAsImage(string fileName) { _view.ToImageFile(fileName); }
        public void CopyTextToClipboard() { Clipboard.SetText(ToCSV()); }
        public void CopyImageToClipboard() { Clipboard.SetImage(_view.ToBitmapSource()); }
        #endregion

        public static RadialViewModel DesignTimeData { get; private set; }
        static RadialViewModel()
        {
            var axisRanges = new RangeCollection();
            axisRanges.Add(new Range(0.1, 10));
            DesignTimeData = new RadialViewModel
            {
                AxisSeriesViewModel = new FourAxisSeriesViewModel
                {
                    BottomAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Range (m)",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Depth (m)",
                            IsInverted = true,
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                    XAxisTicks = null,
                    YAxisTicks = null,
                },
                ColorMapViewModel = ColorMapViewModel.Default,
            };
            DesignTimeData.AxisSeriesViewModel.BottomAxis.DataRange = axisRanges;
            DesignTimeData.AxisSeriesViewModel.LeftAxis.DataRange = axisRanges;
        }

    }
}