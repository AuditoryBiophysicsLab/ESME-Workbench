using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading;
using System.Threading.Tasks.Dataflow;
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
        public RadialViewModel()
        {
        }

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
            _fourAxisSeriesObserver = new PropertyObserver<FourAxisSeriesViewModel>(AxisSeriesViewModel)
                .RegisterHandler(a => a.IsMouseOver, UpdateStatusProperties);
            _xAxisPropertyObserver = new PropertyObserver<DataAxisViewModel>(AxisSeriesViewModel.XAxis)
                .RegisterHandler(x => x.MouseDataLocation, UpdateStatusProperties);
            _yAxisPropertyObserver = new PropertyObserver<DataAxisViewModel>(AxisSeriesViewModel.YAxis)
                .RegisterHandler(y => y.MouseDataLocation, UpdateStatusProperties);
            _instanceObservers.Add(AxisSeriesViewModel.YAxis.VisibleRange.Subscribe(e => CalculateBottomProfileGeometry()));
            _instanceObservers.Add(Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged").ObserveOnDispatcher()
                .Subscribe(e =>
                {
                    if (e.EventArgs.PropertyName == "Radial") RadialChanged();
                }));
            _instanceObservers.Add(Observable.FromEventPattern<PropertyChangedEventArgs>(AxisSeriesViewModel, "PropertyChanged").ObserveOnDispatcher()
                .Subscribe(e =>
                {
                    if (e.EventArgs.PropertyName == "ActualWidth" || e.EventArgs.PropertyName == "ActualHeight")
                    {
                        ColorMapViewModel.CurrentRange.ForceUpdate(ColorMapViewModel.CurrentRange);
                        Render();
                        CalculateBottomProfileGeometry();
                    }
                }));
            _instanceObservers.Add(Observable.FromEventPattern<SizeChangedEventArgs>(view, "SizeChanged").Subscribe(e => Render()));
            _displayQueue.ObserveOnDispatcher()
                .Subscribe(result =>
                {
                    var pixelBuffer = result.Item1;
                    var renderRect = result.Item2;
                    var sequenceNumber = result.Item3;
                    //Debug.WriteLine(string.Format("Got completed event for sequence number {0} completed", sequenceNumber));
                    if (sequenceNumber < _displayedSequenceNumber || WriteableBitmap == null || WriteableBitmap.PixelWidth != renderRect.Width || WriteableBitmap.PixelHeight != renderRect.Height)
                    {
                        Debug.WriteLine(string.Format("Discarding image buffer:{0}{1}", 
                            sequenceNumber < _displayedSequenceNumber ? " Old image sequence" : "", 
                            WriteableBitmap == null ? " No bitmap" : 
                            WriteableBitmap.PixelWidth != renderRect.Width || WriteableBitmap.PixelHeight != renderRect.Height ? " Size mismatch" : ""));
                        return;
                    }
                    _displayedSequenceNumber = sequenceNumber;
                    WriteableBitmap.Lock();
                    WriteableBitmap.WritePixels(renderRect, pixelBuffer, WriteableBitmap.BackBufferStride, 0, 0);
                    WriteableBitmap.AddDirtyRect(renderRect);
                    OnPropertyChanged("WriteableBitmap");
                    WriteableBitmap.Unlock();
                    WriteableBitmapVisibility = Visibility.Visible;
                    _imageSeriesViewModel.ImageSource = WriteableBitmap;
                });
            _instanceObservers.Add(Observable.FromEventPattern<RoutedEventArgs>(view, "Unloaded").Subscribe(e =>
            {
                Debug.WriteLine("Unload event");
                _renderQueue.Complete();
                _displayQueue.Dispose();
                _instanceObservers.ForEach(o => o.Dispose());
                _radialObservers.ForEach(o => o.Dispose());
            }));
            UpdateStatusProperties();
        }

        readonly List<IDisposable> _instanceObservers = new List<IDisposable>();
        readonly List<IDisposable> _radialObservers = new List<IDisposable>();

        void Render()
        {
            if (_transmissionLossRadial == null || (int)AxisSeriesViewModel.ActualWidth == 0 || (int)AxisSeriesViewModel.ActualHeight == 0)
            {
                Debug.WriteLine(string.Format("Skipping render:{0}{1}",
                    _transmissionLossRadial == null ? " No radial" : "",
                    (int)AxisSeriesViewModel.ActualWidth == 0 || (int)AxisSeriesViewModel.ActualHeight == 0 ? " Invalid render size" : ""));
                return;
            }
            var width = (int)Math.Min(_transmissionLossRadial.Ranges.Count, AxisSeriesViewModel.ActualWidth);
            var height = (int)Math.Min(_transmissionLossRadial.Depths.Count, AxisSeriesViewModel.ActualHeight);
            if (WriteableBitmap == null) _dispatcher.InvokeIfRequired(() => WriteableBitmap = new WriteableBitmap(width, height, 96, 96, PixelFormats.Bgr32, null));
            var renderRect = new Int32Rect(0, 0, width, height);
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} Starting render", DateTime.Now));
            if (Radial != null && _transmissionLossRadial != null) _renderQueue.Post(Tuple.Create(_transmissionLossRadial, ColorMapViewModel, renderRect, new Range(ColorMapViewModel.CurrentRange), _sourceSequenceNumber, _displayQueue));
            else WaitToRenderText = "This radial has not yet been calculated";
            //Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} Render complete", DateTime.Now));
            Interlocked.Increment(ref _sourceSequenceNumber);
        }

        readonly Subject<Tuple<int[], Int32Rect, long>> _displayQueue = new Subject<Tuple<int[], Int32Rect, long>>();
        long _sourceSequenceNumber, _displayedSequenceNumber;
        readonly ActionBlock<Tuple<TransmissionLossRadial, ColorMapViewModel, Int32Rect, Range, long, Subject<Tuple<int[], Int32Rect, long>>>> _renderQueue = new ActionBlock<Tuple<TransmissionLossRadial, ColorMapViewModel, Int32Rect, Range, long, Subject<Tuple<int[], Int32Rect, long>>>>(job =>
        {
            var tlRadial = job.Item1;
            var colorMap = job.Item2;
            var renderRect = job.Item3;
            var range = job.Item4;
            var sequenceNumber = job.Item5;
            var displayQueue = job.Item6;
            //Debug.WriteLine(string.Format("Begin rendering sequence number {0}", sequenceNumber));
            var buffer = tlRadial.RenderToPixelBuffer(v => float.IsInfinity(v) ? colorMap.Colors[0] : colorMap.Lookup(v, range), renderRect.Width, renderRect.Height);
            //Debug.WriteLine(string.Format("Done rendering sequence number {0}", sequenceNumber));
            displayQueue.OnNext(Tuple.Create(buffer, renderRect, sequenceNumber));
        },
        new ExecutionDataflowBlockOptions
        {
            BoundedCapacity = -1,
            MaxDegreeOfParallelism = System.Environment.ProcessorCount
        });

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
            _radialObservers.ForEach(o => o.Dispose());
            WriteableBitmapVisibility = Visibility.Collapsed;
            WriteableBitmap = null;
            if (Radial == null || !Radial.IsCalculated)
            {
                WriteableBitmapVisibility = Visibility.Collapsed;
                if (Radial == null) WaitToRenderText = "Please wait...";
                else
                {
                    if (!string.IsNullOrEmpty(Radial.Errors))
                    {
                        var sb = new StringBuilder();
                        sb.AppendLine("This radial was not calculated due to the following error(s):");
                        sb.AppendLine(Radial.Errors);
                        WaitToRenderText = sb.ToString();
                    }
                    else WaitToRenderText = "This radial has not yet been calculated";
                    WriteableBitmap = null;
                }
                return;
            }
            try
            {
                _transmissionLossRadial = new TransmissionLossRadial((float)Radial.Bearing, new BellhopOutput(Radial.BasePath + ".shd"));
                _imageSeriesViewModel.Top = Radial.Depths.First();
                _imageSeriesViewModel.Left = Radial.Ranges.First();
                _imageSeriesViewModel.Bottom = Radial.Depths.Last();
                _imageSeriesViewModel.Right = Radial.Ranges.Last();
                //Debug.WriteLine(string.Format("Radial max depth: {0} max range: {1}", Radial.Depths.Last(), Radial.Ranges.Last()));
                ColorMapViewModel.StatisticalRange.ForceUpdate(_transmissionLossRadial.StatMin, _transmissionLossRadial.StatMax);
                ColorMapViewModel.CurrentRange.ForceUpdate(ColorMapViewModel.StatisticalRange);
                AxisSeriesViewModel.XAxis.DataRange.Update(_imageSeriesViewModel.Left, _imageSeriesViewModel.Right);
                AxisSeriesViewModel.YAxis.DataRange.Update(_imageSeriesViewModel.Top, _imageSeriesViewModel.Bottom);
                AxisSeriesViewModel.XAxis.VisibleRange.Update(_imageSeriesViewModel.Left, _imageSeriesViewModel.Right);
                AxisSeriesViewModel.YAxis.VisibleRange.ForceUpdate(_imageSeriesViewModel.Top, _imageSeriesViewModel.Bottom);
                _radialObservers.Add(ColorMapViewModel.CurrentRange.Sample(TimeSpan.FromMilliseconds(50)).Subscribe(e => Render()));
                Render();
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
            //var deepestPoint = (from profile in profileData
            //                    where !double.IsNaN(profile.Y)
            //                    orderby profile.Y descending
            //                    select profile).FirstOrDefault();
            //Debug.WriteLine(string.Format("CalculateBottomProfileGeometry: Deepest point: {0}m at range {1}m", deepestPoint.Y, deepestPoint.X));
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