using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Windows;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossViewModel : ViewModelBase
    {
        public IHRCSaveFileService SaveFileService { get; set; }

        public TransmissionLossViewModel()
        {
            TitleString = "<no radial selected>";
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "SelectedRadialIndex")
                .Select(e => SelectedRadialIndex)
                .DistinctUntilChanged()
                .Where(selectedRadialIndex => selectedRadialIndex >= 0)
                .Throttle(TimeSpan.FromMilliseconds(200))
                .ObserveOnDispatcher()
                .Subscribe(selectedRadialIndex =>
                {
                    Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} TransmissionLossViewModel: SelectedRadialIndex is now {1}", DateTime.Now, selectedRadialIndex));
                    //CurrentRadialView = RadialViews[value];
                    // var foo = SelectedMode.TransmissionLossPluginType. some regex here.
                    var tlstring = "";
                    if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("bellhop")) tlstring = "Bellhop";
                    if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("ramgeo")) tlstring = "RAMGeo";
                    var nameString = Radials == null ? "<no radial selected>" : string.Format("Radial bearing: {0:000.0} degrees. Calculator: {1}", Radials[selectedRadialIndex].Bearing, tlstring);
                    TitleString = nameString;
                    if (RadialViewModel != null)
                    {
                        RadialViewModel.FullRange.Update(MinTransmissionLoss, MaxTransmissionLoss);
                        RadialViewModel.Radial = Radials == null ? null : Radials[selectedRadialIndex];
                        RadialViewModel.AxisSeriesViewModel.PlotTitle = nameString;
                    }
                });
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "SelectedRadialIndex")
                .Select(e => SelectedRadialIndex)
                .DistinctUntilChanged()
                .Where(selectedRadialIndex => selectedRadialIndex >= 0)
                .ObserveOnDispatcher()
                .Subscribe(selectedRadialIndex =>
                {
                    if (Radials == null || Radials.Count < selectedRadialIndex) SelectedBearingGeometry = null;
                    else
                    {
                        var geometryString = new StringBuilder();
                        SelectedRadialBearing = Radials[selectedRadialIndex].Bearing;
                        const double radius = 8;
                        double x, y;
                        for (double angle = 0; angle <= 2 * Math.PI; angle += Math.PI / 32.0)
                        {
                            geometryString.Append(geometryString.Length == 0 ? "M" : "L");
                            x = (Math.Sin(angle) * radius) + radius;
                            y = (Math.Cos(angle) * radius) + radius;
                            geometryString.Append(string.Format(CultureInfo.InvariantCulture, " {0:0.###},{1:0.###} ", x, y));
                        }
                        geometryString.Append(string.Format("M {0:0.###}, {0:0.###} ", radius));
                        x = (Math.Sin(SelectedRadialBearing * (Math.PI / 180)) * radius) + radius;
                        y = (-Math.Cos(SelectedRadialBearing * (Math.PI / 180)) * radius) + radius;
                        geometryString.Append(string.Format(CultureInfo.InvariantCulture, "L {0:0.###},{1:0.###} ", x, y));
                        SelectedBearingGeometry = geometryString.ToString();
                    }
                });
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .ObserveOnDispatcher()
                .Where(e => e.EventArgs.PropertyName == "Window")
                .Subscribe(e => { if (RadialViewModel == null) RadialViewModel = new RadialViewModel { RadialView = Window.FindChildren<RadialView>().First() }; });
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .ObserveOnDispatcher()
                .Where(e => e.EventArgs.PropertyName == "SelectedMode")
                .Subscribe(e => { if (RadialViewModel != null) RadialViewModel.SelectedMode = SelectedMode; });
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .ObserveOnDispatcher()
                .Where(e => e.EventArgs.PropertyName == "RadialViewModel")
                .Subscribe(e1 =>
                {
                    RadialViewModel.SelectedMode = SelectedMode;
                    RadialViewModel.PropertyChanged += (s, e) => { if (e.PropertyName == "WriteableBitmap" && Window != null) Window.Dispatcher.InvokeIfRequired(() => Window.Activate()); };
                });
            Observable.FromEventPattern<PropertyChangedEventArgs>(this, "PropertyChanged")
                .Where(e => e.EventArgs.PropertyName == "TransmissionLoss")
                .Select(e => TransmissionLoss)
                .DistinctUntilChanged()
                .ObserveOnDispatcher()
                .Subscribe(transmissionLoss =>
                {
                    if (_transmissionLossObserver != null) _transmissionLossObserver.Dispose();
                    _transmissionLossObserver = null;
                    Radials = TransmissionLoss == null ? null : TransmissionLoss.Radials.OrderBy(r => r.Bearing).ToList();
                    if (Radials == null) RadialCount = 0;
                    if (transmissionLoss == null || Radials == null) return;
                    Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} TransmissionLossViewModel: TransmissionLoss changed to {1}", DateTime.Now, transmissionLoss.ToString()));
                    RadialCount = Radials.Count - 1;
                    SelectedRadialIndex = -1;
                    try
                    {
                        MinTransmissionLoss = (from radial in Radials
                                               from minimumValue in radial.MinimumTransmissionLossValues
                                               where !double.IsNaN(minimumValue) && !double.IsInfinity(minimumValue)
                                               select minimumValue).Min();
                        MaxTransmissionLoss = (from radial in Radials
                                               from maximumValue in radial.MaximumTransmissionLossValues
                                               where !double.IsNaN(maximumValue) && !double.IsInfinity(maximumValue)
                                               select maximumValue).Max();
                    }
                    catch (FileNotFoundException ex)
                    {
                        Debug.WriteLine(string.Format("File not found exception loading radial: {0}", ex.FileName));
                    }
                    catch (IOException ex)
                    {
                        Debug.WriteLine(string.Format("I/O exception loading radial: {0}", ex.Message));
                    }
                    catch (InvalidOperationException ex)
                    {
                        Debug.WriteLine(string.Format("Invalid operation exception loading radial: {0}", ex.Message));
                    }
                    SelectedMode = transmissionLoss.Modes.OrderBy(m => m.MaxPropagationRadius).Last();
                    MinRadialBearing = Radials.Min(r => r.Bearing);
                    MaxRadialBearing = Radials.Max(r => r.Bearing);
                    RadialBearingChange = (MaxRadialBearing - MinRadialBearing) / RadialCount;
                    SelectedRadialBearing = MinRadialBearing;
                    Debug.WriteLine("{0:HH:mm:ss.fff} TransmissionLossViewModel: SelectedRadialIndex set to -1", DateTime.Now);
                    _transmissionLossObserver = Observable.FromEventPattern<PropertyChangedEventArgs>(transmissionLoss, "PropertyChanged")
                        .ObserveOnDispatcher()
                        .Select(e => TransmissionLoss)
                        .DistinctUntilChanged()
                        .Subscribe(tl => { if (tl.IsDeleted && Window != null) CloseDialog(null); });
                    SelectedRadialIndex = 0;
                });
        }
        IDisposable _transmissionLossObserver;

        [Initialize("<No radial selected>")] 
        public string TitleString { get; set; }
        
        [Initialize(-1)] 
        public int SelectedRadialIndex { get; set; }

        public Window Window { get; set; }

        public List<Radial> Radials { get; set; }
        public int RadialCount { get; private set; }
        public double MinRadialBearing { get; private set; }
        public double MaxRadialBearing { get; private set; }
        public double RadialBearingChange { get; private set; }
        public double SelectedRadialBearing { get; set; }
        public Mode SelectedMode { get; set; }
        public string SelectedBearingGeometry { get; set; }
        public RadialViewModel RadialViewModel { get; set; }
        public float MaxTransmissionLoss { get; set; }
        public float MinTransmissionLoss { get; set; }
        public ESME.Scenarios.TransmissionLoss TransmissionLoss { get; set; }

        #region Commands
        bool AreSaveCommandsEnabled { get { return RadialViewModel != null && RadialViewModel.Radial != null; } }

        #region SaveAsImageCommand
        public SimpleCommand<object, object> SaveAsImageCommand
        {
            get
            {
                return _saveAsImage ??
                       (_saveAsImage =
                        new SimpleCommand<object, object>(delegate { return AreSaveCommandsEnabled; },
                                                          delegate { SaveAsImageHandler(); }));
            }
        }

        SimpleCommand<object, object> _saveAsImage;

        void SaveAsImageHandler()
        {
            SaveFileService.Filter = "Portable Network Graphics (*.png)|*.png|JPEG (*.jpg)|*.jpg|TIFF (*.tiff)|*.tiff|Bitmap (*.bmp)|*.bmp|GIF (*.gif)|*.gif";
            SaveFileService.OverwritePrompt = true;
            SaveFileService.FileName = RadialViewModel.OutputFileName;

            var result = SaveFileService.ShowDialog();
            if (result.HasValue && result.Value)
            {
                Properties.Settings.Default.LastImageExportFileDirectory = Path.GetDirectoryName(SaveFileService.FileName);
                RadialViewModel.SaveAsImage(SaveFileService.FileName);
            }
        }
        #endregion

        #region SaveAsCSVCommand
        public SimpleCommand<object, object> SaveAsCSVCommand
        {
            get
            {
                return _saveAsCSV ??
                       (_saveAsCSV =
                        new SimpleCommand<object, object>(delegate { return AreSaveCommandsEnabled; },
                                                          delegate { SaveAsCSVHandler(); }));
            }
        }

        SimpleCommand<object, object> _saveAsCSV;

        void SaveAsCSVHandler()
        {
            SaveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
            SaveFileService.OverwritePrompt = true;
            SaveFileService.FileName = RadialViewModel.OutputFileName;
            var result = SaveFileService.ShowDialog();
            if (result.HasValue && result.Value)
            {
                Properties.Settings.Default.LastCSVExportFileDirectory = Path.GetDirectoryName(SaveFileService.FileName);
                RadialViewModel.SaveAsCSV(SaveFileService.FileName);
            }
        }
        #endregion

        #region CopyImageToClipboardCommand
        public SimpleCommand<object, object> CopyImageToClipboardCommand
        {
            get
            {
                return _copyImageToClipboard ??
                       (_copyImageToClipboard =
                        new SimpleCommand<object, object>(delegate { return AreSaveCommandsEnabled; },
                                                          delegate { CopyImageToClipboardHandler(); }));
            }
        }

        SimpleCommand<object, object> _copyImageToClipboard;

        void CopyImageToClipboardHandler() { Clipboard.SetImage(RadialViewModel.ToBitmapSource()); }
        #endregion

        #region CopyCSVToClipboardCommand
        public SimpleCommand<object, object> CopyCSVToClipboardCommand
        {
            get
            {
                return _copyCSVToClipboard ??
                       (_copyCSVToClipboard =
                        new SimpleCommand<object, object>(delegate { return AreSaveCommandsEnabled; },
                                                          delegate { CopyCSVToClipboardHandler(); }));
            }
        }

        SimpleCommand<object, object> _copyCSVToClipboard;

        void CopyCSVToClipboardHandler() { Clipboard.SetText(RadialViewModel.ToCSV()); }
        #endregion

        #region InternalViewClosingCommand
        public SimpleCommand<object, object> InternalViewClosingCommand { get { return _internalViewClosing ?? (_internalViewClosing = new SimpleCommand<object, object>(InternalViewClosingHandler)); } }

        SimpleCommand<object, object> _internalViewClosing;

        static void InternalViewClosingHandler(object o) { Properties.Settings.Default.Save(); }
        #endregion

        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _close;
        #endregion

        #endregion

        #region Design-time data
        public static TransmissionLossViewModel DesignTimeData { get; private set; }
        static TransmissionLossViewModel() { DesignTimeData = new TransmissionLossViewModel { RadialViewModel = RadialViewModel.DesignTimeData }; }
        #endregion
    }
}