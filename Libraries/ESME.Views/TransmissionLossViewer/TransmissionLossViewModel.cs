using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
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
        
        #region RadialViewModel RadialViewModel { get; set; }
        RadialViewModel _radialViewModel;
        public RadialViewModel RadialViewModel
        {
            get { return _radialViewModel; }
            set
            {
                _radialViewModel = value;
                _radialViewModel.SelectedMode = SelectedMode;
                _radialViewModel.PropertyChanged += (s, e) => { if (e.PropertyName == "WriteableBitmap" && Window != null) Window.Dispatcher.InvokeIfRequired(() => Window.Activate()); };
            }
        } 
        #endregion

        #region public string TitleString { get; set; }
        string _titleString = "<no radial selected>";
        public string TitleString { get { return _titleString; } set { _titleString = value; } }
        #endregion

        #region public int RadialCount { get; }
        public int RadialCount
        {
            get
            {
                if (TransmissionLoss == null) return 0;
                return Radials.Count - 1;
            }
        }
        #endregion

        #region public int SelectedRadialIndex {get; set;}
        int _selectedRadialIndex = -1;

        [Affects("SelectedBearingGeometry", "SelectedRadial", "TitleString")] public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                if (_selectedRadialIndex == value) return;
                _selectedRadialIndex = value;
                Debug.WriteLine(string.Format("{0:HH:mm:ss.fff} TransmissionLossViewModel: SelectedRadialIndex is now {1}", DateTime.Now, _selectedRadialIndex));
                //CurrentRadialView = RadialViews[value];
               // var foo = SelectedMode.TransmissionLossPluginType. some regex here.
                var tlstring = "";
                if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("bellhop")) tlstring = "Bellhop";
                if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("ramgeo")) tlstring = "RAMGeo";
                var nameString = Radials == null ? "<no radial selected>" : string.Format("Radial bearing: {0:000.0} degrees. Calculator: {1}", Radials[_selectedRadialIndex].Bearing,tlstring);
                TitleString = nameString;
                if (RadialViewModel != null)
                {
                    RadialViewModel.FullRange.Update(MinTransmissionLoss, MaxTransmissionLoss);
                    RadialViewModel.Radial = Radials == null ? null : Radials[_selectedRadialIndex];
                    RadialViewModel.AxisSeriesViewModel.PlotTitle = nameString;
                }
            }
        }
        #endregion

        #region public string SelectedBearingGeometry { get; }
        public string SelectedBearingGeometry
        {
            get
            {
                var sb = new StringBuilder();
                if (RadialViewModel == null || RadialViewModel.Radial == null) return null;
                var bearing = RadialViewModel.Radial.Bearing;
                const double radius = 8;
                double x, y;
                for (double angle = 0; angle <= 2 * Math.PI; angle += Math.PI / 32.0)
                {
                    sb.Append(sb.Length == 0 ? "M" : "L");
                    x = (Math.Sin(angle) * radius) + radius;
                    y = (Math.Cos(angle) * radius) + radius;
                    sb.Append(string.Format(CultureInfo.InvariantCulture," {0:0.###},{1:0.###} ", x, y));
                }
                sb.Append(string.Format("M {0:0.###}, {0:0.###} ", radius));
                x = (Math.Sin(bearing * (Math.PI / 180)) * radius) + radius;
                y = (-Math.Cos(bearing * (Math.PI / 180)) * radius) + radius;
                sb.Append(string.Format(CultureInfo.InvariantCulture,"L {0:0.###},{1:0.###} ", x, y));
                return sb.ToString();
            }
        }
        #endregion

        #region public Window Window { get; set; }
        Window _window;

        public Window Window
        {
            get { return _window; }
            set
            {
                _window = value;
                if (RadialViewModel == null) RadialViewModel = new RadialViewModel { RadialView = _window.FindChildren<RadialView>().First() };
            }
        }
        #endregion
        
        Mode _selectedMode;

        public Mode SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                _selectedMode = value;
                if (RadialViewModel != null) RadialViewModel.SelectedMode = _selectedMode;
            }
        }

        #region public TransmissionLoss TransmissionLoss { get; set; }
        ESME.Scenarios.TransmissionLoss _transmissionLoss;

        [Affects("Radials", "RadialCount", "SelectedRadialIndex", "SelectedRadial", "SelectedBearingGeometry")] 
        public ESME.Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                //var maxTransmissionLoss = float.NaN;
                //var minTransmissionLoss = float.NaN;
                //RadialViewModels.ForEach(r => r.Dispose());
                Radials.Clear();
                //RadialViewModels.Clear();
                //RadialViews.Clear();
                if (_transmissionLoss != null)
                {
                    Radials = _transmissionLoss == null ? null : _transmissionLoss.Radials.OrderBy(r => r.Bearing).ToList();
                    if (Radials == null)
                    {
                        Debug.WriteLine("This transmission loss contains no radials");
                        return;
                    }
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
                    ((INotifyPropertyChanged)_transmissionLoss).PropertyChanged += (s, e) =>
                    {
                        if (e.PropertyName == "IsDeleted" && Window != null)
                        {
                            CloseDialog(null);
                        }
                    };
                    SelectedMode = (from m in _transmissionLoss.Modes
                                    orderby m.MaxPropagationRadius
                                    select m).Last();
#if false
                    var tlstring = "";
                    if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("bellhop")) tlstring = "Bellhop";
                    if (SelectedMode.TransmissionLossPluginType.ToLowerInvariant().Contains("ramgeo")) tlstring = "RAMGeo";
                    foreach (var radial in Radials)
                    {
                        var radialViewModel = new RadialViewModel();
                        radialViewModel.ColorMapViewModel.DataRange.Update(MinTransmissionLoss, MaxTransmissionLoss);
                        radialViewModel.SelectedMode = SelectedMode;
                        radialViewModel.AxisSeriesViewModel.PlotTitle = string.Format("NEW: Radial bearing: {0:000.0} degrees. Calculator: {1}", radial.Bearing, tlstring);
                        radialViewModel.WaitToRenderText = "Please wait...";
                        var radialView = new RadialView { DataContext = radialViewModel };
                        radialViewModel.RadialView = radialView;
                        radialViewModel.Initialize();
                        radialViewModel.Radial = radial;
                        RadialViews.Add(radialView);
                        RadialViewModels.Add(radialViewModel);
                    }
#endif
                }
                //SelectedRadialIndex = 0;
            }
        }
        #endregion

        //[Initialize] public List<RadialView> RadialViews { get; set; }
        //[Initialize] public List<RadialViewModel> RadialViewModels { get; set; }
        [Initialize] public List<Radial> Radials { get; set; }
        //public RadialView CurrentRadialView { get; set; }

        public float MaxTransmissionLoss { get; set; }
        public float MinTransmissionLoss { get; set; }

        #region commands
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
        public static TransmissionLossViewModel DesignTimeData { get; private set; }

        static TransmissionLossViewModel()
        {
            DesignTimeData = new TransmissionLossViewModel
                {
                    RadialViewModel = RadialViewModel.DesignTimeData,
                    //RadialViewModels = new List<RadialViewModel> { RadialViewModel.DesignTimeData, RadialViewModel.DesignTimeData },
                    //RadialViews = new List<RadialView>
                    //{
                    //    new RadialView { DataContext = RadialViewModel.DesignTimeData },
                    //    new RadialView { DataContext = RadialViewModel.DesignTimeData },
                    //    new RadialView { DataContext = RadialViewModel.DesignTimeData },
                    //}
                };
        }

    }
}