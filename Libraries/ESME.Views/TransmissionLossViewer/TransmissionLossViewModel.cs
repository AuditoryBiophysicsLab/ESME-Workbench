using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.TransmissionLossViewer
{
    public class TransmissionLossViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        public IHRCSaveFileService SaveFileService { get; set; }
        [Affects("RadialCount")] public ObservableList<Radial> Radials { get; set; }
        
        #region RadialViewModel RadialViewModel { get; set; }
        RadialViewModel _radialViewModel;
        public RadialViewModel RadialViewModel
        {
            get { return _radialViewModel; }
            set
            {
                _radialViewModel = value;
                _radialViewModel.SelectedMode = SelectedMode;
                _radialViewModel.PropertyChanged += (s, e) => { if (e.PropertyName == "WriteableBitmap" && Window!=null) Window.Activate(); };
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
        int _selectedRadialIndex;

        [Affects("SelectedBearingGeometry", "SelectedRadial", "TitleString")] public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                _selectedRadialIndex = value;
                TitleString = Radials == null ? "<no radial selected>" : string.Format("Radial bearing: {0:000.0} degrees", Radials[_selectedRadialIndex].Bearing);
                if (RadialViewModel != null)
                {
                    RadialViewModel.Radial = Radials == null ? null : Radials[_selectedRadialIndex];
                    //if (!float.IsNaN(MaxTransmissionLoss)) RadialViewModel.ColorMapViewModel.MaxValue = MaxTransmissionLoss;
                    //if (!float.IsNaN(MinTransmissionLoss)) RadialViewModel.ColorMapViewModel.MinValue = MinTransmissionLoss;
                    RadialViewModel.ColorMapViewModel.FullRange.Update(MinTransmissionLoss, MaxTransmissionLoss);
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
                    sb.Append(string.Format(" {0:0.###},{1:0.###} ", x, y));
                }
                sb.Append(string.Format("M {0:0.###}, {0:0.###} ", radius));
                x = (Math.Sin(bearing * (Math.PI / 180)) * radius) + radius;
                y = (-Math.Cos(bearing * (Math.PI / 180)) * radius) + radius;
                sb.Append(string.Format("L {0:0.###},{1:0.###} ", x, y));
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
                if (RadialViewModel == null)
                {
                    RadialViewModel = new RadialViewModel(_window.FindChildren<RadialView>().First()) {Radial = Radials == null ? null : Radials[_selectedRadialIndex]};
                }
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
                Radials = _transmissionLoss == null ? null : new ObservableList<Radial>(from r in _transmissionLoss.Radials orderby r.Bearing select r);
                //var maxTransmissionLoss = float.NaN;
                //var minTransmissionLoss = float.NaN;
                if (_transmissionLoss != null)
                {
                    try
                    {
                        MinTransmissionLoss = (from radial in _transmissionLoss.Radials
                                               from minimumValue in radial.MinimumTransmissionLossValues
                                               where !double.IsNaN(minimumValue) && !double.IsInfinity(minimumValue)
                                               select minimumValue).Min();
                        MaxTransmissionLoss = (from radial in _transmissionLoss.Radials
                                               from maximumValue in radial.MaximumTransmissionLossValues
                                               where !double.IsNaN(maximumValue) && !double.IsInfinity(maximumValue)
                                               select maximumValue).Max();
#if DEBUG
                        foreach (var radial in _transmissionLoss.Radials)
                        {
                            var maxIndex = radial.MaximumTransmissionLossValues.ToList().IndexOf(MaxTransmissionLoss);
                            if (maxIndex == -1) continue;
                            Debug.WriteLine(string.Format("Maximum TL value of {0} found in radial bearing {1} at range {2}", MaxTransmissionLoss, radial.Bearing, radial.Ranges[maxIndex]));
                            radial.ExtractAxisData(null, maxIndex);
                        }
#endif

                    }
                    catch (FileNotFoundException ex)
                    {
                        Debug.WriteLine(string.Format("File not found exception loading radial: {0}", ex.FileName));
                    }
                    catch (IOException ex)
                    {
                        Debug.WriteLine(string.Format("I/O exception loading radial: {0}", ex.Message));
                    }
#if false
                    foreach (var radial in _transmissionLoss.Radials)
                    {
                        try
                        {
                            var curMax = radial.MaximumTransmissionLossValues.Where(v => !double.IsNaN(v) && !double.IsInfinity(v)).Max();
                            var curMin = radial.MinimumTransmissionLossValues.Where(v => !double.IsNaN(v) && !double.IsInfinity(v)).Min(v => !double.IsInfinity(v) ? v : 1000);
                            maxTransmissionLoss = float.IsNaN(maxTransmissionLoss) ? curMax : Math.Max(maxTransmissionLoss, curMax);
                            minTransmissionLoss = float.IsNaN(minTransmissionLoss) ? curMin : Math.Min(minTransmissionLoss, curMin);
                        }
                        catch (FileNotFoundException)
                        {
                            Debug.WriteLine(string.Format("File not found exception loading radial: {0}", radial.Filename));
                        }
                        catch (IOException)
                        {
                            Debug.WriteLine(string.Format("I/O exception loading radial: {0}", radial.Filename));
                        }
                    }
#endif
                    _transmissionLoss.PropertyChanged += (s, e) =>
                    {
                        if (e.PropertyName == "IsDeleted" && Window != null)
                        {
                            CloseDialog(null);
                        }
                    };
                    SelectedMode = (from m in _transmissionLoss.Modes
                                    orderby m.MaxPropagationRadius
                                    select m).Last();
                }
                //MaxTransmissionLoss = maxTransmissionLoss;
                //MinTransmissionLoss = minTransmissionLoss;
                SelectedRadialIndex = 0;
            }
        }
        #endregion

        public float MaxTransmissionLoss { get; set; }
        public float MinTransmissionLoss { get; set; }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            if (RadialViewModel == null)
                RadialViewModel = new RadialViewModel(((Window)viewAwareStatusService.View).FindChildren<RadialView>().First());
        }

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
    }
}