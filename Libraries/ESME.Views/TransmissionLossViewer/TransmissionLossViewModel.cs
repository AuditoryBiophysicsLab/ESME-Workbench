using System;
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
    public class TransmissionLossViewModel : ViewModelBase,IViewStatusAwareInjectionAware
    {
        public IHRCSaveFileService SaveFileService { get; set; }
        [Affects("RadialCount")]
        public ObservableList<Radial> Radials { get; set; }
        public RadialViewModel RadialViewModel { get; set; }
        
        #region public string TitleString { get; set; }
        private string _titleString = "<no radial selected>";
        public string TitleString
        {
            get { return _titleString; }
            set { _titleString = value; }
        } 
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

        [Affects("SelectedBearingGeometry","SelectedRadial","TitleString")]
        public int SelectedRadialIndex
        {
            get { return _selectedRadialIndex; }
            set
            {
                _selectedRadialIndex = value;
                TitleString = Radials == null ? "<no radial selected>" : string.Format("Radial bearing: {0:000.0} degrees", Radials[_selectedRadialIndex].Bearing);
                if(RadialViewModel!=null) RadialViewModel.Radial = Radials == null ? null:Radials[_selectedRadialIndex];
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
        private Window _window;
        public Window Window
        {
            get { return _window; }
            set
            {
                _window = value;
                if (RadialViewModel == null)
                {
                    RadialViewModel = new RadialViewModel(_window.FindChildren<RadialView>().First()) { Radial = Radials == null ? null : Radials[_selectedRadialIndex] };
                }
            }
        } 
        #endregion

        #region public Scenario Scenario { get; set; }
        private Scenarios.TransmissionLoss _transmissionLoss;
        [Affects("Radials", "RadialCount", "SelectedRadialIndex", "SelectedRadial", "SelectedBearingGeometry")]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                Radials = _transmissionLoss == null ? null : new ObservableList<Radial>(from r in _transmissionLoss.Radials orderby r.Bearing select r);
                SelectedRadialIndex = 0;
            }
        } 
        #endregion

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            if (RadialViewModel == null)
                RadialViewModel =new RadialViewModel(((Window) viewAwareStatusService.View).FindChildren<RadialView>().First());
        }

        #region commands
        private bool AreSaveCommandsEnabled
        {
            get { return RadialViewModel != null && RadialViewModel.Radial != null; }
        }
        
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

        private SimpleCommand<object, object> _saveAsImage;
        

        private void SaveAsImageHandler()
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

        private SimpleCommand<object, object> _saveAsCSV;

        private void SaveAsCSVHandler()
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

        private SimpleCommand<object, object> _copyImageToClipboard;

        private void CopyImageToClipboardHandler()
        {
            Clipboard.SetImage(RadialViewModel.ToBitmapSource());
        }

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

        private SimpleCommand<object, object> _copyCSVToClipboard;
        
        private void CopyCSVToClipboardHandler()
        {
            Clipboard.SetText(RadialViewModel.ToCSV());
        }

        #endregion
        #endregion
    }
}
