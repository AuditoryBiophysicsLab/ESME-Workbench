using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Environment;
using ESME.Views.Controls;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Environment
{
    public class SoundSpeedProfileViewModel : ViewModelBase
    {
        readonly IHRCSaveFileService _saveFileService;

        public SoundSpeedProfileView View { get; set; }
        public SoundSpeedProfileWindowView WindowView { get; set; }
        public float SSPMax { get; set; }
        public float SSPMin { get; set; }
        public float DepthMin { get; set; }
        public float DepthMax { get; set; }
        [Initialize("M 0,0")] public string SoundSpeedGeometry { get; private set; }
        [Initialize("M 0,0")] public string SoundSpeedDataPoints { get; private set; }
        [Initialize("M 0,0")] public string MajorGrid { get; private set; }
        [Initialize("M 0,0")] public string MinorGrid { get; private set; }
        [Initialize("Sound Speed Profile")] public string WindowTitle { get; set; }
        public string OutputFileName { get; set; }
        public ObservableList<AxisTick> DepthAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> DepthAxisMinorTicks { get; set; }
        public ObservableList<AxisTick> SpeedAxisMajorTicks { get; set; }
        public ObservableList<AxisTick> SpeedAxisMinorTicks { get; set; }

        #region public SoundSpeedProfile SoundSpeedProfile {get; set; }
        SoundSpeedProfile _soundSpeedProfile;

        public SoundSpeedProfile SoundSpeedProfile
        {
            get { return _soundSpeedProfile; }
            set
            {
                _soundSpeedProfile = value;
                DepthMin = value.Data.Min(d => d.Depth);
                DepthMax = value.Data.Max(d => d.Depth);
                SSPMin = value.Data.Min(d => d.SoundSpeed);
                SSPMax = value.Data.Max(d => d.SoundSpeed);
                RenderCanvas();
                WindowTitle = string.Format("Sound Speed Profile ({0:0.000}, {1:0.000})", _soundSpeedProfile.Latitude, _soundSpeedProfile.Longitude);
                OutputFileName = string.Format("Sound Speed Profile lat {0:0.000} lon {1:0.000}", _soundSpeedProfile.Latitude, _soundSpeedProfile.Longitude);
            }
        }
        #endregion

        public SoundSpeedProfileViewModel(IHRCSaveFileService saveFile) { _saveFileService = saveFile; }

        void RenderCanvas()
        {
            if (SoundSpeedProfile == null) return;
            var height = View.OverlayCanvas.ActualHeight;
            var width = View.OverlayCanvas.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (height == 0 || width == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            MajorGrid = DataAxis.GetGrid(DepthAxisMajorTicks, SpeedAxisMajorTicks, 1, height, width);
            MinorGrid = DataAxis.GetGrid(DepthAxisMinorTicks, SpeedAxisMinorTicks, 0, height, width);

            SoundSpeedGeometry = SoundSpeedProfile.GetGeometry(height, width);
            SoundSpeedDataPoints = SoundSpeedProfile.GetGeometry(height, width, glyphStyle:GlyphStyle.Circle);
        }

        #region commands

        #region SaveToCSVCommand
        public SimpleCommand<object, object> SaveToCSVCommand
        {
            get
            {
                return _saveToCSV ??
                       (_saveToCSV =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate
                                                          {
                                                              _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                                                              _saveFileService.OverwritePrompt = true;
                                                              _saveFileService.FileName = OutputFileName;
                                                              var result = _saveFileService.ShowDialog(WindowView);
                                                              if (result.HasValue && result.Value)
                                                              {
                                                                  SaveToCSVHandler(_saveFileService.FileName);
                                                              }
                                                          }));
            }
        }

        SimpleCommand<object, object> _saveToCSV;

        void SaveToCSVHandler(string fileName)
        {
            using (var writer = new StreamWriter(fileName))
            {
                writer.WriteLine(string.Format("#Sound Speed Profile ({0:0.000} {1:0.000})", SoundSpeedProfile.Latitude, SoundSpeedProfile.Longitude));
                writer.WriteLine("Depth (m),SoundSpeed (m/s)");
                foreach (var point in (from d in SoundSpeedProfile.Data orderby d.Depth select d))
                {
                    writer.WriteLine(string.Format("{0:0.00},{1:0.000}", point.Depth, point.SoundSpeed));
                }
            }
        }
        #endregion

        #region SaveToImageCommand
        public SimpleCommand<object, object> SaveToImageCommand
        {
            get
            {
                return _saveToImage ??
                       (_saveToImage =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate
                                                          {
                                                              _saveFileService.Filter =
                                                                  "Portable Network Graphics (*.png)|*.png|Bitmap (*.bmp)|*.bmp|GIF (*.gif)|*.gif|JPEG (*.jpg)|*.jpg|TIFF (*.tiff)|*.tiff";
                                                              _saveFileService.OverwritePrompt = true;
                                                              _saveFileService.FileName = OutputFileName;
                                                              var result = _saveFileService.ShowDialog(WindowView);
                                                              if (result.HasValue && result.Value)
                                                              {
                                                                  View.ToImageFile(_saveFileService.FileName);
                                                              }
                                                          }));
            }
        }

        SimpleCommand<object, object> _saveToImage;
        #endregion

        #region CopyTextToClipboardCommand
        public SimpleCommand<object, object> CopyTextToClipboardCommand
        {
            get
            {
                return _copyTextToClipboard ??
                       (_copyTextToClipboard =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate { CopyTextToClipboardHandler(); }));
            }
        }

        SimpleCommand<object, object> _copyTextToClipboard;

        void CopyTextToClipboardHandler()
        {
            var sb = new StringBuilder();
            sb.AppendLine(string.Format("#Sound Speed Profile ({0:0.000},{1:0.000})",
                                        SoundSpeedProfile.Latitude,
                                        SoundSpeedProfile.Longitude));
            sb.AppendLine("Depth (m),SoundSpeed (m/s)");
            foreach (var point in (from d in SoundSpeedProfile.Data orderby d.Depth select d))
            {
                sb.AppendLine(string.Format("{0:0.00},{1:0.000}", point.Depth, point.SoundSpeed));
            }
            Clipboard.SetText(sb.ToString());
        }
        #endregion

        #region CopyImageToClipboardCommand
        public SimpleCommand<object, object> CopyImageToClipboardCommand
        {
            get
            {
                return _copyImageToClipboard ??
                       (_copyImageToClipboard =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate { Clipboard.SetImage(View.ToBitmapSource()); }));
            }
        }

        SimpleCommand<object, object> _copyImageToClipboard;
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, object> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, object>(ViewClosingHandler)); } }
        SimpleCommand<object, object> _viewClosing;
        static void ViewClosingHandler(object o) { Properties.Settings.Default.Save(); }
        #endregion

        #region GridSizeChangedCommand
        public SimpleCommand<object, object> GridSizeChangedCommand
        {
            get
            {
                return _gridSizeChanged ??
                       (_gridSizeChanged =
                        new SimpleCommand<object, object>(delegate { GridSizeChangedHandler(); }));
            }
        }

        SimpleCommand<object, object> _gridSizeChanged;

        void GridSizeChangedHandler() { RenderCanvas(); }
        #endregion

        #endregion
    }
}