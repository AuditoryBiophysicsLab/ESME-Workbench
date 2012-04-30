using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Environment;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
using HRC.WPF;
using HRC.ViewModels;

namespace ESME.Views.Environment
{
    public class SoundSpeedProfileViewModel : ViewModelBase
    {
        private readonly IHRCSaveFileService _saveFileService;

        public SoundSpeedProfileView View { get; set; }
        public SoundSpeedProfileWindowView WindowView { get; set; }
        public float SSPMax { get; set; }
        public float SSPMin { get; set; }
        public float DepthMin { get; set; }
        public float DepthMax { get; set; }
        [Initialize("M 0,0")]
        public string SoundSpeedGeometry { get; private set; }
        [Initialize("M 0,0")]
        public string SoundSpeedDataPoints { get; private set; }
        [Initialize("M 0,0")]
        public string MajorGrid { get; private set; }
        [Initialize("M 0,0")]
        public string MinorGrid { get; private set; }
        [Initialize("Sound Speed Profile")]
        public string WindowTitle { get; set; }
        public string OutputFileName { get; set; }
        public ObservableList<double> DepthAxisMajorTicks { get; set; }
        public ObservableList<double> DepthAxisMinorTicks { get; set; }
        public ObservableList<double> SpeedAxisMajorTicks { get; set; }
        public ObservableList<double> SpeedAxisMinorTicks { get; set; }
       
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

        public SoundSpeedProfileViewModel(IHRCSaveFileService saveFile)
        {
            _saveFileService = saveFile;
        }

        void RenderCanvas()
        {
            if (SoundSpeedProfile == null) return;
            var height = View.OverlayCanvas.ActualHeight;
            var width = View.OverlayCanvas.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (height == 0 || width == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            MajorGrid = PlotHelpers.GetGrid(DepthAxisMajorTicks, SpeedAxisMajorTicks, 1, height, width);
            MinorGrid = PlotHelpers.GetGrid(DepthAxisMinorTicks, SpeedAxisMinorTicks, 0, height, width);

            SoundSpeedGeometry = SoundSpeedProfile.GetGeometry(height, width);
            SoundSpeedDataPoints = SoundSpeedProfile.GetGeometry(height, width, glyphStyle: GlyphStyle.Circle);
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

        private SimpleCommand<object, object> _saveToCSV;

        private void SaveToCSVHandler(string fileName)
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
                                                              _saveFileService.Filter = "Bitmap (*.bmp)|*.bmp|GIF (*.gif)|*.gif|JPEG (*.jpg)|*.jpg|Portable Network Graphics (*.png)|*.png|TIFF (*.tiff)|*.tiff";
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

        private SimpleCommand<object, object> _saveToImage;
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

        private SimpleCommand<object, object> _copyTextToClipboard;

        private void CopyTextToClipboardHandler()
        {
            var sb = new StringBuilder();
            sb.AppendLine(string.Format("#Sound Speed Profile ({0:0.000},{1:0.000})", SoundSpeedProfile.Latitude,
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

        private SimpleCommand<object, object> _copyImageToClipboard;
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

        private SimpleCommand<object, object> _gridSizeChanged;

        private void GridSizeChangedHandler()
        {
            RenderCanvas();
        }

        #endregion
        #endregion
    }
}
