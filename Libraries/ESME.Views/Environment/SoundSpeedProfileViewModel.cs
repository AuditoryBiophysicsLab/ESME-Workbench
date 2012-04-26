using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Environment;
using HRC.Aspects;
using HRC.Services;
using HRC.Utility;
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
            var actualControlHeight = View.OverlayCanvas.ActualHeight;
            var actualControlWidth = View.OverlayCanvas.ActualWidth;
            // ReSharper disable CompareOfFloatsByEqualityOperator
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            // ReSharper restore CompareOfFloatsByEqualityOperator

            MajorGrid = this.GetMajorGrid(actualControlHeight, actualControlWidth);
            MinorGrid = this.GetMinorGrid(actualControlHeight, actualControlWidth);

            SoundSpeedGeometry = SoundSpeedProfile.GetGeometry(actualControlHeight, actualControlWidth);
            SoundSpeedDataPoints = SoundSpeedProfile.GetGeometry(actualControlHeight, actualControlWidth, glyphStyle: GlyphStyle.Circle);
        }

        #region commands
        #region SaveToCSVCommand

        public SimpleCommand<object, object> SaveToCSVCommand
        {
            get
            {
                return _saveToCSV ??
                       (_saveToCSV =
                        new SimpleCommand<object, object>(delegate { return IsSaveToCSVCommandEnabled; },
                                                          delegate
                                                          {
                                                              _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                                                              _saveFileService.OverwritePrompt = true;

                                                              var result = _saveFileService.ShowDialog(WindowView);
                                                              if (result.HasValue && result.Value)
                                                              {
                                                                  SaveToCSVHandler(_saveFileService.FileName);
                                                              }
                                                          }));
            }
        }

        private SimpleCommand<object, object> _saveToCSV;

        private bool IsSaveToCSVCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

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

        #region SavetoPNGCommand

        public SimpleCommand<object, object> SavetoPNGCommand
        {
            get
            {
                return _savetoPNG ??
                       (_savetoPNG =
                        new SimpleCommand<object, object>(delegate { return IsSavetoPNGCommandEnabled; },
                                                          delegate { SavetoPNGHandler(); }));
            }
        }

        private SimpleCommand<object, object> _savetoPNG;

        private bool IsSavetoPNGCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

        private void SavetoPNGHandler()
        {

        }

        #endregion

        #region SaveToGIFCommand

        public SimpleCommand<object, object> SaveToGIFCommand
        {
            get
            {
                return _saveToGIF ??
                       (_saveToGIF =
                        new SimpleCommand<object, object>(delegate { return IsSaveToGIFCommandEnabled; },
                                                          delegate { SaveToGIFHandler(); }));
            }
        }

        private SimpleCommand<object, object> _saveToGIF;

        private bool IsSaveToGIFCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

        private void SaveToGIFHandler()
        {

        }

        #endregion

        #region SavetoJPGCommand

        public SimpleCommand<object, object> SavetoJPGCommand
        {
            get
            {
                return _savetoJPG ??
                       (_savetoJPG =
                        new SimpleCommand<object, object>(delegate { return IsSavetoJPGCommandEnabled; },
                                                          delegate { SavetoJPGHandler(); }));
            }
        }

        private SimpleCommand<object, object> _savetoJPG;

        private bool IsSavetoJPGCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

        private void SavetoJPGHandler()
        {

        }

        #endregion

        #region CopyTextToClipboardCommand

        public SimpleCommand<object, object> CopyTextToClipboardCommand
        {
            get
            {
                return _copyTextToClipboard ??
                       (_copyTextToClipboard =
                        new SimpleCommand<object, object>(delegate { return IsCopyTextToClipboardCommandEnabled; },
                                                          delegate { CopyTextToClipboardHandler(); }));
            }
        }

        private SimpleCommand<object, object> _copyTextToClipboard;

        private bool IsCopyTextToClipboardCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

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
                        new SimpleCommand<object, object>(delegate { return IsCopyImageToClipboardCommandEnabled; },
                                                          delegate { CopyImageToClipboardHandler(); }));
            }
        }

        private SimpleCommand<object, object> _copyImageToClipboard;

        private bool IsCopyImageToClipboardCommandEnabled
        {
            get { return SoundSpeedProfile != null; }
        }

        private void CopyImageToClipboardHandler()
        {

        }

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

    internal static class SoundSpeedViewModelExtensions
    {
        public static string GetMajorGrid(this SoundSpeedProfileViewModel profile, double height, double width)
        {
            var sb = new StringBuilder();
            //skip the first (don't draw on the axis)
            for (var i = 0; i < profile.DepthAxisMajorTicks.Count - 1; i++)
            {
                var tick = profile.DepthAxisMajorTicks[i];
                sb.Append(string.Format("M 0,{0} H {1}", tick, width));
            }
            //skip the last(don't draw on the axis)
            for (var i = 1; i < profile.SpeedAxisMajorTicks.Count; i++)
            {
                var tick = profile.SpeedAxisMajorTicks[i];
                sb.Append(string.Format("M {0},0 V {1}", tick, height));
            }
            return sb.ToString();
        }

        public static string GetMinorGrid(this SoundSpeedProfileViewModel profile, double height, double width)
        {
            var sb = new StringBuilder();
            foreach (var tick in profile.DepthAxisMinorTicks)
            {
                sb.Append(string.Format("M 0,{0} H {1}", tick, width));
            }
            foreach (var tick in profile.SpeedAxisMinorTicks)
            {
                sb.Append(string.Format("M {0},0 V {1}", tick, height));
            }
            return sb.ToString();
        }
    }
}
