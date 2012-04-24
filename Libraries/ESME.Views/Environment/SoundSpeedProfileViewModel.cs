using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using ESME.Environment;
using HRC.Services;
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
        public string SoundSpeedGeometry { get; private set; }
        public string SoundSpeedDataPoints { get; private set; }
        public string WindowTitle { get; set; }

        #region public SoundSpeedProfile SoundSpeedProfile {get; set; }
        SoundSpeedProfile _soundSpeedProfile;
        public SoundSpeedProfile SoundSpeedProfile
        {
            get { return _soundSpeedProfile; }
            set
            {
                _soundSpeedProfile = value;
                CalculateSoundSpeedProfileGeometry();
                WindowTitle = string.Format("Sound Speed Profile ({0:0.000}, {1:0.000})", _soundSpeedProfile.Latitude, _soundSpeedProfile.Longitude);
            }
        }
        #endregion

        public SoundSpeedProfileViewModel(IHRCSaveFileService saveFile)
        {
            _saveFileService = saveFile;
        }

        void CalculateSoundSpeedProfileGeometry()
        {
            if (SoundSpeedProfile == null) return;
            var actualControlHeight = View.OverlayCanvas.ActualHeight;
            var actualControlWidth = View.OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;

            var speeds = (from p in SoundSpeedProfile.Data
                          select p.SoundSpeed).ToArray();
            var depths = (from d in SoundSpeedProfile.Data
                          select d.Depth).ToArray();
            SSPMin = speeds.Min();
            SSPMax = speeds.Max();
            var diff = SSPMax - SSPMin;
            SSPMin -= (float).1 * diff;
            SSPMax += (float).1 * diff;
            diff = SSPMax - SSPMin;
            DepthMin = depths.Min();
            DepthMax = depths.Max();

            var orderedData = (from d in SoundSpeedProfile.Data
                               orderby d.Depth
                               select d);
            var sb = new StringBuilder();
            var pb = new StringBuilder();
            foreach (var t in orderedData)
            {
                var y = t.Depth * (actualControlHeight / DepthMax);
                var x = (t.SoundSpeed - SSPMin) * (actualControlWidth / diff);
                sb.Append(sb.Length == 0 ? string.Format("M {0},{1} ", x, y) : string.Format("L {0},{1} ", x, y));
                pb.Append(pb.Length == 0 ? string.Format("M {0},{1} ", x, y) : CircleAt(x, y, 5));
            }
            SoundSpeedGeometry = sb.ToString();
            SoundSpeedDataPoints = pb.ToString();
        }

        string CircleAt(double x, double y, double d)
        {
            var sb = new StringBuilder();
            sb.Append(string.Format("M {0},{1} ", x - (d / 2), y));
            sb.Append(string.Format("a {0},{0} 180 1 1 {1},0 ", d / 2, d));
            sb.Append(string.Format("{0},{0} 180 1 1 {1},0 ", d / 2, -d));
            sb.Append(string.Format("M {0},{1} ", x, y));
            return sb.ToString();
        }

        string SquareAt(double x, double y, double d)
        {
            var sb = new StringBuilder();
            sb.Append(string.Format("M {0},{1} ", x - (d / 2), y));
            sb.Append(string.Format("A {0},{0} 180 1 1 {1},{2} ", d / 2, x + (d / 2), y));
            sb.Append(string.Format("A {0},{0} 180 1 1 {1},{2} ", d / 2, x - (d / 2), y));
            sb.Append(string.Format("M {0},{1} ", x, y));
            return sb.ToString();
        }


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
            CalculateSoundSpeedProfileGeometry();
        }

        #endregion
    }
}
