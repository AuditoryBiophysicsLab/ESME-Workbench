using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment;
using HRC.Aspects;

namespace ESME.Views.Environment
{
    [NotifyPropertyChanged]
    public class SoundSpeedProfileViewModel
    {
        public SoundSpeedProfileView View { get; set; }

        public float SSPMax { get; set; }
        public float SSPMin { get; set; }
        public float DepthMin { get; set; }
        public float DepthMax { get; set; }
        #region public string SoundSpeedGeometry { get; private set; }
        private string _soundSpeedGeometry = "M 0,0";
        public string SoundSpeedGeometry
        {
            get { return _soundSpeedGeometry; }
            private set
            {
                if (_soundSpeedGeometry == value) return;
                _soundSpeedGeometry = value;
            }
        } 
        #endregion

        SoundSpeedProfile _soundSpeedProfile;
        public SoundSpeedProfile SoundSpeedProfile
        {
            get { return _soundSpeedProfile; }
            set
            {
                _soundSpeedProfile = value;
                var speeds = (from p in _soundSpeedProfile.Data
                              orderby p.SoundSpeed
                              select p.SoundSpeed).ToArray();

                SSPMin = speeds.First();
                SSPMax = speeds.Last();
                DepthMin = _soundSpeedProfile.Data.First().Depth;
                DepthMax = _soundSpeedProfile.Data.Last().Depth;
                CalculateSoundSpeedProfileGeometry();
            }
        }

        void CalculateSoundSpeedProfileGeometry()
        {
            if (SoundSpeedProfile == null) return;
            var actualControlHeight = View.OverlayCanvas.ActualHeight;
            var actualControlWidth = View.OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            
            var sb = new StringBuilder();
            foreach (var t in SoundSpeedProfile.Data)
            {
                var y = t.Depth * (actualControlHeight / DepthMax);
                var x = t.SoundSpeed * (actualControlWidth / SSPMax);
                sb.Append(sb.Length == 0 ? string.Format("M 0,{0} ", y) : string.Format("L {0},{1} ", x, y));
            }
            SoundSpeedGeometry = sb.ToString();
        }

        void DrawGrid()
        {

            if (SoundSpeedProfile == null) return;
            var actualControlHeight = View.OverlayCanvas.ActualHeight;
            var actualControlWidth = View.OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;

            var sb = new StringBuilder();

        }

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
