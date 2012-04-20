using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment;
using HRC.Aspects;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.Environment
{
    [ExportViewModel("SoundSpeedProfileViewModel")]
    [NotifyPropertyChanged]
    class SoundSpeedProfileViewModel
    {
        private readonly SoundSpeedProfile _profile;
        private readonly SoundSpeedProfileView _view;
        
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

        [ImportingConstructor]
        public SoundSpeedProfileViewModel(SoundSpeedProfile profile, SoundSpeedProfileView view )
        {
            _profile = profile;
            _view = view;
            var speeds = (from p in profile.Data
                      orderby p.SoundSpeed
                      select p.SoundSpeed).ToArray();

            SSPMin = speeds.First();
            SSPMax = speeds.Last();
            DepthMin = profile.Data.First().Depth;
            DepthMax = profile.Data.Last().Depth;
            CalculateSoundSpeedProfileGeometry();
        }

        void CalculateSoundSpeedProfileGeometry()
        {
            if (_profile == null) return;
            var actualControlHeight = _view.OverlayCanvas.ActualHeight;
            var actualControlWidth = _view.OverlayCanvas.ActualWidth;
            if (actualControlHeight == 0 || actualControlWidth == 0) return;
            
            var sb = new StringBuilder();
            foreach (var t in _profile.Data)
            {
                var y = t.Depth * (actualControlHeight / DepthMax);
                var x = t.SoundSpeed * (actualControlWidth / SSPMax);
                sb.Append(sb.Length == 0 ? string.Format("M 0,{0} ", y) : string.Format("L {0},{1} ", x, y));
            }
            SoundSpeedGeometry = sb.ToString();
        }

        void DrawGrid()
        {

            if (_profile == null) return;
            var actualControlHeight = _view.OverlayCanvas.ActualHeight;
            var actualControlWidth = _view.OverlayCanvas.ActualWidth;
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
