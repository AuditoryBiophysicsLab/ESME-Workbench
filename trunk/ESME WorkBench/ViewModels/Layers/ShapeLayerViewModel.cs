using System.ComponentModel;
using System.Drawing;
using Cinch;
using ESME.Overlay;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Layers
{
#if false
    public class ShapeLayerViewModel : LayerViewModel<InMemoryFeatureLayer>
    {
        public ShapeLayerViewModel(string name, OverlayShape shape, LayerTreeViewModel layerTreeViewModel) : base(name, null, layerTreeViewModel) { }

        #region public Color Color { get; set; }

        static readonly PropertyChangedEventArgs ColorChangedEventArgs = ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.Color);
        Color _color;

        public Color Color
        {
            get { return _color; }
            set
            {
                if (_color == value) return;
                _color = value;
                //Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(ColorChangedEventArgs);
            }
        }

        #endregion

        #region public float LineWidth { get; set; }

        static readonly PropertyChangedEventArgs LineWidthChangedEventArgs = ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.LineWidth);
        float _lineWidth;

        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                if (_lineWidth == value) return;
                _lineWidth = value;
                //Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(LineWidthChangedEventArgs);
            }
        }

        #endregion
    }
#endif
}