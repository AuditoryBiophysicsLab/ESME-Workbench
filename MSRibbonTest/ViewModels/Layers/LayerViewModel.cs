using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Windows.Media;
using Cinch;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMERibbonDemo.ViewModels.Layers
{
    public class LayerViewModel<T> : ViewModelBase
        where T : Layer
    {
        #region public string Name { get; set; }

        private static readonly PropertyChangedEventArgs NameChangeArgs =
            ObservableHelper.CreateArgs<LayerViewModel<T>>(x => x.Name);

        private string _name;

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangeArgs);
            }
        }

        #endregion

        #region public T Layer { get; set; }

        private static readonly PropertyChangedEventArgs LayerChangeArgs =
            ObservableHelper.CreateArgs<LayerViewModel<T>>(x => x.Layer);

        private T _layer;

        public T Layer
        {
            get { return _layer; }
            set
            {
                if (_layer == value) return;
                _layer = value;
                NotifyPropertyChanged(LayerChangeArgs);
            }
        }

        #endregion

        #region public string FileName { get; set; }

        private static readonly PropertyChangedEventArgs FileNameChangeArgs =
            ObservableHelper.CreateArgs<LayerViewModel<T>>(x => x.FileName);

        private string _fileName;

        public string FileName
        {
            get { return _fileName; }
            set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangeArgs);
            }
        }

        #endregion

        private readonly ICollection<LayerViewModel<T>> _layers = new ObservableCollection<LayerViewModel<T>>();

        public ICollection<LayerViewModel<T>> Layers
        {
            get { return _layers; }
        }

        public WpfMap WpfMap { get; set; }
    }

    public class ShapeLayerViewModel : LayerViewModel<InMemoryFeatureLayer>
    {
        #region public Color Color { get; set; }

        private static readonly PropertyChangedEventArgs ColorChangeArgs =
            ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.Color);

        private Color _color;

        public Color Color
        {
            get { return _color; }
            set
            {
                if (_color == value) return;
                _color = value;
                Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen =
                    new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(ColorChangeArgs);
            }
        }

        #endregion

        #region public float LineWidth { get; set; }

        private static readonly PropertyChangedEventArgs LineWidthChangeArgs =
            ObservableHelper.CreateArgs<ShapeLayerViewModel>(x => x.LineWidth);

        private float _lineWidth;

        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                if (_lineWidth == value) return;
                _lineWidth = value;
                Layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen =
                    new GeoPen(GeoColor.FromArgb(Color.A, Color.R, Color.G, Color.B), LineWidth);
                NotifyPropertyChanged(LineWidthChangeArgs);
            }
        }

        #endregion
    }
}