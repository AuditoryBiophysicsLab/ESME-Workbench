using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Windows.Media;
using ESME.Overlay;
using ThinkGeo.MapSuite.Core;
using LineStyle = ThinkGeo.MapSuite.Core.LineStyle;

namespace ESMEWorkBench.ViewModels.Map
{
    public class OverlayShapesMapLayer : MapLayer
    {
        Color _color;
        InMemoryFeatureLayer _newLayer;
        float _width;

        public OverlayShapesMapLayer()
        {
            OverlayShapes = new ObservableCollection<OverlayShape>();
            ShapeLayers = new ObservableCollection<InMemoryFeatureLayer>();
        }

        public OverlayShapesMapLayer(IEnumerable<OverlayShape> shapes) : this()
        {
            foreach (var shape in shapes) OverlayShapes.Add(shape);
            CommitShapes();
        }

        public ObservableCollection<InMemoryFeatureLayer> ShapeLayers
        {
            get { return _shapeLayers; }
            set
            {
                if (_shapeLayers == value) return;
                _shapeLayers = value;
                _shapeLayers.CollectionChanged += ShapeLayersCollectionChanged;
            }
        }
        ObservableCollection<InMemoryFeatureLayer> _shapeLayers;

        public LineStyle LineStyle { get; set; }

        public PointStyle PointStyle { get; set; }

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null)
            {
                foreach (var newLayer in e.NewItems.Cast<InMemoryFeatureLayer>()) 
                {
                    if ((LineStyle == null) && (PointStyle == null))
                    {
                        newLayer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle.OuterPen = new GeoPen(GeoColor.FromArgb(_color.A, _color.R, _color.G, _color.B), _width);
                        newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolPen = new GeoPen(GeoColor.FromArgb(_color.A, _color.R, _color.G, _color.B), _width);
                        newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolSize = _width;
                        newLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle.SymbolType = PointSymbolType.Circle;
                    }
                    else
                    {
                        if (LineStyle != null) newLayer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(LineStyle);
                        if (PointStyle != null) newLayer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(PointStyle);
                    }

                    newLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;

                    Layers.Add(newLayer);
                }
            }
            if (e.OldItems == null) return;
            foreach (var oldLayer in e.OldItems.Cast<InMemoryFeatureLayer>())
                Layers.Remove(oldLayer);
        }

        public ObservableCollection<OverlayShape> OverlayShapes
        {
            get { return _overlayShapes; }
            set
            {
                if (_overlayShapes == value) return;
                _overlayShapes = value;
                _overlayShapes.CollectionChanged += OverlayShapesCollectionChanged;
            }
        }
        ObservableCollection<OverlayShape> _overlayShapes;

        void OverlayShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            foreach (var item in e.NewItems)
            {
                var shape = (OverlayShape)item;
                if (_newLayer != null)
                {
                    if ((shape.Color != _color) || (shape.Width != _width)) CommitShapes();
                }
                if (_newLayer == null)
                {
                    _newLayer = new InMemoryFeatureLayer();
                    _color = shape.Color;
                    _width = shape.Width;
                }
                _newLayer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(shape.WellKnownText)));
            }
        }

        public void CommitShapes()
        {
            ShapeLayers.Add(_newLayer);
            _newLayer = null;
        }
    }

    class OverlayShapesMapLayerImpl : OverlayShapesMapLayer {}
}