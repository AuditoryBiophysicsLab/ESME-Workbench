using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Windows.Media;
using Cinch;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;
using LineStyle = ThinkGeo.MapSuite.Core.LineStyle;

namespace ESMEWorkbench.ViewModels.Layers
{
#if false
    public class OverlayShapesLayerViewModel : LayerViewModel
    {
        Color _color;
        InMemoryFeatureLayer _newLayer;
        float _width;

        public OverlayShapesLayerViewModel(string name, IEnumerable<OverlayShape> shapes, LayerTreeViewModel layerTreeViewModel) 
            : base(name, null, layerTreeViewModel)
        {
            OverlayShapes = new ObservableCollection<OverlayShape>();
            ShapeLayers = new ObservableCollection<InMemoryFeatureLayer>();

            foreach (var shape in shapes) OverlayShapes.Add(shape);
            CommitShapes();
        }

        public OverlayShapesLayerViewModel(Overlay layerOverlay, string name, LayerTreeViewModel layerTreeViewModel)
            : base(name, null, layerTreeViewModel)
        {
            OverlayShapes = new ObservableCollection<OverlayShape>();
            ShapeLayers = new ObservableCollection<InMemoryFeatureLayer>();
            if (layerOverlay != null) Overlay = layerOverlay;
        }

        #region public ObservableCollection<InMemoryFeatureLayer> ShapeLayers { get; set; }

        static readonly PropertyChangedEventArgs ShapeLayersChangedEventArgs = ObservableHelper.CreateArgs<OverlayShapesLayerViewModel>(x => x.ShapeLayers);
        ObservableCollection<InMemoryFeatureLayer> _shapeLayers;

        public ObservableCollection<InMemoryFeatureLayer> ShapeLayers
        {
            get { return _shapeLayers; }
            set
            {
                if (_shapeLayers == value) return;
                _shapeLayers = value;
                _shapeLayers.CollectionChanged += ShapeLayersCollectionChanged;
                NotifyPropertyChanged(ShapeLayersChangedEventArgs);
            }
        }

        public LineStyle LineStyle { get; set; }

        public PointStyle PointStyle { get; set; }

        void ShapeLayersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null)
            {
                foreach (var item in e.NewItems)
                {
                    var newLayer = (InMemoryFeatureLayer) item;
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

                    ((LayerOverlay) Overlay).Layers.Add(newLayer);
                }
                //LayerOverlay.Refresh();
            }
            if (e.OldItems != null)
            {
                foreach (var item in e.OldItems)
                {
                    var oldLayer = (InMemoryFeatureLayer) item;
                    ((LayerOverlay) Overlay).Layers.Remove(oldLayer);
                }
                //LayerOverlay.Refresh();
            }
            NotifyPropertyChanged(ShapeLayersChangedEventArgs);
            //WpfMap.Refresh();
        }

        #endregion

        #region public ObservableCollection<OverlayShape> OverlayShapes { get; set; }

        static readonly PropertyChangedEventArgs OverlayShapesChangedEventArgs = ObservableHelper.CreateArgs<OverlayShapesLayerViewModel>(x => x.OverlayShapes);
        ObservableCollection<OverlayShape> _overlayShapes;

        public ObservableCollection<OverlayShape> OverlayShapes
        {
            get { return _overlayShapes; }
            set
            {
                if (_overlayShapes == value) return;
                _overlayShapes = value;
                _overlayShapes.CollectionChanged += OverlayShapesCollectionChanged;
                NotifyPropertyChanged(OverlayShapesChangedEventArgs);
            }
        }

        void OverlayShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            foreach (var item in e.NewItems)
            {
                var shape = (OverlayShape) item;
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
            NotifyPropertyChanged(OverlayShapesChangedEventArgs);
        }

        #endregion

        public void CommitShapes()
        {
            ShapeLayers.Add(_newLayer);
            _newLayer = null;
        }
    }
#endif
}