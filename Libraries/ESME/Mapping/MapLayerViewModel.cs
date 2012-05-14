using System;
using System.Windows.Media;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Mapping
{
    [Serializable]
    public class MapLayerViewModel : ViewModelBase
    {
        #region Helpers
        public static AreaStyle CreateAreaStyle(Color pen, float width, Color brush)
        {
            if (brush == Colors.Transparent) return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width));
            return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width),
                                 new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B)));
        }

        public static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }
        public static PointStyle CreatePointStyle(PointSymbolType pointSymbolType, Color pen, int width)
        {
            return new PointStyle(pointSymbolType, new GeoSolidBrush(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B)),
                                  width);
        }
        #endregion

        public MapLayerViewModel()
        {
            LayerOverlay = new LayerOverlay { TileType = TileType.SingleTile };
            AreaColor = Colors.Transparent;
            AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            LineStyle = CreateLineStyle(LineColor, LineWidth);
        }

        string _name;

        public string Name
        {
            get { return _name; }
            set
            {
                _name = value;
                if (LayerOverlay != null) LayerOverlay.Name = _name;
            }
        }

        LayerOverlay _layerOverlay;

        public LayerOverlay LayerOverlay
        {
            get { return _layerOverlay; }
            set
            {
                _layerOverlay = value;
                if (_layerOverlay != null && Name != null) _layerOverlay.Name = Name;
            }
        }

        #region AreaStyle AreaStyle { get; set; }
        AreaStyle _areaStyle;
        public AreaStyle AreaStyle
        {
            get { return _areaStyle; }
            set
            {
                _areaStyle = value;
                if (_areaStyle == null) return;

                _lineColor = Color.FromArgb(value.OutlinePen.Color.AlphaComponent, value.OutlinePen.Color.RedComponent,
                                           value.OutlinePen.Color.GreenComponent, value.OutlinePen.Color.BlueComponent);
                _lineWidth = value.OutlinePen.Width;
                _areaColor = Color.FromArgb(value.FillSolidBrush.Color.AlphaComponent,
                                           value.FillSolidBrush.Color.RedComponent,
                                           value.FillSolidBrush.Color.GreenComponent,
                                           value.FillSolidBrush.Color.BlueComponent);
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = _areaStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public LineStyle LineStyle { get; set; }
        LineStyle _lineStyle;
        public LineStyle LineStyle
        {
            get { return _lineStyle; }
            set
            {
                _lineStyle = value;
                if (_lineStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = _lineStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public LineStyle CustomLineStyle { get; set; }
        LineStyle _customLineStyle;

        public LineStyle CustomLineStyle
        {
            get { return _customLineStyle; }
            set
            {
                _customLineStyle = value;
                if (_customLineStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(_customLineStyle);
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public PointStyle PointStyle { get; set; }
        PointStyle _pointStyle;

        public PointStyle PointStyle
        {
            get { return _pointStyle; }
            set
            {
                _pointStyle = value;
                if (_pointStyle == null) return;
                if (LayerOverlay.Layers.Count == 0) return;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = _pointStyle;
                ((FeatureLayer)LayerOverlay.Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel =
                    ApplyUntilZoomLevel.Level20;
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public float LineWidth { get; set; }
        float _lineWidth;
        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                _lineWidth = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                _lineStyle = CreateLineStyle(LineColor, LineWidth);
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public Color AreaColor { get; set; }
        Color _areaColor;
        public Color AreaColor
        {
            get { return _areaColor; }
            set
            {
                _areaColor = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public Color LineColor { get; set; }
        Color _lineColor;
        public Color LineColor
        {
            get { return _lineColor; }
            set
            {
                _lineColor = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                _lineStyle = CreateLineStyle(LineColor, LineWidth);
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)Math.Max(1, LineWidth));
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }
        #endregion

        #region public PointSymbolType PointSymbolType { get; set; }
        PointSymbolType _pointSymbolType;
        public PointSymbolType PointSymbolType
        {
            get { return _pointSymbolType; }
            set
            {
                _pointSymbolType = value;
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
            }
        }
        #endregion
    }
}