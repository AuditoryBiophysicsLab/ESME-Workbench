using System;
using System.Windows.Media;
using System.Xml.Serialization;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Map
{
    public class MapLayer : LayerOverlay
    {
        static Random _random;

        #region public string Name { get; set; }

        public new string Name
        {
            get { return base.Name; }
            set
            {
                if (base.Name == value) return;
                base.Name = value;
            }
        }

        #endregion

        #region public Color LineColor { get; set; }

        [XmlElement]
        public Color LineColor
        {
            get { return _lineColor; }
            set
            {
                if (_lineColor == value) return;
                _lineColor = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                LineStyle = CreateLineStyle(LineColor, LineWidth);
            }
        }

        Color _lineColor = Color.FromArgb(0, 0, 0, 0);

        #endregion

        #region public float LineWidth { get; set; }

        [XmlElement]
        public float LineWidth
        {
            get { return _lineWidth; }
            set
            {
                if (_lineWidth == value) return;
                _lineWidth = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                LineStyle = CreateLineStyle(LineColor, LineWidth);
            }
        }

        float _lineWidth = 1f;

        #endregion

        #region public Color AreaColor { get; set; }

        [XmlElement]
        public Color AreaColor
        {
            get { return _areaColor; }
            set
            {
                if (_areaColor == value) return;
                _areaColor = value;
                AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            }
        }

        Color _areaColor = Color.FromArgb(0, 0, 0, 0);

        #endregion

        #region public AreaStyle AreaStyle { get; set; }

        [XmlIgnore]
        public AreaStyle AreaStyle
        {
            get { return _areaStyle; }
            set
            {
                if (_areaStyle == value) return;
                LineColor = Color.FromArgb(value.OutlinePen.Color.AlphaComponent, value.OutlinePen.Color.RedComponent, value.OutlinePen.Color.GreenComponent, value.OutlinePen.Color.BlueComponent);
                LineWidth = value.OutlinePen.Width;
                AreaColor = Color.FromArgb(value.FillSolidBrush.Color.AlphaComponent, value.FillSolidBrush.Color.RedComponent, value.FillSolidBrush.Color.GreenComponent, value.FillSolidBrush.Color.BlueComponent);
                _areaStyle = value;
                if (Layers.Count == 0) return;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = _areaStyle;
                ((FeatureLayer) Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        AreaStyle _areaStyle;

        #endregion

        #region public LineStyle LineStyle { get; set; }

        [XmlIgnore]
        public LineStyle LineStyle
        {
            get { return _lineStyle; }
            set
            {
                if (_lineStyle == value) return;
                _lineStyle = value;
                if (Layers.Count == 0) return;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultLineStyle = _lineStyle;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        LineStyle _lineStyle;

        #endregion

        #region public LineStyle CustomLineStyle { get; set; }

        public LineStyle CustomLineStyle
        {
            get { return _customLineStyle; }
            set
            {
                if (_customLineStyle == value) return;
                _customLineStyle = value;
                if (Layers.Count == 0) return;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.CustomStyles.Add(_customLineStyle);
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        LineStyle _customLineStyle;

        #endregion


        #region public PointStyle PointStyle { get; set; }

        [XmlIgnore]
        public PointStyle PointStyle
        {
            get { return _pointStyle; }
            set
            {
                if (_pointStyle == value) return;
                _pointStyle = value;
                if (Layers.Count == 0) return;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.DefaultPointStyle = _pointStyle;
                ((FeatureLayer)Layers[0]).ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            }
        }

        PointStyle _pointStyle;

        #endregion

        #region public LayerType LayerType { get; set; }

        public LayerType LayerType
        {
            get { return _layerType; }
            set
            {
                if (_layerType == value) return;
                _layerType = value;
            }
        }

        LayerType _layerType;

        #endregion
        
        #region Helpers

        static AreaStyle CreateAreaStyle(Color pen, float width, Color brush) { return new AreaStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width), new GeoSolidBrush(GeoColor.FromArgb(brush.A, brush.R, brush.G, brush.B))); }

        static LineStyle CreateLineStyle(Color pen, float width) { return new LineStyle(new GeoPen(GeoColor.FromArgb(pen.A, pen.R, pen.G, pen.B), width)); }

        [XmlIgnore]
        public static AreaStyle RandomAreaStyle
        {
            get { return CreateAreaStyle(RandomColor, 1f, RandomColor); }
        }

        [XmlIgnore]
        public static LineStyle RandomLineStyle
        {
            get { return CreateLineStyle(RandomColor, 1f); }
        }

        [XmlIgnore]
        static Color RandomColor
        {
            get
            {
                if (_random == null) _random = new Random();
                var randomBytes = new byte[3];
                _random.NextBytes(randomBytes);
                return Color.FromArgb(255, randomBytes[0], randomBytes[1], randomBytes[2]);
            }
        }

        #endregion

        [XmlIgnore]
        public LayerViewModel LayerViewModel { get; set; }
        [XmlIgnore]
        public int MapLayerIndex { get; set; }
    }
}