using System;
using System.Windows;
using System.Windows.Media;
using HRC.Services;
using HRC.Utility;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Mapping
{
    [Serializable]
    public class MapLayerViewModel : ViewModelBase
    {
        protected static readonly Random Random;
        static readonly Color[] Palette;
        static int _paletteIndex;
        #region Helpers
        protected static Color RandomColor
        {
            get
            {
                var color = Palette[_paletteIndex++];
                _paletteIndex %= Palette.Length;
                return color;
            }
        }

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

        static MapLayerViewModel()
        {
            Random = new Random();
            Palette = ExtensionMethods.CreateHSVPalette(60);
        }

        public MapLayerViewModel()
        {
            LayerOverlay = new LayerOverlay
            {
                TileType = TileType.SingleTile,
            };
            LineColorBrush = new SolidColorBrush(_lineColor);
            AreaColor = Colors.Transparent;
            AreaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
            LineStyle = CreateLineStyle(LineColor, LineWidth);
        }

        public string Name { get; set; }
        public static IHRCColorPickerService ColorPickerService { get; set; }
        public static GeoCollection<Overlay> MapOverlay { get; set; }

        public LayerOverlay LayerOverlay { get; set; }

        public virtual Visibility IsLineColorVisible
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return Visibility.Hidden;
                    case LayerType.BaseMap:
                    case LayerType.Shapefile:
                    case LayerType.SimArea:
                    case LayerType.OpArea:
                    case LayerType.Bathymetry:
                    case LayerType.Animal:
                    case LayerType.SoundSpeed:
                    case LayerType.BottomType:
                    case LayerType.WindSpeed:
                    case LayerType.Pressure:
                        return Visibility.Visible;
                }
            }
        }

        public Visibility IsAreaColorVisible
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return Visibility.Hidden;
                    case LayerType.BaseMap:
                    case LayerType.Shapefile:
                        return Visibility.Visible;
                }
            }
        }

        public bool IsFeatureLayer
        {
            get
            {
                switch (LayerType)
                {
                    default:
                        return false;
                    case LayerType.Animal:
                    case LayerType.SoundSpeed:
                    case LayerType.BottomType:
                    case LayerType.WindSpeed:
                        return true;
                }
            }
        }

        public LayerType LayerType { get; set; }

        bool _isEnabled = true;
        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                _isEnabled = value;
                if (!_isEnabled) IsChecked = false;
            }
        }
        bool _isChecked;
        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                _isChecked = value;
                if (LayerOverlay != null) MediatorMessage.Send(_isChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, LayerOverlay);
            }
        }

        public Brush LineColorBrush { get; set; }

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
        float _lineWidth = Random.Next(2, 10) / 2.0f;
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

        Color _areaColor = RandomColor;
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

        Color _lineColor = RandomColor;
        public Color LineColor
        {
            get { return _lineColor; }
            set
            {
                if (_lineColor == value) return;
                _lineColor = value;
                _areaStyle = CreateAreaStyle(LineColor, LineWidth, AreaColor);
                _lineStyle = CreateLineStyle(LineColor, LineWidth);
                _pointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)Math.Max(1, LineWidth));
                LineColorBrush = new SolidColorBrush(_lineColor);
                MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
            }
        }

        #region public PointSymbolType PointSymbolType { get; set; }

        PointSymbolType _pointSymbolType = (PointSymbolType)(Random.Next(8));
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