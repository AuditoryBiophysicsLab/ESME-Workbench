using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Serialization;
using ESME.NEMO.Overlay;
using ESME.Scenarios;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    [Serializable]
    public class OverlayFileMapLayer : OverlayShapeMapLayer
    {
        #region public string OverlayFileName { get; set; }
        [XmlIgnore]
        string _overlayFileName;

        [XmlElement]
        public string OverlayFileName
        {
            get { return _overlayFileName; }
            set
            {
                if (_overlayFileName == value) return;
                _overlayFileName = value;
                //Name = Path.GetFileNameWithoutExtension(_overlayFileName);
                Clear();
                var overlayFile = new OverlayFile(_overlayFileName);
                Add(overlayFile.Shapes);
                Done();
            }
        }
        #endregion
    }

    [Serializable]
    public class OverlayShapeMapLayer : MapLayerViewModel
    {
        readonly InMemoryFeatureLayer _layer = new InMemoryFeatureLayer();

        public void Add(OverlayShape overlayShape)
        {
            if (CustomLineStyle == null)
            {
                if (LineStyle == null)
                {
                    LineColor = overlayShape.Color;
                    LineWidth = overlayShape.Width;
                }
                if (PointStyle == null)
                    PointStyle = new PointStyle(PointSymbolType.Circle, new GeoSolidBrush(GeoColor.FromArgb(LineColor.A, LineColor.R, LineColor.G, LineColor.B)), (int)LineWidth);
            }
            var wellKnownText = overlayShape.WellKnownText;
            if (wellKnownText != null) _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(overlayShape.WellKnownText)));
        }

        static string WellKnownText(string openTag, ICollection<Geo> geos, string closeTag)
        {
            if (geos.Count < 1) return null;
            return geos.Count == 1 ? string.Format("POINT({0})", GeosToStrings(geos)) : string.Format("{0}{1}{2}", openTag, string.Join(", ", GeosToStrings(geos)), closeTag);
        }
        static IEnumerable<string> GeosToStrings(IEnumerable<Geo> geos) { return geos.Select(geo => string.Format("{0} {1}", geo.Longitude, geo.Latitude)); }

        public void Add(IEnumerable<OverlayShape> overlayShapes) { foreach (var shape in overlayShapes) Add(shape); }
        public void AddLines(ICollection<Geo> geos) { _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(WellKnownText("LINESTRING(", geos, ")")))); }
        public void AddPoints(ICollection<Geo> geos) { _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(WellKnownText("MULTIPOINT(", geos, ")")))); }
        public void AddPolygon(ICollection<Geo> geos) { _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(WellKnownText("POLYGON((", geos, "))")))); }
        public void Clear() { _layer.InternalFeatures.Clear(); }
        public Action DrawAction { get; set; }

        public void Done()
        {
            if (_layer == null) return;
            if (CustomLineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(CustomLineStyle);
            else
            {
                if (LineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle = LineStyle;
                if (PointStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle = PointStyle;
            }

            _layer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            LayerOverlay.Layers.Clear();
            LayerOverlay.Layers.Add(_layer);
        }
    }
}