using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using ESME.Overlay;
using ESMEWorkBench.ViewModels.Layers;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    public class OverlayFileMapLayer : OverlayShapeMapLayer
    {
        public OverlayFileMapLayer() { LayerType = LayerType.OverlayFile; Layers.Clear(); }

        #region public string OverlayFileName { get; set; }
        [XmlElement]
        public string OverlayFileName
        {
            get { return _overlayFileName; }
            set
            {
                if (_overlayFileName == value) return;
                _overlayFileName = value;
                Name = Path.GetFileNameWithoutExtension(_overlayFileName);

                var overlayFile = new OverlayFile(_overlayFileName);
                Add(overlayFile.Shapes);
                Done();
            }
        }
        [XmlIgnore]
        string _overlayFileName;

        #endregion
    }

    public class OverlayShapeMapLayer : MapLayer
    {
        public OverlayShapeMapLayer() { Layers.Clear(); }

        InMemoryFeatureLayer _layer;
        public void Add(OverlayShape overlayShape)
        {
            if (_layer == null) _layer = new InMemoryFeatureLayer();
            if (LineStyle == null)
            {
                LineColor = overlayShape.Color;
                LineWidth = overlayShape.Width;
            }
            if (PointStyle == null)
            {
                PointStyle = new PointStyle(PointSymbolType.Circle, new GeoSolidBrush(GeoColor.FromArgb(LineColor.A, LineColor.R, LineColor.G, LineColor.B)), (int)LineWidth);
            }

            _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(overlayShape.WellKnownText)));
        }

        public void Add(IEnumerable<OverlayShape> overlayShapes)
        {
            foreach (var shape in overlayShapes) Add(shape);
        }

        public void Done()
        {
            if (_layer == null) return;
            //_layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Clear();
            if (LineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(LineStyle);
            if (PointStyle != null) _layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(PointStyle);
            _layer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            Layers.Add(_layer);
        }
    }
}