﻿using System.Collections.Generic;
using System.Xml.Serialization;
using ESME.NEMO.Overlay;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    public class OverlayFileMapLayer : OverlayShapeMapLayer
    {
        public OverlayFileMapLayer() { LayerType = LayerType.OverlayFile; LayerOverlay.Layers.Clear(); }

        #region public string OverlayFileName { get; set; }
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
        [XmlIgnore]
        string _overlayFileName;

        #endregion
    }

    public class OverlayShapeMapLayer : MapLayerViewModel
    {
        public OverlayShapeMapLayer() { LayerOverlay.Layers.Clear(); }

        InMemoryFeatureLayer _layer;
        public void Add(OverlayShape overlayShape)
        {
            if (_layer == null) _layer = new InMemoryFeatureLayer();
            if (CustomLineStyle == null)
            {
                if (LineStyle == null)
                {
                    LineColor = overlayShape.Color;
                    LineWidth = overlayShape.Width;
                }
                if (PointStyle == null)
                {
                    PointStyle = new PointStyle(PointSymbolType.Circle, new GeoSolidBrush(GeoColor.FromArgb(LineColor.A, LineColor.R, LineColor.G, LineColor.B)), (int) LineWidth);
                }
            }
            var wellKnownText = overlayShape.WellKnownText;
            if (wellKnownText != null) _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(overlayShape.WellKnownText)));
        }

        public void Add(IEnumerable<OverlayShape> overlayShapes)
        {
            foreach (var shape in overlayShapes) Add(shape);
        }

        public void Clear() { if (_layer != null) _layer.InternalFeatures.Clear(); }

        public void Done()
        {
            if (_layer == null) return;
            if (CustomLineStyle != null)
            {
                _layer.ZoomLevelSet.ZoomLevel01.CustomStyles.Add(CustomLineStyle);
            }
            else
            {

                if (LineStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultLineStyle = LineStyle;
                if (PointStyle != null) _layer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle = PointStyle;
            }

            _layer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
            LayerOverlay.Layers.Add(_layer);
        }
    }
}