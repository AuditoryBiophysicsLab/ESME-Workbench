using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
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
        public OverlayFileMapLayer()
        {
            LayerType = LayerType.OverlayFile;
            LayerOverlay.Layers.Clear();
        }

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
        InMemoryFeatureLayer _layer;

        public OverlayShapeMapLayer()
        {
            LayerOverlay.Layers.Clear();
            PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
            while (PointSymbolType == PointSymbolType.Cross) PointSymbolType = (PointSymbolType)(Random.Next(8));
        }

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
                    PointStyle = new PointStyle(PointSymbolType.Circle,
                                                new GeoSolidBrush(GeoColor.FromArgb(LineColor.A, LineColor.R,
                                                                                    LineColor.G, LineColor.B)),
                                                (int)LineWidth);
            }
            var wellKnownText = overlayShape.WellKnownText;
            if (wellKnownText != null)
                _layer.InternalFeatures.Add(
                                            new Feature(
                                                BaseShape.CreateShapeFromWellKnownData(overlayShape.WellKnownText)));
        }

        static string WellKnownText(ICollection<Geo> geos, bool isPointData)
        {
            if (geos.Count < 1) return null;
            if (geos.Count == 1) return string.Format("POINT({0} {1})", geos.First().Latitude, geos.First().Longitude);
            var retval = new StringBuilder();
            retval.Append(isPointData ? "MULTIPOINT(" : "LINESTRING(");
            foreach (var geo in geos) retval.Append(string.Format("{0} {1}, ", geo.Longitude, geo.Latitude));
            retval.Remove(retval.Length - 2, 2); // Lose the last comma and space
            retval.Append(")");
            return retval.ToString();
        }

        public void Add(IEnumerable<OverlayShape> overlayShapes) { foreach (var shape in overlayShapes) Add(shape); }
        public void Add(ICollection<Geo> geos, bool isPointData = false)
        {
            _layer = new InMemoryFeatureLayer();
            _layer.InternalFeatures.Add(new Feature(BaseShape.CreateShapeFromWellKnownData(WellKnownText(geos, isPointData))));
        }

        public void Clear() { if (_layer != null) _layer.InternalFeatures.Clear(); }

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
            LayerOverlay.Layers.Add(_layer);
            //MediatorMessage.Send(MediatorMessage.AddMapLayer, this);
            //MediatorMessage.Send(MediatorMessage.RefreshMapLayer, this);
        }

        [XmlIgnore]
        public override Visibility IsLineColorVisible
        {
            get { return Visibility.Visible; }
        }

    }

    [Serializable]
    public class AnalysisPointLayer : OverlayShapeMapLayer
    {
        public AnalysisPointLayer()
        {
            LayerType = LayerType.AnalysisPoint;
        }

        AnalysisPoint _analysisPoint;

        [XmlIgnore]
        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                _analysisPoint = value;
                if (_analysisPoint != null) _analysisPoint.PropertyChanged += (s, e) =>
                {
                    var ap = (AnalysisPoint)s;
                    ap.Validate();
                    ValidationErrorText = ap.ValidationErrorText;
                };
            }
        }

        public override void Validate()
        {
            if (AnalysisPoint == null)
            {
                ValidationErrorText = "Unable to validate - AnalysisPoint is null";
                return;
            }
            AnalysisPoint.Validate();
            ValidationErrorText = AnalysisPoint.ValidationErrorText;
        }
    }

    [Serializable]
    public class PropagationLayer : OverlayShapeMapLayer
    {
        public PropagationLayer() 
        {
            LayerType = LayerType.Propagation;
        }

        Scenarios.TransmissionLoss _transmissionLoss;

        [XmlIgnore]
        public Scenarios.TransmissionLoss TransmissionLoss
        {
            get { return _transmissionLoss; }
            set
            {
                _transmissionLoss = value;
                Validate();
            }
        }

        public override void Validate()
        {
            if (TransmissionLoss == null)
            {
                ValidationErrorText = "Unable to validate - TransmissionLoss is null";
                return;
            }
            TransmissionLoss.Validate();
            ValidationErrorText = TransmissionLoss.ValidationErrorText;
        }
    }
}