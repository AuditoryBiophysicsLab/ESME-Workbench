using System.Collections.Generic;
using System.Windows;
using System.Xml.Serialization;
using Cinch;
using ESME.NEMO.Overlay;
using HRC.ViewModels;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
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

    public class OverlayShapeMapLayer : MapLayerViewModel
    {
        InMemoryFeatureLayer _layer;

        public OverlayShapeMapLayer()
        {
            LayerOverlay.Layers.Clear();
            PointColorMenu.Command = LineColorMenu.Command;
            PointShapePickerMenu = new List<MenuItemViewModelBase>
            {
                PointColorMenu,
                SymbolSizeMenu,
                PointStyleMenu,
            };
            PointStyle = CreatePointStyle(PointSymbolType, LineColor, (int)LineWidth);
            foreach (var item in PointStyleMenu.Children)
            {
                var item1 = item;
                item.Command = new SimpleCommand<object, object>(obj =>
                {
                    PointSymbolType = ((PointSymbolTypeMenuItemViewModel)item1).PointSymbolType;
                    MediatorMessage.Send(MediatorMessage.SetExperimentAsModified, true);
                    MediatorMessage.Send(MediatorMessage.RefreshLayer, this);
                });
            }
            CheckProperPointSymbolTypeMenu();
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

        public void Add(IEnumerable<OverlayShape> overlayShapes) { foreach (var shape in overlayShapes) Add(shape); }

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
        }
    }

    public class AnalysisPointLayer : OverlayShapeMapLayer
    {
        #region Properties for analysis point layers only

        #region public Visibility VisibleIfAnalysisPointLayer { get; set; }
        [XmlIgnore]
        public Visibility VisibleIfAnalysisPointLayer
        {
            get { return LayerType == LayerType.AnalysisPoint ? Visibility.Visible : Visibility.Hidden; }
        }
        #endregion

        #endregion


    }
}