using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Windows.Media;
using ESME.NEMO.Overlay;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    public class MapLayerCollection : ObservableCollection<MapLayerViewModel>
    {
        public T Find<T>(LayerType layerType, string layerName) where T : MapLayerViewModel
        {
            if (Count == 0) return null;
            return this.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault() as T;
        }

        #region public bool IsActive { get; set; }

        public bool IsActive
        {
            get { return _isActive; }
            set
            {
                if (_isActive == value) return;
                _isActive = value;
                if (_isActive) MediatorMessage.Send(MediatorMessage.SetMapLayers, this);
            }
        }

        bool _isActive;

        #endregion

        public new void Add(MapLayerViewModel item)
        {
            item.MapLayers = this;
            if (IndexOf(item) != -1) return;
            base.Add(item);
        }

        public void DisplayShapeFile(string layerName, string shapeFileName)
        {
            DisplayShapeFile(layerName, shapeFileName, Colors.Transparent, AreaStyles.Country2);
        }

        public void DisplayShapeFile(string layerName, string shapeFileName, Color lineColor, AreaStyle areaStyle, bool isVisible = true, float lineWidth = 1f, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true, bool canChangeAreaColor = true)
        {
            var shapeFileLayer = Find<ShapefileMapLayer>(LayerType.Shapefile, layerName) ?? new ShapefileMapLayer
            {
                    LayerType = LayerType.BaseMap,
                    Name = layerName,
                    AreaStyle = areaStyle,
                    CanBeRemoved = canBeRemoved,
                    CanBeReordered = canBeReordered,
                    CanChangeAreaColor = canChangeAreaColor,
                    CanChangeLineColor = canChangeLineColor,
                    ShapefileName = shapeFileName,
            };
            if (lineColor != Colors.Transparent) shapeFileLayer.LineColor = lineColor;
            shapeFileLayer.IsChecked = isVisible;
            Add(shapeFileLayer);
        }

        public void DisplayOverlayFile(string layerName, string overlayFileName, bool isVisible = true, float lineWidth = 1f, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true)
        {
            DisplayOverlayFile(layerName, overlayFileName, Colors.Transparent, isVisible, lineWidth, canBeRemoved, canBeReordered, canChangeLineColor);
        }

        public void DisplayOverlayFile(string layerName, string overlayFileName, Color lineColor, bool isVisible = true, float lineWidth = 1f, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true)
        {
            var overlayShapeLayer = Find<OverlayShapeMapLayer>(LayerType.OverlayFile, layerName) ?? new OverlayShapeMapLayer
            {
                LayerType = LayerType.OverlayFile,
                Name = layerName,
                CanBeRemoved = canBeRemoved,
                CanBeReordered = canBeReordered,
                CanChangeAreaColor = false,
                CanChangeLineColor = canChangeLineColor,
                LineWidth = lineWidth,
            };
            if (lineColor != Colors.Transparent) overlayShapeLayer.LineColor = lineColor;
            overlayShapeLayer.Clear();
            var opAreaOverlay = new OverlayFile(overlayFileName);
            foreach (var shape in opAreaOverlay.Shapes)
                overlayShapeLayer.Add(shape);
            overlayShapeLayer.Done();
            overlayShapeLayer.IsChecked = isVisible;
            Add(overlayShapeLayer);
        }
    }
}
