using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data.Linq;
using System.Linq;
using System.Windows.Media;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    public class MapLayerCollection : ObservableCollection<MapLayerViewModel>
    {
        internal MapLayerCollection(string collectionName)
        {
            Name = collectionName;
            CurrentExtent = new RectangleShape(-180, 90, 180, -90);
        }

        internal MapLayerCollection(string collectionName, string baseMapShapefileName, string baseMapLayerName = "Base Map") : this(collectionName)
        {
            base.Add(new ShapefileMapLayer
            {
                LayerType = LayerType.BaseMap,
                AreaStyle = AreaStyles.Country2,
                CanBeRemoved = false,
                CanBeReordered = true,
                CanChangeAreaColor = true,
                CanChangeLineColor = true,
                ShapefileName = baseMapShapefileName,
                Name = baseMapLayerName,
                MapLayers = this,
            });
        }

        public T Find<T>(LayerType layerType, string layerName) where T : MapLayerViewModel
        {
            if (Count == 0) return null;
            return this.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault() as T;
        }

        public string Name { get; private set; }

        public new void Add(MapLayerViewModel item)
        {
            if (item.MapLayers == null) item.MapLayers = this;
            if (IndexOf(item) != -1) return;
            base.Add(item);
        }

        public ShapefileMapLayer DisplayShapeFile(string layerName, string shapeFileName)
        {
            return DisplayShapeFile(layerName, shapeFileName, Colors.Transparent, AreaStyles.Country2);
        }

        public ShapefileMapLayer DisplayShapeFile(string layerName, string shapeFileName, Color lineColor, AreaStyle areaStyle, bool isVisible = true, float lineWidth = 0f, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true, bool canChangeAreaColor = true)
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
                MapLayers = this,
            };
            if (lineColor != Colors.Transparent) shapeFileLayer.LineColor = lineColor;
            shapeFileLayer.IsChecked = isVisible;
            Add(shapeFileLayer);
            return shapeFileLayer;
        }

        public OverlayShapeMapLayer DisplayOverlayFile(string layerName, string overlayFileName, bool isVisible = true, float lineWidth = 0f, PointSymbolType pointSymbolType = PointSymbolType.Circle, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true)
        {
            return DisplayOverlayFile(layerName, overlayFileName, Colors.Transparent, isVisible, lineWidth, pointSymbolType, canBeRemoved, canBeReordered, canChangeLineColor);
        }

        public OverlayShapeMapLayer DisplayOverlayFile(string layerName, string overlayFileName, Color lineColor, bool isVisible = true, float lineWidth = 0f, PointSymbolType pointSymbolType = PointSymbolType.Circle, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true)
        {
            return DisplayOverlayShapes(layerName, LayerType.OverlayFile, lineColor, new OverlayFile(overlayFileName).Shapes, lineWidth, pointSymbolType, isVisible, null, canBeRemoved, canBeReordered, canChangeLineColor);
        }

        public OverlayShapeMapLayer DisplayOverlayShapes(string layerName, LayerType layerType, Color lineColor, IEnumerable<OverlayShape> overlayShapes, float lineWidth = 0f, PointSymbolType pointSymbolType = PointSymbolType.Circle, bool isVisible = true, ThinkGeo.MapSuite.Core.LineStyle customLineStyle = null, bool canBeRemoved = true, bool canBeReordered = true, bool canChangeLineColor = true)
        {
            var overlayShapeLayer = Find<OverlayShapeMapLayer>(layerType, layerName) ?? new OverlayShapeMapLayer
            {
                LayerType = layerType,
                Name = layerName,
                CanBeRemoved = canBeRemoved,
                CanBeReordered = canBeReordered,
                CanChangeAreaColor = false,
                CanChangeLineColor = canChangeLineColor,
                MapLayers = this,
            };
            if (lineColor != Colors.Transparent) overlayShapeLayer.LineColor = lineColor;
            if (lineWidth != 0f) overlayShapeLayer.LineWidth = lineWidth;
            if (customLineStyle == null) overlayShapeLayer.PointSymbolType = pointSymbolType;
            else overlayShapeLayer.CustomLineStyle = customLineStyle;
            overlayShapeLayer.Clear();
            overlayShapeLayer.Add(overlayShapes);
            overlayShapeLayer.Done();
            overlayShapeLayer.IsChecked = isVisible;
            Add(overlayShapeLayer);
            return overlayShapeLayer;
        }

        public RasterMapLayer DisplayBathymetryRaster(string layerName, string bitmapFileName, bool isVisible = true, bool canBeRemoved = true, bool canBeReordered = true, GeoRect bounds = null)
        {
            var bitmapLayer = Find<RasterMapLayer>(LayerType.BathymetryRaster, layerName) ?? new RasterMapLayer
            {
                Name = layerName,
                CanBeReordered = canBeReordered,
                CanChangeLineColor = false,
                CanChangeLineWidth = false,
                CanBeRemoved = canBeRemoved,
                LayerType = LayerType.BathymetryRaster,
            };
            if (bounds != null)
            {
                bitmapLayer.North = (float)bounds.North;
                bitmapLayer.South = (float)bounds.South;
                bitmapLayer.East = (float)bounds.East;
                bitmapLayer.West = (float)bounds.West;
            }
            bitmapLayer.RasterFilename = bitmapFileName;
            bitmapLayer.IsChecked = isVisible;
            Add(bitmapLayer);
            return bitmapLayer;
        }

        public void DisplayAnalysisPoint(AnalysisPoint curPoint)
        {
            var analysisPointName = string.Format("Analysis Point: [{0:0.###}, {1:0.###}]", curPoint.Latitude, curPoint.Longitude);
            var analysisPointLayer = Find<OverlayShapeMapLayer>(LayerType.AnalysisPoint, analysisPointName);
            if (analysisPointLayer == null)
            {
                analysisPointLayer = new OverlayShapeMapLayer
                {
                    Name = analysisPointName,
                    LayerType = LayerType.AnalysisPoint,
                    LineWidth = 1,
                    CanBeRemoved = true,
                    CanBeReordered = true,
                    HasSettings = true,
                    CanChangeLineColor = true,
                    CanChangeLineWidth = true,
                    CanChangeAreaColor = false,
                };
                Add(analysisPointLayer);
            }

            analysisPointLayer.AnalysisPoint = curPoint;
            analysisPointLayer.Validate();

            analysisPointLayer.Clear();
            foreach (var soundSource in curPoint.SoundSources)
            {
                var sourcePoints = new List<EarthCoordinate>();
                var circlePoints = new List<EarthCoordinate>();
                if (!soundSource.ShouldBeCalculated) continue;
                sourcePoints.Add(curPoint);
                foreach (var radialBearing in soundSource.RadialBearings)
                {
                    sourcePoints.Add(EarthCoordinate.Move(curPoint, radialBearing, soundSource.Radius));
                    sourcePoints.Add(curPoint);
                }

                for (var angle = 0; angle <= 360; angle++)
                    circlePoints.Add(EarthCoordinate.Move(curPoint, angle, soundSource.Radius));

                analysisPointLayer.Add(new OverlayLineSegments(sourcePoints.ToArray(), Colors.Red, 5));
                analysisPointLayer.Add(new OverlayLineSegments(circlePoints.ToArray(), Colors.Red, 5));
            }
            analysisPointLayer.Done();
        }

        public void RemovePropagationPoint(CASSOutput curPoint)
        {
            var layerName = string.Format("Prop {0}|{1}|{2}: [{3:0.###}, {4:0.###}]", curPoint.PlatformName, curPoint.SourceName, curPoint.ModeName, curPoint.Latitude, curPoint.Longitude);
            var propagationPointLayer = Find<OverlayShapeMapLayer>(LayerType.Propagation, layerName);
            if (propagationPointLayer != null) Remove(propagationPointLayer);
        }

        public void DisplayPropagationPoint(CASSOutput curPoint)
        {
            var layerName = string.Format("Prop {0}|{1}|{2}: [{3:0.###}, {4:0.###}]", curPoint.PlatformName, curPoint.SourceName, curPoint.ModeName, curPoint.Latitude, curPoint.Longitude);
            var propagationPointLayer = Find<OverlayShapeMapLayer>(LayerType.Propagation, layerName);
            if (propagationPointLayer == null)
            {
                propagationPointLayer = new OverlayShapeMapLayer
                {
                    Name = layerName,
                    LayerType = LayerType.Propagation,
                    LineWidth = 4,
                    CanBeRemoved = false,
                    CanBeReordered = true,
                    HasSettings = true,
                    CanChangeLineColor = true,
                    CanChangeLineWidth = true,
                    CanChangeAreaColor = false,
                    IsChecked = false,
                };
                Add(propagationPointLayer);
            }

            propagationPointLayer.CASSOutput = curPoint;
            propagationPointLayer.Validate();

            propagationPointLayer.Clear();
            var displayPoints = new List<EarthCoordinate>();
            var circlePoints = new List<EarthCoordinate>();
            var radialCount = curPoint.RadialBearings.Count();
            for (var radialIndex = 0; radialIndex < radialCount; radialIndex++)
            {
                displayPoints.Clear();
#if false
                displayPoints.Add(curPoint);
                const float wiggleSize = 5;
                var wiggleOffset = -wiggleSize;
                var wiggleDirection = 1;
                const int wiggleSteps = 8;
                for (var wiggleIndex = 0; wiggleIndex < wiggleSteps; wiggleIndex++)
                {
                    displayPoints.Add(EarthCoordinate.Move(curPoint, curPoint.RadialBearings[radialIndex] + wiggleOffset, (curPoint.MaxRangeDistance / wiggleSteps) * (wiggleIndex + 1)));
                    wiggleOffset += wiggleSize * wiggleDirection;
                    if (Math.Abs(wiggleOffset) == wiggleSize) wiggleDirection *= -1;
                }
#endif
                var curRadialBearing = curPoint.RadialBearings[radialIndex];
                // Line from center to radius
                displayPoints.Add(curPoint);
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
#if false
                // arrow halfway along radial
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance * .5));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing + 10, curPoint.MaxRangeDistance * .4));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance * .5));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing - 10, curPoint.MaxRangeDistance * .4));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
#endif
                // arrow at end of radial
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing + 5, curPoint.MaxRangeDistance * .9));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing - 5, curPoint.MaxRangeDistance * .9));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
            }

            if (!float.IsNaN(curPoint.ThresholdRadius))
            {
                for (var angle = 0; angle <= 360; angle++) circlePoints.Add(EarthCoordinate.Move(curPoint, angle, curPoint.ThresholdRadius));
                propagationPointLayer.Add(new OverlayLineSegments(circlePoints.ToArray(), Colors.Red, 5));
            }
            propagationPointLayer.Done();
        }


        public RectangleShape CurrentExtent { get; set; }

        public void Remove(string layerName)
        {
            foreach (var layer in this.Where(layer => layer.Name == layerName)) 
            {
                Remove(layer);
                break;
            }
        }
    }

    public class MapLayerCollections : List<MapLayerCollection>
    {
        public MapLayerCollection this[string collectionName] { get { return Find(collection => collection.Name == collectionName); } }

        public void Add(string collectionName)
        {
            Add(new MapLayerCollection(collectionName));
        }

        public void Add(string collectionName, string baseMapShapefileName, string baseMapLayerName = "Base Map")
        {
            Add(new MapLayerCollection(collectionName, baseMapShapefileName, baseMapLayerName));
        }

        public new void Add(MapLayerCollection mapLayerCollection)
        {
            var result = Find(collection => collection.Name == mapLayerCollection.Name);
            if (result != null) throw new DuplicateKeyException("MapLayerCollections: \"" + mapLayerCollection.Name + "\" already exists");
            base.Add(mapLayerCollection);
        }

        public new bool Remove(MapLayerCollection collectionToRemove)
        {
            if (collectionToRemove == ActiveLayer) throw new InvalidOperationException("MapLayerCollections: Cannot remove the active layer");
            return base.Remove(collectionToRemove);
        }

        public MapLayerCollection ActiveLayer
        {
            get { return _activeLayer; }
            set
            {
                if (_activeLayer == value) return;
                _activeLayer = value;
                MediatorMessage.Send(MediatorMessage.SetMapLayers, _activeLayer);
                MediatorMessage.Send(MediatorMessage.SetCurrentExtent, _activeLayer.CurrentExtent);
            }
        }

        MapLayerCollection _activeLayer;
    }
}
