﻿using System;
using System.Collections.Generic;
using System.Data.Linq;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows.Media;
using ESME.Animats;
using ESME.NEMO;
using ESME.NEMO.Overlay;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;

namespace ESME.Mapping
{
    public static class MapCollectionExtensions
    {
        public static IEnumerable<T> Find<T>(this IEnumerable<T> source) where T : MapLayerViewModel
        {
            return source.OfType<T>();
        }

        public static IEnumerable<T> Find<T>(this IEnumerable<T> source, LayerType layerType) where T : MapLayerViewModel
        {
            return source.OfType<T>().Where(layer => layer.LayerType == layerType);
        }

        public static IEnumerable<T> Find<T>(this IEnumerable<T> source, string layerName) where T : MapLayerViewModel
        {
            return source.OfType<T>().Where(layer => layer.Name == layerName);
        }

        public static IEnumerable<T> Find<T>(this IEnumerable<T> source, Regex nameRegex) where T : MapLayerViewModel
        {
            return source.OfType<T>().Where(layer => nameRegex.IsMatch(layer.Name));
        }

        public static IEnumerable<T> Find<T>(this IEnumerable<T> source, LayerType layerType, string layerName) where T : MapLayerViewModel
        {
            return source.Find<T>(layerType).Find<T>(layerName);
        }

        public static IEnumerable<T> Find<T>(this IEnumerable<T> source, LayerType layerType, Regex nameRegex) where T : MapLayerViewModel
        {
            return source.Find(layerType).Find(nameRegex);
        }

        public static void AddOrReplace<T>(this List<T> source, LayerType layerType, string layerName, T newLayer) where T : MapLayerViewModel
        {
            var result = source.Find(layerType, layerName).ToList();
            if (result.Count > 1) throw new ApplicationException(string.Format("More than one layer matches type {0} and name \"{1}\", which should be impossible", layerType, layerName));
            if (result.Count == 1) source.Remove(result[0]);
            source.Add(newLayer);
        }

        public static int RemoveLayerType<T>(this List<T> source, LayerType layerType) where T : MapLayerViewModel
        {
            return source.RemoveAll(item => item.LayerType == layerType);
        }
    }

    public class MapLayerCollection : ObservableList<MapLayerViewModel>
    {
        public MapLayerCollection(string baseMapShapefileName, string baseMapLayerName = "Base Map")
        {
            CurrentExtent = new RectangleShape(-180, 90, 180, -90);
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
            MediatorMessage.Send(MediatorMessage.SetMapLayers, this);
            MediatorMessage.Send(MediatorMessage.SetCurrentExtent, CurrentExtent);
        }

        public IEnumerable<MapLayerViewModel> Find(LayerType layerType)
        {
            return this.Where(layer => layer.LayerType == layerType);
        }

        public IEnumerable<MapLayerViewModel> Find(string layerName)
        {
            return this.Where(layer => layer.Name == layerName);
        }

        public T Find<T>(LayerType layerType, string layerName) where T : MapLayerViewModel
        {
            if (Count == 0) return null;
            return this.Where(layer => layer.LayerType == layerType).Where(layer => layer.Name == layerName).FirstOrDefault() as T;
        }

        public T Find<T>(LayerType layerType, Func<MapLayerViewModel, bool> predicate) where T : MapLayerViewModel
        {
            if (Count == 0) return null;
            return this.Where(layer => layer.LayerType == layerType).Where(predicate).FirstOrDefault() as T;
        }

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
            overlayShapeLayer.IsEnabled = true;
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
            bitmapLayer.IsEnabled = true;
            bitmapLayer.RasterFilename = bitmapFileName;
            bitmapLayer.IsChecked = isVisible;
            Add(bitmapLayer);
            return bitmapLayer;
        }

        public void DisplayAnalysisPoint(AnalysisPoint curPoint)
        {
            var oldIndex = -1;
            if (curPoint.OldLocation != null)
            {
                var oldName = string.Format("Analysis Point: [{0:0.###}, {1:0.###}]", curPoint.OldLocation.Latitude, curPoint.OldLocation.Longitude);
                var oldLayer = Find<OverlayShapeMapLayer>(LayerType.AnalysisPoint, oldName);
                oldIndex = IndexOf(oldLayer);
                //if (oldLayer != null) Remove(oldLayer);
                //oldIndex--;
                curPoint.OldLocation = null;
            }
            var analysisPointName = string.Format("Analysis Point: [{0:0.###}, {1:0.###}]", curPoint.Latitude, curPoint.Longitude);
            var analysisPointLayer = Find<AnalysisPointLayer>(LayerType.AnalysisPoint, analysisPointName);
            if (analysisPointLayer == null)
            {
                analysisPointLayer = new AnalysisPointLayer
                {
                    Name = analysisPointName,
                    LineWidth = 1,
                    CanBeRemoved = true,
                    CanBeReordered = true,
                    CanChangeLineColor = true,
                    CanChangeLineWidth = true,
                    CanChangeAreaColor = false,
                };
                if (oldIndex < 0) Add(analysisPointLayer);
                else this[oldIndex] = analysisPointLayer;
            }
            analysisPointLayer.IsEnabled = true;

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
            var propagationPointLayer = Find<PropagationLayer>(LayerType.Propagation, layerName);
            if (propagationPointLayer != null) Remove(propagationPointLayer);
        }

        public void DisplayPropagationPoint(CASSOutput curPoint)
        {
            var layerName = string.Format("Prop {0}|{1}|{2}: [{3:0.###}, {4:0.###}]", curPoint.PlatformName, curPoint.SourceName, curPoint.ModeName, curPoint.Latitude, curPoint.Longitude);
            var propagationPointLayer = Find<PropagationLayer>(LayerType.Propagation, layerName);
            if (propagationPointLayer == null)
            {
                propagationPointLayer = new PropagationLayer
                {
                    Name = layerName,
                    LayerType = LayerType.Propagation,
                    LineWidth = 1,
                    CanBeRemoved = false,
                    CanBeReordered = true,
                    CanChangeLineColor = true,
                    CanChangeLineWidth = true,
                    CanChangeAreaColor = false,
                    IsChecked = false,
                };
                Add(propagationPointLayer);
            }
            propagationPointLayer.IsEnabled = true;
            propagationPointLayer.CASSOutput = curPoint;
            propagationPointLayer.Validate();

            propagationPointLayer.Clear();
            var displayPoints = new List<EarthCoordinate>();
            var circlePoints = new List<EarthCoordinate>();
            var radialCount = curPoint.RadialBearings.Count();
            for (var radialIndex = 0; radialIndex < radialCount; radialIndex++)
            {
                displayPoints.Clear();

                var curRadialBearing = curPoint.RadialBearings[radialIndex];
                // Line from center to radius
                displayPoints.Add(curPoint);
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();

                // arrow at end of radial
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing + 5, curPoint.MaxRangeDistance * .9));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing, curPoint.MaxRangeDistance));
                displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing - 5, curPoint.MaxRangeDistance * .9));
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
            }

            if (curPoint.ThresholdRadii != null)
            {
                for (var radialIndex = 0; radialIndex <= radialCount; radialIndex++)
                {
                    var curRadialBearing = curPoint.RadialBearings[radialIndex % radialCount];
                    displayPoints.Add(EarthCoordinate.Move(curPoint, curRadialBearing,
                                                           curPoint.ThresholdRadii[radialIndex % radialCount]));
                }
                propagationPointLayer.Add(new OverlayLineSegments(displayPoints.ToArray(), Colors.Red, 5));
                displayPoints.Clear();
            }

            if (!float.IsNaN(curPoint.ThresholdRadius))
            {
                // Display circle at maximum threshold radius
                for (var angle = 0; angle <= 360; angle++) circlePoints.Add(EarthCoordinate.Move(curPoint, angle, curPoint.ThresholdRadius));
                propagationPointLayer.Add(new OverlayLineSegments(circlePoints.ToArray(), Colors.Red, 5));
            }
            propagationPointLayer.Done();
        }

        public void DisplaySpecies(string speciesName, AnimatFile animatFile)
        {
            var speciesLayerName = string.Format("Species: {0}", speciesName);
            var speciesLayer = Find<OverlayShapeMapLayer>(LayerType.Animal, speciesLayerName);
            if (speciesLayer == null)
            {
                speciesLayer = new OverlayShapeMapLayer
                {
                    Name = speciesLayerName,
                    LayerType = LayerType.Animal,
                    LineWidth = 1,
                    CanBeRemoved = false,
                    CanBeReordered = true,
                    CanChangeLineColor = true,
                    CanChangeLineWidth = true,
                    CanChangeAreaColor = false,
                    IsChecked = false,
                };
                Add(speciesLayer);
            }
            var startPoints = animatFile.AnimatStartPoints.Select(startPoint => new OverlayPoint(startPoint));
            speciesLayer.ToolTip = String.Format("Layer contains {0} animats", animatFile.TotalAnimats);
            speciesLayer.IsEnabled = true;
            speciesLayer.Clear();
            speciesLayer.Add(startPoints);
            speciesLayer.Done();
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
}
