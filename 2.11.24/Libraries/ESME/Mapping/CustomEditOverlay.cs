using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESME.Mapping
{
    public class OldCustomEditInteractiveOverlay : EditInteractiveOverlay
    {
        protected override IEnumerable<Feature> CalculateResizeControlPointsCore(Feature feature)
        {
            // Override the base method and modify the control points for resizing if the shape is the "custom"
            var resizeControlPoints = new Collection<Feature>();
            var polygonShape = feature.GetShape() as PolygonShape;
            if (polygonShape != null) foreach (var vertex in polygonShape.OuterRing.Vertices) resizeControlPoints.Add(new Feature(vertex, feature.Id));
            return resizeControlPoints;
        }
        protected override Feature ResizeFeatureCore(Feature sourceFeature, PointShape sourceControlPoint, PointShape targetControlPoint)
        {
            // Override the base method and modify the logic for resizing if the shape is the "custom"
            var polygonShape = sourceFeature.GetShape() as PolygonShape;
            if (polygonShape != null)
            {
                // If the rectangle is horizontal or vertical, it will use the custom method.
                if (string.Equals(polygonShape.GetBoundingBox().GetWellKnownText(), polygonShape.GetWellKnownText()))
                {
                    var fixedPointIndex = GetFixedPointIndex(polygonShape, sourceControlPoint);
                    var fixedPointShape = new PointShape(polygonShape.OuterRing.Vertices[fixedPointIndex]);
                    var newRectangleShape = new LineShape(new[] { new Vertex(fixedPointShape), new Vertex(targetControlPoint) }).GetBoundingBox();
                    return base.ResizeFeatureCore(new Feature(newRectangleShape.GetWellKnownBinary(), sourceFeature.Id, sourceFeature.ColumnValues),
                                                  targetControlPoint,
                                                  targetControlPoint);
                }
            }
            return base.ResizeFeatureCore(sourceFeature, sourceControlPoint, targetControlPoint);
        }

        private static int GetFixedPointIndex(PolygonShape sourcePolygonShape, PointShape sourceControlPointShape)
        {
            var index = 0;
            for (var i = 0; i < sourcePolygonShape.OuterRing.Vertices.Count; i++)
            {
                var vertex = sourcePolygonShape.OuterRing.Vertices[i];
                if (Math.Abs(vertex.X - sourceControlPointShape.X) > 10E-6 || Math.Abs(vertex.Y - sourceControlPointShape.Y) > 10E-6) continue;
                index = i;
                break;
            }
            int fixedPointIndex;
            if (index <= 2) fixedPointIndex = index + 2;
            else fixedPointIndex = index - 2;
            return fixedPointIndex;
        }
    }
}
