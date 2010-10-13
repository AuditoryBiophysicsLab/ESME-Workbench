using System.Collections.Generic;
using System.Collections.ObjectModel;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Map
{
    // Ideally I would want to make this inherit from the FeatureLayer
    // so you could do spatial queries.  In the interests of time I 
    // inherited from the Layer to make things simple and show the point
    // of how easy it is to extend Map Suite.  When we roll this into
    // the main product we may create a FeatureSource and FeatureLayer.

    internal class MapShapeLayer : Layer
    {
        readonly Dictionary<string, MapShape> _mapShapes;

        public MapShapeLayer() { _mapShapes = new Dictionary<string, MapShape>(); }

        // Here is where you place all of your map shapes.
        public Dictionary<string, MapShape> MapShapes
        {
            get { return _mapShapes; }
        }

        // This is a required overload of the Layer.  As you can see we simply
        // loop through all of our map shapes and then choose the correct zoom level.
        // After that, the zoom level class takes care of the heavy lifiting.  You
        // have to love how easy this framework is to re-use.

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            foreach (var mapShapeKey in _mapShapes.Keys)
            {
                var mapShape = _mapShapes[mapShapeKey];
                var currentZoomLevel = mapShape.ZoomLevels.GetZoomLevelForDrawing(canvas.CurrentWorldExtent, canvas.Width, GeographyUnit.DecimalDegree);
                if (currentZoomLevel != null)
                {
                    if (canvas.CurrentWorldExtent.Intersects(mapShape.Feature.GetBoundingBox()))
                    {
                        currentZoomLevel.Draw(canvas, new[]
                                                      {
                                                          mapShape.Feature
                                                      }, new Collection<SimpleCandidate>(), labelsInAllLayers);
                    }
                }
            }
        }
    }
}