using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Windows.Media;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Layers
{
    class CustomStartEndLineStyle : LineStyle
    {
        readonly PointStyle _startPointStyle;
        readonly PointStyle _endPointStyle;
        readonly LineStyle _lineStyle;

        public CustomStartEndLineStyle(PointSymbolType startType, Color startColor, int startSize,
                                       PointSymbolType endType, Color endColor, int endSize,
                                       Color lineColor, int lineSize)
            : this(new PointStyle(startType, new GeoSolidBrush(GeoColor.FromArgb(startColor.A, startColor.R, startColor.G, startColor.B)), startSize),
                   new PointStyle(endType, new GeoSolidBrush(GeoColor.FromArgb(endColor.A, endColor.R, endColor.G, endColor.B)), endSize),
                   new LineStyle(new GeoPen(GeoColor.FromArgb(lineColor.A, lineColor.R, lineColor.G, lineColor.B), lineSize)))
        { }

        public CustomStartEndLineStyle(PointStyle startPointStyle, PointStyle endPointStyle, LineStyle lineStyle)
        {
            _startPointStyle = startPointStyle;
            _endPointStyle = endPointStyle;
            _lineStyle = lineStyle;
        }


        protected override void DrawCore(IEnumerable<Feature> features, GeoCanvas canvas, Collection<SimpleCandidate> labelsInThisLayer, Collection<SimpleCandidate> labelsInAllLayers)
        {
            var startPointShapes = new Collection<BaseShape>();
            var endPointShapes = new Collection<BaseShape>();
            var lineShapes = new Collection<BaseShape>();

            //Loops thru the features to display the first and end point of each LineShape of the MultilineShape.
            foreach (var feature in features)
            {
                var shape = feature.GetShape();
                lineShapes.Add(shape);
                if (shape is MultilineShape)
                {
                    var multilineShape = (MultilineShape)shape;
                    for (var i = 0; i <= multilineShape.Lines.Count - 1; i++)
                    {
                        var lineShape = multilineShape.Lines[i];
                        startPointShapes.Add(new PointShape(lineShape.Vertices[0]));
                        endPointShapes.Add(new PointShape(lineShape.Vertices[lineShape.Vertices.Count - 1]));
                    }
                }
                else if (shape is LineShape)
                {
                    var lineShape = (LineShape) shape;
                    startPointShapes.Add(new PointShape(lineShape.Vertices[0]));
                    endPointShapes.Add(new PointShape(lineShape.Vertices[lineShape.Vertices.Count - 1]));
                }
            }

            _lineStyle.Draw(lineShapes, canvas, labelsInThisLayer, labelsInAllLayers);
            _startPointStyle.Draw(startPointShapes, canvas, labelsInThisLayer, labelsInAllLayers);
            _endPointStyle.Draw(endPointShapes, canvas, labelsInThisLayer, labelsInAllLayers);
        }
    }
}