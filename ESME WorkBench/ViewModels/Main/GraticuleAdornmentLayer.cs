using System;
using System.Collections.ObjectModel;
using System.Linq;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Main
{
    //this class inherits from AdornmentLayer. 
    internal class MyGraticuleAdornmentLayer : AdornmentLayer
    {
        private readonly GeoColor _graticuleColor;
        private readonly int _graticuleDensity;
        private readonly Collection<double> _intervals = new Collection<double>();

        //Structure used for labeling the meridians and parallels at the desired location on the map in screen coordinates.

        public MyGraticuleAdornmentLayer()
        {
            //This property gives the approximate density of lines that the map will have.
            _graticuleDensity = 10;
            //The color of the lines
            _graticuleColor = GeoColor.FromArgb(255, GeoColor.StandardColors.LightBlue);

            //Sets all the intervals in degree to be displayed.
            //The intervals need to be added from the smallest to the largest.
            _intervals.Add(0.0005);
            _intervals.Add(0.001);
            _intervals.Add(0.002);
            _intervals.Add(0.005);
            _intervals.Add(0.01);
            _intervals.Add(0.02);
            _intervals.Add(0.05);
            _intervals.Add(0.1);
            _intervals.Add(0.2);
            _intervals.Add(0.5);
            _intervals.Add(1);
            _intervals.Add(2);
            _intervals.Add(5);
            _intervals.Add(10);
            _intervals.Add(20);
            _intervals.Add(40);
            _intervals.Add(50);
        }

        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            var currentExtent = canvas.CurrentWorldExtent;
            var currentMinX = currentExtent.UpperLeftPoint.X;
            var currentMaxX = currentExtent.UpperRightPoint.X;
            var currentMaxY = currentExtent.UpperLeftPoint.Y;
            var currentMinY = currentExtent.LowerLeftPoint.Y;

            //Gets the increment according to the current extent of the map and the graticule density set 
            //by the GrsaticuleDensity property
            var increment = GetIncrement(currentExtent.Width, _graticuleDensity);

            //Collections of GraticuleLabel for labeling the different lines.
            var meridianGraticuleLabels = new Collection<GraticuleLabel>();
            var parallelGraticuleLabels = new Collection<GraticuleLabel>();

            //Loop for displaying the meridians (lines of common longitude).
            double x;
            for (x = CeilingNumber(currentExtent.UpperLeftPoint.X, increment);
                 x <= currentExtent.UpperRightPoint.X;
                 x += increment)
            {
                var lineShapeMeridian = new LineShape();
                lineShapeMeridian.Vertices.Add(new Vertex(x, currentMaxY));
                lineShapeMeridian.Vertices.Add(new Vertex(x, currentMinY));
                canvas.DrawLine(lineShapeMeridian, new GeoPen(_graticuleColor, 0.5F), DrawingLevel.LevelFour);

                //Gets the label and screen position of each meridian.
                var meridianLabelPosition = ExtentHelper.ToScreenCoordinate(canvas.CurrentWorldExtent, x,
                                                                                     currentMaxY, canvas.Width,
                                                                                     canvas.Height);
                meridianGraticuleLabels.Add(new GraticuleLabel(FormatLatLong(x, LineType.Meridian, increment),
                                                               meridianLabelPosition));
            }

            //Loop for displaying the parallels (lines of common latitude).
            double y;
            for (y = CeilingNumber(currentExtent.LowerLeftPoint.Y, increment);
                 y <= currentExtent.UpperRightPoint.Y;
                 y += increment)
            {
                var lineShapeParallel = new LineShape();
                lineShapeParallel.Vertices.Add(new Vertex(currentMaxX, y));
                lineShapeParallel.Vertices.Add(new Vertex(currentMinX, y));
                canvas.DrawLine(lineShapeParallel, new GeoPen(_graticuleColor, 0.5F), DrawingLevel.LevelFour);

                //Gets the label and screen position of each parallel.
                ScreenPointF parallelLabelPosition = ExtentHelper.ToScreenCoordinate(canvas.CurrentWorldExtent,
                                                                                     currentMinX, y, canvas.Width,
                                                                                     canvas.Height);
                parallelGraticuleLabels.Add(new GraticuleLabel(FormatLatLong(y, LineType.Parallel, increment),
                                                               parallelLabelPosition));
            }


            //Loop for displaying the label for the meridians.
            foreach (var meridianGraticuleLabel in meridianGraticuleLabels)
            {
                var locations = new Collection<ScreenPointF> {new ScreenPointF(meridianGraticuleLabel.Location.X, meridianGraticuleLabel.Location.Y + 6)};

                canvas.DrawText(meridianGraticuleLabel.Label, new GeoFont("Arial", 10),
                                new GeoSolidBrush(GeoColor.StandardColors.Navy),
                                new GeoPen(GeoColor.StandardColors.White, 2), locations, DrawingLevel.LevelFour, 8, 0, 0);
            }

            //Loop for displaying the label for the parallels.
            foreach (var parallelGraticuleLabel in parallelGraticuleLabels)
            {
                var locations = new Collection<ScreenPointF> {new ScreenPointF(parallelGraticuleLabel.Location.X, parallelGraticuleLabel.Location.Y)};

                canvas.DrawText(parallelGraticuleLabel.Label, new GeoFont("Arial", 10),
                                new GeoSolidBrush(GeoColor.StandardColors.Navy),
                                new GeoPen(GeoColor.StandardColors.White, 2), locations, DrawingLevel.LevelFour, 8, 0,
                                90);
            }
        }

        //Formats the decimal degree value into Degree Minute and Seconds according to the increment. It also looks
        //if the longitude is East or West and the latitude North or South.
        private static string FormatLatLong(double value, LineType lineType, double increment)
        {
            string result;
            try
            {
                if (increment >= 1)
                {
                    result = DecimalDegreesHelper.GetDegreesMinutesSecondsStringFromDecimalDegree(Math.Abs(value));
                    result = result.Substring(0, result.Length - 9);
                }
                else if (increment >= 0.1)
                {
                    result = DecimalDegreesHelper.GetDegreesMinutesSecondsStringFromDecimalDegree(Math.Abs(value));
                    result = result.Substring(0, result.Length - 5);
                }
                else if (increment >= 0.01)
                {
                    result = DecimalDegreesHelper.GetDegreesMinutesSecondsStringFromDecimalDegree(Math.Abs(value));
                }
                else
                {
                    result = DecimalDegreesHelper.GetDegreesMinutesSecondsStringFromDecimalDegree(Math.Abs(value), 2);
                }

                if (lineType == LineType.Meridian)
                {
                    if (value > 0) result = result + " E";
                    else if (value < 0) result = result + " W";
                }

                if (lineType == LineType.Parallel)
                {
                    if (value > 0) result = result + " N";
                    else if (value < 0) result = result + " S";
                }
            }
            catch
            {
                result = "N/A";
            }

            return result;
        }

        //Function used for determining the degree value to use according to the interval.
        private static double CeilingNumber(double number, double interval)
        {
            double result;
            var ieeeRemainder = Math.IEEERemainder(number, interval);
            if (ieeeRemainder > 0)
                result = (number - ieeeRemainder) + interval;
            else if (ieeeRemainder < 0)
                result = number + Math.Abs(ieeeRemainder);
            else
                result = number;
            return result;
        }

        //Gets the increment to used according to the with of the current extent and the graticule density.
        private double GetIncrement(double currentExtentWidth, double divisor)
        {
            var rawInterval = currentExtentWidth/divisor;

            var result = _intervals.FirstOrDefault(interval => rawInterval < interval);
            if (result == 0.0) result = _intervals[_intervals.Count - 1];
            return result;
        }

        #region Nested type: GraticuleLabel

        private struct GraticuleLabel
        {
            public readonly string Label;
            public ScreenPointF Location;

            public GraticuleLabel(string label, ScreenPointF location)
            {
                Label = label;
                Location = location;
            }
        }

        #endregion

        #region Nested type: LineType

        private enum LineType
        {
            Meridian,
            Parallel
        } ;

        #endregion
    }
}