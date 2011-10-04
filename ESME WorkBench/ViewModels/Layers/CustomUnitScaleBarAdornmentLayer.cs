using System;
using System.Collections.ObjectModel;
using ThinkGeo.MapSuite.Core;

namespace OneNavyModel.ViewModels.Layers
{
    internal class CustomUnitScaleBarAdornmentLayer : AdornmentLayer
    {
        int _width = 200;
        string _unitText;
        double _meterToUnit;
        double _unitRoundValue;

        //Maximum width of the scale bar in screen coordinate
        public int MaxWidth
        {
            get { return _width; }
            set { _width = value; }
        }

        //Text to be displayed for the unit.
        public string UnitText
        {
            get { return _unitText; }
            set { _unitText = value; }
        }

        //Ratio of meter to the unit.
        public double MeterToUnit
        {
            get { return _meterToUnit; }
            set { _meterToUnit = value; }
        }

        public GeoFont GeoFont { get; set; }

        public GeoSolidBrush GeoSolidBrush { get; set; }


        protected override void DrawCore(GeoCanvas canvas, Collection<SimpleCandidate> labelsInAllLayers)
        {
            //Set the position of the scale bar on canvas.
            const float xpos = 15;
            var ypos = canvas.Height - 15;

            //Gets the left and right location of the scale bar in world coordinate according to the maximum width and X and Y position.
            var screenLocation = GetDrawingLocation(canvas, _width, ypos);
            var scaleBarMapPointR = ExtentHelper.ToWorldCoordinate(canvas.CurrentWorldExtent, screenLocation.X, screenLocation.Y, canvas.Width, canvas.Height);
            var scaleBarMapRightPointR = ExtentHelper.ToWorldCoordinate(canvas.CurrentWorldExtent, screenLocation.X + _width, screenLocation.Y, canvas.Width, canvas.Height);
            if ((Math.Abs(scaleBarMapPointR.X) > 180.0) || (Math.Abs(scaleBarMapRightPointR.X) > 180.0)) return;
            if ((Math.Abs(scaleBarMapPointR.Y) > 90.0) || (Math.Abs(scaleBarMapRightPointR.Y) > 90.0)) return;
            try
            {
                //Gets the length of the scale bar according to the unit and the maximum width of the scale bar.
                var fullBarLength = scaleBarMapPointR.GetDistanceTo(scaleBarMapRightPointR, canvas.MapUnit, DistanceUnit.Meter);
                //Adjusts the length of the scale bar in order to have a round number.
                _unitRoundValue = GetRoundValue(fullBarLength/_meterToUnit);
                var barLength = ((_unitRoundValue*_meterToUnit)*_width)/fullBarLength;

                //Draw the line of the scale bar according to the adjusted length.
                var pen = new GeoPen(GeoColor.StandardColors.White, 1F);
                canvas.DrawLine(new[]
                                {
                                    new ScreenPointF(xpos, ypos - 10), new ScreenPointF(xpos, ypos), new ScreenPointF((float) barLength + xpos, ypos), new ScreenPointF((float) barLength + xpos, ypos - 10)
                                }, pen, DrawingLevel.LevelOne, 0, 0);

                //Displays the text for the value and unit text.
                canvas.DrawText(Convert.ToString(_unitRoundValue) + " " + _unitText, GeoFont, GeoSolidBrush, new[]
                                                                                                             {
                                                                                                                 new ScreenPointF((float) (barLength/2) + xpos, ypos - 10)
                                                                                                             }, DrawingLevel.LevelOne);
            }
            catch (Exception) {}
        }

        //Function to round down the length to fit within the maximum width of the scale bar.
        static double GetRoundValue(double unitValue)
        {
            var interval = GetRoundingValue(unitValue);
            var result = FloorNumber(unitValue, interval);
            return result;
        }

        //Gets the rounding value to be used according to the length of the unit value.
        static double GetRoundingValue(double unitValue)
        {
            double result = 0;
            if (unitValue > 100000)
            {
                result = 50000;
            }
            else if (unitValue > 10000)
            {
                result = 5000;
            }
            else if (unitValue > 1000)
            {
                result = 500;
            }
            else if (unitValue > 100)
            {
                result = 50;
            }
            else if (unitValue > 10)
            {
                result = 5;
            }
            else if (unitValue > 1)
            {
                result = 0.5;
            }
            else if (unitValue > 0.1)
            {
                result = 0.05;
            }
            else if (unitValue > 0.01)
            {
                result = 0.005;
            }

            return result;
        }

        //Function used for rounding down a number according to the rounding value.
        static double FloorNumber(double number, double rounding)
        {
            double result;
            var ieeeRemainder = Math.IEEERemainder(number, rounding);
            if (ieeeRemainder > 0) result = number - ieeeRemainder;
            else if (ieeeRemainder < 0) result = (number + Math.Abs(ieeeRemainder)) - rounding;
            else result = number;
            return result;
        }
    }
}