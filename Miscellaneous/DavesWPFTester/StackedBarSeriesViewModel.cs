using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Shapes;
using HRC.Utility;

namespace DavesWPFTester
{
    public class StackedBarSeriesViewModel : MultipleBarSeriesBase
    {
        protected override void RenderSample() { }
        protected override void AddPoint(Point newPoint) { }
        protected override void RemovePoint(Point oldPoint) { }
        internal override Shape RectToShape(Rect rect) { throw new NotImplementedException(); }
    
        public override void RenderShapes()
        {
            if (BarSeriesCollection == null || BarSeriesCollection.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            var xCoordinates = new List<double>();
            foreach (var series in BarSeriesCollection)
            {
                if (!SeriesPlotPointCache.ContainsKey(series)) SeriesPlotPointCache.Add(series, new Dictionary<double, Tuple<Point, double>>());
                SeriesPlotPointCache[series].Clear();
                foreach (var point in series.Points)
                {
                    var plotPoint = new Point(XAxis.ValueToPosition(Math.Round(point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y));
                    xCoordinates.Add(plotPoint.X);
                    SeriesPlotPointCache[series][plotPoint.X] = Tuple.Create(point, plotPoint.Y);
                }
                XRange.Add(series.XRange);
            }
            var xPlotCoordinates = xCoordinates.Distinct().ToList();
            foreach (var x in xPlotCoordinates)
                YRange.Add((from series in BarSeriesCollection
                            where SeriesPlotPointCache.ContainsKey(series)
                            select SeriesPlotPointCache[series][x].Item1.Y).Sum());
            MinimumXPlotSpacing = xPlotCoordinates.AdjacentDifferences().Min();
            var width = MinimumXPlotSpacing * BarWidth;
            PlotOriginY = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            Shapes.Clear();
            //Debug.WriteLine("");
            //Debug.WriteLine("Re-rendering StackedBarSeries");
            foreach (var plotX in xPlotCoordinates)
            {
                var lastDataY = Math.Max(YAxis.VisibleRange.Min, 0);
                //Debug.WriteLine(string.Format("Rendering stacked bars at plotX = {0}", plotX));
                foreach (var series in BarSeriesCollection)
                {
                    if (!SeriesPlotPointCache[series].ContainsKey(plotX)) continue;
                    // This series contains a Y value for the current X, turn it into a rect
                    var value = SeriesPlotPointCache[series][plotX];
                    var curDataY = value.Item1.Y;
                    var curPlotY = YAxis.ValueToPosition(curDataY);
                    var height = Math.Abs(PlotOriginY - curPlotY);
                    if (Math.Abs(height - 0) < double.Epsilon) continue;
                    var lastPlotY = YAxis.ValueToPosition(lastDataY);
                    var topShift = PlotOriginY - lastPlotY;
                    var rect = new Rect(plotX - width / 2, curPlotY - topShift, width, height);
                    //Debug.WriteLine(string.Format("Created rect for series {0}. Data: x = {1}, y = {2}  Rect: left = {3}, top = {4}, width = {5}, height = {6}, right = {7}, bottom = {8}", series.SeriesName, value.Item1.X, value.Item1.Y, rect.Left, rect.Top, rect.Width, rect.Height, rect.Right, rect.Bottom));
                    Shapes.Add(series.RectToShape(rect));
                    lastDataY += curDataY;
                }
            }
        }
    }

    public class GroupedBarSeriesViewModel : MultipleBarSeriesBase
    {
        protected override void RenderSample() { }
        protected override void AddPoint(Point newPoint) { }
        protected override void RemovePoint(Point oldPoint) { }
        internal override Shape RectToShape(Rect rect) { throw new NotImplementedException(); }

        public override void RenderShapes()
        {
            if (BarSeriesCollection == null || BarSeriesCollection.Count == 0 || XAxis == null || XAxis.ValueToPosition == null || YAxis == null || YAxis.ValueToPosition == null) return;
            var xCoordinates = new List<double>();
            foreach (var series in BarSeriesCollection)
            {
                if (!SeriesPlotPointCache.ContainsKey(series)) SeriesPlotPointCache.Add(series, new Dictionary<double, Tuple<Point, double>>());
                SeriesPlotPointCache[series].Clear();
                foreach (var point in series.Points)
                {
                    var plotPoint = new Point(XAxis.ValueToPosition(Math.Round(point.X, XRoundingPrecision)), YAxis.ValueToPosition(point.Y));
                    xCoordinates.Add(plotPoint.X);
                    SeriesPlotPointCache[series][plotPoint.X] = Tuple.Create(point, plotPoint.Y);
                }
                XRange.Add(series.XRange);
                YRange.Add(series.YRange);
            }
            var xPlotCoordinates = xCoordinates.Distinct().ToList();
            MinimumXPlotSpacing = xPlotCoordinates.AdjacentDifferences().Min();
            var groupWidth = MinimumXPlotSpacing * BarWidth;
            var seriesWidth = groupWidth / BarSeriesCollection.Count;
            PlotOriginY = YAxis.ValueToPosition(Math.Max(YAxis.VisibleRange.Min, 0));
            Shapes.Clear();
            //Debug.WriteLine("");
            //Debug.WriteLine("Re-rendering StackedBarSeries");
            foreach (var plotX in xPlotCoordinates)
            {
                var groupPlotX = plotX - groupWidth / 2;
                foreach (var series in BarSeriesCollection)
                {
                    if (!SeriesPlotPointCache[series].ContainsKey(plotX)) continue;
                    // This series contains a Y value for the current X, turn it into a rect
                    var value = SeriesPlotPointCache[series][plotX];
                    var curDataY = value.Item1.Y;
                    var curPlotY = YAxis.ValueToPosition(curDataY);
                    var height = Math.Abs(PlotOriginY - curPlotY);
                    if (Math.Abs(height - 0) < double.Epsilon) continue;
                    var rect = new Rect(groupPlotX, curPlotY, seriesWidth, height);
                    //Debug.WriteLine(string.Format("Created rect for series {0}. Data: x = {1}, y = {2}  Rect: left = {3}, top = {4}, width = {5}, height = {6}, right = {7}, bottom = {8}", series.SeriesName, value.Item1.X, value.Item1.Y, rect.Left, rect.Top, rect.Width, rect.Height, rect.Right, rect.Bottom));
                    Shapes.Add(series.RectToShape(rect));
                    groupPlotX += seriesWidth;
                }
            }
        }
    }
}