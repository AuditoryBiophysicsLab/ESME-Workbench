using System;
using System.Collections.ObjectModel;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.Views.Controls;
using Point = System.Windows.Point;

namespace DavesWPFTester
{
    public interface ISeries
    {
        double MinX { get; }
        double MaxX { get; }
        double MinY { get; }
        double MaxY { get; }
        /// <summary>
        /// Converts an item in the series to a Point.  X and Y should be whatever natural values should be plotted for that point
        /// </summary>
        Func<object, Point> ItemToPoint { get; }
        /// <summary>
        /// ImageSource for the sample image for this series, used in the legend
        /// </summary>
        ImageSource SampleImageSource { get; }
        /// <summary>
        /// The X Axis control to plot the DataPoints against (used for mapping X values to screen coordinates)
        /// </summary>
        DataAxis XAxis { get; set; }
        /// <summary>
        /// The Y Axis control to plot the DataPoints against (used for mapping Y values to screen coordinates)        
        /// </summary>
        DataAxis YAxis { get; set; }
        /// <summary>
        /// The name of the series, used to create a legend
        /// </summary>
        string SeriesName { get; }
        /// <summary>
        /// Render the series.  Updates the Shapes collection
        /// </summary>
        void RenderShapes();
        /// <summary>
        /// The collection of shapes representing this series
        /// </summary>
        ObservableCollection<Shape> Shapes { get; }
    }
}