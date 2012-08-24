using System;
using System.Collections;
using System.Collections.ObjectModel;
using System.Windows.Media;
using System.Windows.Shapes;
using ESME.Views.Controls;
using Brush = System.Windows.Media.Brush;
using Point = System.Windows.Point;

namespace DavesWPFTester
{
    public interface ISeries
    {
        /// <summary>
        /// The data for the series
        /// </summary>
        ICollection SeriesData { get; }
        /// <summary>
        /// Converts an item in the series to a Point.  X and Y should be whatever natural values should be plotted for that point
        /// </summary>
        Func<object, Point> ItemToPoint { get; }
        /// <summary>
        /// An action that adds a Point to a StreamGeometryContext using a given size
        /// </summary>
        Action<StreamGeometryContext, Point, double> MarkerType { get; }

        ImageSource SampleImageSource { get; }
        /// <summary>
        /// Thickness of the stroke used to draw the outline of a marker
        /// </summary>
        double MarkerStrokeThickness { get; }
        /// <summary>
        /// Size of the marker
        /// </summary>
        double MarkerSize { get; }
        /// <summary>
        /// Brush used to stroke the outline of the marker
        /// </summary>
        Brush MarkerStroke { get; }
        /// <summary>
        /// Brush used to fill the marker
        /// </summary>
        Brush MarkerFill { get; }
        /// <summary>
        /// Thickness of the line between series points
        /// </summary>
        double LineStrokeThickness { get; }
        /// <summary>
        /// Brush used to stroke the line between series points.  If null, no line will be drawn
        /// </summary>
        Brush LineStroke { get; }
        /// <summary>
        /// Each Double in the collection specifies the length of a dash or gap relative to the 
        /// Thickness of the pen. For example, a value of 1 creates a dash or gap that has the 
        /// same length as the thickness of the pen (a square).
        /// The first item in the collection, which is located at index 0, specifies the length 
        /// of a dash; the second item, which is located at index 1, specifies the length of a gap
        /// Objects with an even index value specify dashes; objects with an odd index value specify gaps.
        /// </summary>
        DoubleCollection LineStrokeDashArray { get; }
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