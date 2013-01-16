using System;
using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;

namespace HRC.Plotting
{
    public interface ISeries
    {
        DataAxisViewModel XAxis { get; set; }
        DataAxisViewModel YAxis { get; set; }
        Range XRange { get; }
        Range YRange { get; }
        /// <summary>
        /// Converts an item in the series to a Point.  X and Y should be whatever natural values should be plotted for that point
        /// </summary>
        Func<object, Point> ItemToPoint { get; }
        /// <summary>
        /// ImageSource for the sample image for this series, used in the legend
        /// </summary>
        ImageSource SampleImageSource { get; }
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