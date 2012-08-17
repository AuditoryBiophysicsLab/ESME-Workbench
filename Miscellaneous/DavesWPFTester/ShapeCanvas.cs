using System.Collections.Specialized;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Shapes;
using HRC.Utility;

namespace DavesWPFTester
{
    public class ShapeCanvas : Canvas
    {
        static ShapeCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(ShapeCanvas), new FrameworkPropertyMetadata(typeof(ShapeCanvas))); }

        #region dependency property ObservableList<Shape> Shapes

        public static DependencyProperty ShapesProperty = DependencyProperty.Register("Shapes",
                                                                                 typeof(ObservableList<Shape>),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, ShapesPropertyChanged));

        public ObservableList<Shape> Shapes { get { return (ObservableList<Shape>)GetValue(ShapesProperty); } set { SetValue(ShapesProperty, value); } }

        static void ShapesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).ShapesPropertyChanged(args); }
        void ShapesPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((ObservableList<Shape>)args.OldValue).CollectionChanged -= ShapesCollectionChanged;
            if (args.NewValue != null) ((ObservableList<Shape>)args.NewValue).CollectionChanged += ShapesCollectionChanged;
        }

        void ShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs notifyCollectionChangedEventArgs)
        {
            Children.Clear();
            if (Shapes != null) foreach (var shape in Shapes) Children.Add(shape);
            InvalidateVisual();
        }
        #endregion
    }
}
