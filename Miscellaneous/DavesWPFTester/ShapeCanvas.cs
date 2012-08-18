using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Shapes;

namespace DavesWPFTester
{
    public class ShapeCanvas : Canvas
    {
        static ShapeCanvas() { DefaultStyleKeyProperty.OverrideMetadata(typeof(ShapeCanvas), new FrameworkPropertyMetadata(typeof(ShapeCanvas))); }

        #region dependency property ObservableCollection<Shape> Shapes

        public static DependencyProperty ShapesProperty = DependencyProperty.Register("Shapes",
                                                                                 typeof(ObservableCollection<Shape>),
                                                                                 typeof(ShapeCanvas),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender, ShapesPropertyChanged));

        public ObservableCollection<Shape> Shapes { get { return (ObservableCollection<Shape>)GetValue(ShapesProperty); } set { SetValue(ShapesProperty, value); } }

        static void ShapesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((ShapeCanvas)obj).ShapesPropertyChanged(args); }
        void ShapesPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null) ((ObservableCollection<Shape>)args.OldValue).CollectionChanged -= ShapesCollectionChanged;
            if (args.NewValue != null) ((ObservableCollection<Shape>)args.NewValue).CollectionChanged += ShapesCollectionChanged;
        }

        void ShapesCollectionChanged(object sender, NotifyCollectionChangedEventArgs args)
        {
            switch (args.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    var addIndex = args.NewStartingIndex;
                    foreach (var shape in args.NewItems) Children.Insert(addIndex++, (UIElement)shape);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    var replaceIndex = args.OldStartingIndex;
                    foreach (var shape in args.NewItems) Children[replaceIndex++] = (UIElement)shape;
                    break;
                case NotifyCollectionChangedAction.Remove:
                    foreach (var shape in args.OldItems) Children.Remove((UIElement)shape);
                    break;
                case NotifyCollectionChangedAction.Move:
                case NotifyCollectionChangedAction.Reset:
                    Children.Clear();
                    if (Shapes != null) foreach (var shape in Shapes) Children.Add(shape);
                    break;
            }
            InvalidateVisual();
        }
        #endregion
    }
}
