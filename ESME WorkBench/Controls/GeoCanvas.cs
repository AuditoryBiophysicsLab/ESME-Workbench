using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Threading;
using ESME.GUI.Classes;

namespace ESME.GUI.Controls
{
    /// <summary>
    /// Follow steps 1a or 1b and then 2 to use this custom control in a XAML file.
    ///
    /// Step 1a) Using this custom control in a XAML file that exists in the current project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:ESME.GUI.Controls"
    ///
    ///
    /// Step 1b) Using this custom control in a XAML file that exists in a different project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:ESME.GUI.Controls;assembly=ESMEWorkbench"
    ///
    /// You will also need to add a project reference from the project where the XAML file lives
    /// to this project and Rebuild to avoid compilation errors:
    ///
    ///     Right click on the target project in the Solution Explorer and
    ///     "Add Reference"->"Projects"->[Browse to and select this project]
    ///
    ///
    /// Step 2)
    /// Go ahead and use your control in the XAML file.
    ///
    ///     <MyNamespace:GeoCanvas/>
    ///
    /// </summary>
    public class GeoCanvas : Canvas
    {
        #region Dependency Properties

        #region public double MouseLatitude { get; set; }

        public static readonly DependencyProperty MouseLatitudeProperty = DependencyProperty.Register("MouseLatitude",
            typeof(double), typeof(GeoCanvas), new FrameworkPropertyMetadata(double.NaN));
        public double MouseLatitude
        {
            get { return (double)GetValue(GeoCanvas.MouseLatitudeProperty); }
            set { SetValue(GeoCanvas.MouseLatitudeProperty, value); }
        }
        static void MouseLatitudePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as GeoCanvas).MouseLatitudePropertyChanged(args);
        }
        void MouseLatitudePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateVisual();
        }

        #endregion
        #region public double MouseLongitude { get; set; }

        public static readonly DependencyProperty MouseLongitudeProperty = DependencyProperty.Register("MouseLongitude",
            typeof(double), typeof(GeoCanvas), new FrameworkPropertyMetadata(double.NaN));
        public double MouseLongitude
        {
            get { return (double)GetValue(GeoCanvas.MouseLongitudeProperty); }
            set { SetValue(GeoCanvas.MouseLongitudeProperty, value); }
        }
        static void MouseLongitudePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as GeoCanvas).MouseLongitudePropertyChanged(args);
        }
        void MouseLongitudePropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateVisual();
        }

        #endregion
        #region public string ModeInstructions { get; set; }

        public static readonly DependencyProperty ModeInstructionsProperty = DependencyProperty.Register("ModeInstructions",
            typeof(string), typeof(GeoCanvas), new FrameworkPropertyMetadata(""));
        public string ModeInstructions
        {
            get { return (string)GetValue(GeoCanvas.ModeInstructionsProperty); }
            set { SetValue(GeoCanvas.ModeInstructionsProperty, value); }
        }
        static void ModeInstructionsPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as GeoCanvas).ModeInstructionsPropertyChanged(args);
        }
        void ModeInstructionsPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateVisual();
        }
        #endregion
        #region public Rect BoundingBox { get; set; }

        public static readonly DependencyProperty BoundingBoxProperty = DependencyProperty.Register("BoundingBox",
            typeof(Rect), typeof(GeoCanvas));
        public Rect BoundingBox
        {
            get { return (Rect)GetValue(GeoCanvas.BoundingBoxProperty); }
            set { SetValue(GeoCanvas.BoundingBoxProperty, value); }
        }
        static void BoundingBoxPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as GeoCanvas).BoundingBoxPropertyChanged(args);
        }
        void BoundingBoxPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            Rect newBox = (Rect)args.NewValue;
            if (((newBox.Width != 0) && (newBox.Height != 0)) && 
                ((_boundingBox.Width != newBox.Width) || (_boundingBox.Height != newBox.Height)))
            {
                _boundingBox = newBox;

                this.Children.Clear();

                UpdateShapeTransform();
            }
        }
        #endregion
        #region public DisplayLayerCollection DisplayLayers { get; set; }

        public static readonly DependencyProperty DisplayLayersProperty = DependencyProperty.Register("DisplayLayers",
            typeof(DisplayLayerCollection), typeof(GeoCanvas));
        public DisplayLayerCollection DisplayLayers
        {
            get { return (DisplayLayerCollection)GetValue(GeoCanvas.DisplayLayersProperty); }
            set { SetValue(GeoCanvas.DisplayLayersProperty, value); }
        }
        static void DisplayLayersPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as GeoCanvas).DisplayLayersPropertyChanged(args);
        }
        void DisplayLayersPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            if (args.OldValue != null)
                RenderSurfaceChanged -= (args.OldValue as DisplayLayerCollection).RenderSurfaceChangedHandler;
            if (args.NewValue != null)
                RenderSurfaceChanged += (args.NewValue as DisplayLayerCollection).RenderSurfaceChangedHandler;
        }
        #endregion

        #endregion

        #region Mouse handlers
        protected override void OnMouseEnter(MouseEventArgs e)
        {
            _isMouseDragging = false;
        }

        protected override void OnMouseDown(MouseButtonEventArgs e)
        {
            if (_shapeTransform == null)
                return;

            // Update previous mouse location for start of dragging.
            _prevMouseLocation = e.GetPosition(this);
            _isMouseDragging = true;
            if ((e.LeftButton == MouseButtonState.Pressed) && (e.ClickCount == 1))
                _mouseCaptured = Mouse.Capture(this);
            else
            {
                if (_mouseCaptured)
                {
                    Mouse.Capture(null);
                }
            }
        }

        public event EventHandler<EventArgs> MouseClick;
        protected virtual void OnMouseClick()
        {
            if (MouseClick != null)
                MouseClick(this, new EventArgs());
        }

        /// <summary>
        /// Handle the MouseMove event for the canvas.
        /// </summary>
        /// <param name="sender">Event sender.</param>
        /// <param name="e">Event arguments.</param>
        protected override void OnMouseMove(MouseEventArgs e)
        {
            // First, apply the inverse of the view transformation.
            Point latLon = _viewTransform.Inverse.Transform(e.GetPosition(this));
            // Next, apply the inverse of the shape transformation if there is one
            if (_shapeTransform != null)
                latLon = _shapeTransform.Inverse.Transform(latLon);

            if (latLon.Y >= -90 && latLon.Y <= 90)
                MouseLatitude = latLon.Y;
            else
                MouseLatitude = double.NaN;

            if (latLon.X >= -180 && latLon.X <= 180)
                MouseLongitude = latLon.X;
            else
                MouseLongitude = double.NaN;

            // Implement panning with the mouse. 
            if (_isMouseDragging)
            {
                if (!_isPanningEnabled)
                    return;

                // Obtain the current mouse location and compute the
                // difference with the previous mouse location.
                Point currMouseLocation = e.GetPosition(this);
                double xOffset = currMouseLocation.X - _prevMouseLocation.X;
                double yOffset = currMouseLocation.Y - _prevMouseLocation.Y;

                // To avoid panning on every single mouse move, we check
                // if the movement is larger than the pan tolerance.
                if (Math.Abs(xOffset) > _panTolerance || Math.Abs(yOffset) > _panTolerance)
                {
                    _panTransform.X += xOffset;
                    _panTransform.Y += yOffset;

                    _prevMouseLocation = currMouseLocation;
                }
            }
        }

        protected override void OnMouseUp(MouseButtonEventArgs e)
        {
            _isMouseDragging = false;
            if (_mouseCaptured)
            {
                Mouse.Capture(null);
                OnMouseClick();
            }
        }

        protected override void OnMouseLeave(MouseEventArgs e)
        {
            _isMouseDragging = false;
            if (_mouseCaptured) Mouse.Capture(null);
        }
        #endregion

#if false
        public DisplayLayerCollection Layers
        {
            get { return _shapeListCollection; }
            set
            {
                if (_shapeListCollection != value)
                {
                    this.Children.Clear();
                    _shapeListCollection = value;
                    foreach (DisplayLayer Layer in _shapeListCollection)
                        foreach (GeoShape geoShape in Layer)
                            this.Children.Add(geoShape.Path);
                }
            }
        }
#endif
        internal TransformGroup ShapeTransform { get { return _shapeTransform; } }

        public event EventHandler<ShapeTransformChangedEventArgs> ShapeTransformChanged;
        protected virtual void OnShapeTransformChanged()
        {
            if (ShapeTransformChanged != null)
                ShapeTransformChanged(this, new ShapeTransformChangedEventArgs { ShapeTransform = _shapeTransform });
        }

        public event EventHandler<RenderSurfaceChangedEventArgs> RenderSurfaceChanged;
        protected virtual void OnRenderSurfaceChanged(RenderSurfaceChangedEventArgs e)
        {
            if (RenderSurfaceChanged != null)
                RenderSurfaceChanged(this, e);
        }

        public GeoCanvas()
        {
            LayoutUpdated += delegate(object sender, EventArgs e)
            {
                if ((ActualWidth == 0) || (ActualHeight == 0))
                    return;
                Rect newBox = new Rect(new Point(0, 0), new Size(ActualWidth, ActualHeight));
                if (((int)newBox.Top != (int)_boundingBox.Top) ||
                    ((int)newBox.Left != (int)_boundingBox.Left) ||
                    ((int)newBox.Width != (int)_boundingBox.Width) ||
                    ((int)newBox.Height != (int)_boundingBox.Height))
                {
                    _boundingBox = newBox;
                    if ((_boundingBox.Width != 0) && (_boundingBox.Height != 0))
                    {
                        this.Children.Clear();
                        UpdateShapeTransform();
                    }
                }
            };
        }

        void GeoCanvas_LayoutUpdated(object sender, EventArgs e)
        {
            throw new NotImplementedException();
        }

        #region Transformations
        /// <summary>
        /// Computes a transformation so that the shapefile geometry
        /// will maximize the available space on the canvas and be
        /// perfectly centered as well.
        /// </summary>
        /// <param name="info">Shapefile information.</param>
        /// <returns>A transformation object.</returns>
        public void UpdateShapeTransform()
        {
            // Width and height of the bounding box.
            double width = Math.Abs(_boundingBox.Width);
            double height = Math.Abs(_boundingBox.Height);

            // Aspect ratio of the bounding box.
            double aspectRatio = width / height;

            // Aspect ratio of the canvas.
            double canvasRatio = ActualWidth / ActualHeight;

            // Compute a scale factor so that the shapefile geometry
            // will maximize the space used on the canvas while still
            // maintaining its aspect ratio.
            double scaleFactor = 1.0;
            if (aspectRatio < canvasRatio)
                scaleFactor = ActualHeight / height;
            else
                scaleFactor = ActualWidth / width;

            //Console.WriteLine("Current scale factor: " + scaleFactor);
            // Compute the scale transformation. Note that we flip
            // the Y-values because the lon/lat grid is like a cartesian
            // coordinate system where Y-values increase upwards.
            _scaleTransform = new ScaleTransform();
            _scaleTransform.ScaleX = scaleFactor;
            _scaleTransform.ScaleY = -scaleFactor;

            // Compute the translate transformation so that the shapefile
            // geometry will be centered on the canvas.
            _translateTransform = new TranslateTransform();
            _translateTransform.X = ActualWidth - ((_boundingBox.Right * scaleFactor) / 2);
            _translateTransform.Y = ActualHeight + ((_boundingBox.Bottom * scaleFactor) / 2);

            _shapeTransform = new TransformGroup();
            _shapeTransform.Children.Add(_scaleTransform);
            _shapeTransform.Children.Add(_translateTransform);
            
            OnRenderSurfaceChanged(new RenderSurfaceChangedEventArgs 
            { 
                NewSize = new Size(ActualWidth, ActualHeight), 
                NewTransform = _shapeTransform 
            });
        }

        protected override void OnRenderSizeChanged(SizeChangedInfo sizeInfo)
        {
            BoundingBox = new Rect(new Point(0, 0), sizeInfo.NewSize);
        }
        #endregion Transformations

        bool _isMouseDragging = false;
        bool _isPanningEnabled = false;
        bool _mouseCaptured = false;
        const double _panTolerance = 1;
        TransformGroup _shapeTransform;
        ScaleTransform _scaleTransform;
        TranslateTransform _translateTransform;

        TransformGroup _viewTransform = new TransformGroup();
        ScaleTransform _zoomTransform = new ScaleTransform();
        TranslateTransform _panTransform = new TranslateTransform();
        Point _prevMouseLocation;
        Rect _boundingBox = Rect.Empty;
    }

    public class ShapeTransformChangedEventArgs : EventArgs
    {
        public TransformGroup ShapeTransform { get; internal set; }
    }
}
