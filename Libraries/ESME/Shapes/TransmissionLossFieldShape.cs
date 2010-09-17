using System;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;

namespace ESME.Shapes
{
    public class TransmissionLossFieldShape : Shape
    {
        EllipseGeometry center = new EllipseGeometry();
        RotateTransform xform = new RotateTransform();

        #region Dependency Properties

        #region public Point Location { get; set; }
        public static readonly DependencyProperty LocationProperty = DependencyProperty.Register("Location",
            typeof(Point), typeof(TransmissionLossFieldShape), 
            new PropertyMetadata(new Point(0, 0), new PropertyChangedCallback(LocationPropertyChanged)));
        public Point Location
        {
            get { return (Point)GetValue(TransmissionLossFieldShape.LocationProperty); }
            set { SetValue(TransmissionLossFieldShape.LocationProperty, value); }
        }
        static void LocationPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as TransmissionLossFieldShape).LocationPropertyChanged(args);
        }
        void LocationPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #region public int RadialCount { get; set; }
        public static readonly DependencyProperty RadialCountProperty = DependencyProperty.Register("RadialCount",
            typeof(int), typeof(TransmissionLossFieldShape), 
            new PropertyMetadata((object)8, new PropertyChangedCallback(RadialCountPropertyChanged)));
        public int RadialCount
        {
            get { return (int)GetValue(TransmissionLossFieldShape.RadialCountProperty); }
            set { SetValue(TransmissionLossFieldShape.RadialCountProperty, value); }
        }
        static void RadialCountPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as TransmissionLossFieldShape).RadialCountPropertyChanged(args);
        }
        void RadialCountPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #region public double RadialBearing_degrees { get; set; }
        public static readonly DependencyProperty RadialBearing_degreesProperty = DependencyProperty.Register("RadialBearing_degrees",
            typeof(double), typeof(TransmissionLossFieldShape),
            new PropertyMetadata((object)0, new PropertyChangedCallback(RadialBearing_degreesPropertyChanged)));
        public double RadialBearing_degrees
        {
            get { return (double)GetValue(TransmissionLossFieldShape.RadialBearing_degreesProperty); }
            set { SetValue(TransmissionLossFieldShape.RadialBearing_degreesProperty, value); }
        }
        static void RadialBearing_degreesPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as TransmissionLossFieldShape).RadialBearing_degreesPropertyChanged(args);
        }
        void RadialBearing_degreesPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #region public double Radius_meters { get; set; }
        public static readonly DependencyProperty Radius_metersProperty = DependencyProperty.Register("Radius_meters",
            typeof(double), typeof(TransmissionLossFieldShape), 
            new PropertyMetadata((object)25000, new PropertyChangedCallback(Radius_metersPropertyChanged)));
        public double Radius_meters
        {
            get { return (double)GetValue(TransmissionLossFieldShape.Radius_metersProperty); }
            set { SetValue(TransmissionLossFieldShape.Radius_metersProperty, value); }
        }
        static void Radius_metersPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            (obj as TransmissionLossFieldShape).Radius_metersPropertyChanged(args);
        }
        void Radius_metersPropertyChanged(DependencyPropertyChangedEventArgs args)
        {
            InvalidateMeasure();
        }
        #endregion

        #endregion

        public TransmissionLossFieldShape()
        {
            center.Transform = xform;
        }

        // Required DefiningGeometry override
        protected override Geometry DefiningGeometry
        {
            get
            {
                return center;
            }
        }
    }
}

