using System.Windows.Interactivity;
using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace HRC.Interactivity
{
    public class RandomMovementBehavior : Behavior<Canvas>
    {
        private double _i;
        private Canvas _hostPanel;

        static readonly Random RanNum = new Random();
        static Point _parentWindowSize;

        bool _setProperties;

        #region Property to Expose
        [Category("Animation Properties")]

        public double Speed
        {
            get { return (double)GetValue(SpeedProperty); }
            set { SetValue(SpeedProperty, value); }
        }

        public static readonly DependencyProperty SpeedProperty = DependencyProperty.Register("Movement speed", typeof(double), typeof(RandomMovementBehavior), new PropertyMetadata(5.0));

        #endregion

        protected override void OnAttached()
        {
            SetProperties();

            if (!DesignerProperties.GetIsInDesignMode(this))
            {
                CompositionTarget.Rendering += CompositionTargetRendering;
            }
        }

        private void SetProperties()
        {
            _hostPanel = AssociatedObject;
            _parentWindowSize = new Point(_hostPanel.ActualWidth, _hostPanel.ActualHeight);

            _i = 0;

            foreach (FrameworkElement element in _hostPanel.Children)
            {
                var speed = RanNum.NextDouble() * Speed;
                double radius = RanNum.Next(10, 20);

                var randomPosition = new Point(RanNum.Next((int)-element.ActualWidth, (int)(_parentWindowSize.X + element.ActualWidth)), RanNum.Next((int)-element.ActualHeight, (int)(_parentWindowSize.Y + element.ActualHeight)));

                if (element.Tag == null)
                {
                    element.Tag = new ElementProperties { Radius = radius, Speed = speed, CurrentPosition = new Point(Canvas.GetLeft(element), Canvas.GetTop(element)), RandomPosition = randomPosition };
                }
            }
        }

        void CompositionTargetRendering(object sender, EventArgs e)
        {
            if (!_setProperties)
            {
                SetProperties();
                _setProperties = true;
            }
            else
            {
                Animate();
            }
        }

        private void Animate()
        {
            _i += 1;
            foreach (FrameworkElement element in _hostPanel.Children)
            {
                var elementProperty = element.Tag as ElementProperties;

                if (elementProperty != null)
                {

                    var currentX = Canvas.GetLeft(element);
                    var currentY = Canvas.GetTop(element);

                    var setX = elementProperty.CurrentPosition.X;
                    var setY = elementProperty.CurrentPosition.Y;

                    var futureX = elementProperty.RandomPosition.X;
                    var futureY = elementProperty.RandomPosition.Y;

                    var dX = futureX - setX;
                    var dY = futureY - setY;

                    Math.Sqrt(Math.Pow(dX, 2) + Math.Pow(dY, 2));

                    if (InDestinationRegion(currentX, futureX) || InDestinationRegion(currentY, futureY))
                    {
                        SetRandomCoordinates(element);
                    }
                    else
                    {
                        Canvas.SetLeft(element, currentX + dX / 100);
                        Canvas.SetTop(element, currentY + dY / 100);
                    }
                }
            }
        }

        private static bool InDestinationRegion(double current, double future)
        {
            if (Math.Abs(current - future) < 2)
            {
                return true;
            }
            return false;
        }

        private void SetRandomCoordinates(FrameworkElement element)
        {
            var elementProperty = element.Tag as ElementProperties;

            _parentWindowSize = new Point(_hostPanel.ActualWidth, _hostPanel.ActualHeight);

            if (elementProperty != null) 
            {
                elementProperty.CurrentPosition = new Point(Canvas.GetLeft(element), Canvas.GetTop(element));
                elementProperty.RandomPosition = new Point(RanNum.Next((int)-element.ActualWidth, (int)(_parentWindowSize.X + element.ActualWidth)), RanNum.Next((int)-element.ActualHeight, (int)(_parentWindowSize.Y + element.ActualHeight)));
            }
        }

        public class ElementProperties
        {
            public double Radius
            {
                get;
                set;
            }

            public double Speed
            {
                get;
                set;
            }

            public Point CurrentPosition
            {
                get;
                set;
            }

            public Point RandomPosition
            {
                get;
                set;
            }
        }
    }
}
