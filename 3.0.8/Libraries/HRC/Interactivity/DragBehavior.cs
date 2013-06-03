using System.Windows.Interactivity;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;

namespace HRC.Interactivity
{
    public class DragBehavior : Behavior<UIElement>
    {
        private bool _isDragging;
        private UIElement _attachedElement;
        private Window _parent;

        Point _lastPosition;

        protected override void OnAttached()
        {
            _attachedElement = AssociatedObject;
            _parent = Application.Current.MainWindow;

            _attachedElement.MouseLeftButtonDown += MouseIsDown;
            _attachedElement.MouseLeftButtonUp += MouseIsUp;
            _attachedElement.MouseMove += MouseIsMoving;
        }

        void MouseIsMoving(object sender, MouseEventArgs e)
        {
            if (_isDragging)
            {
                var currentPosition = e.GetPosition(_parent);

                var dX = currentPosition.X - _lastPosition.X;
                var dY = currentPosition.Y - _lastPosition.Y;

                _lastPosition = currentPosition;

                var oldTransform = _attachedElement.RenderTransform;
                var rt = new TransformGroup();
                var newPos = new TranslateTransform {X = dX, Y = dY};

                if (oldTransform != null)
                {
                    rt.Children.Add(oldTransform);
                }
                rt.Children.Add(newPos);

                var mt = new MatrixTransform {Matrix = rt.Value};

                _attachedElement.RenderTransform = mt;
            }
        }

        void MouseIsUp(object sender, MouseButtonEventArgs e)
        {
            _isDragging = false;
            
            _attachedElement.ReleaseMouseCapture();
        }

        void MouseIsDown(object sender, MouseButtonEventArgs e)
        {
            _isDragging = true;
            _lastPosition = e.GetPosition(_parent);
            _attachedElement.CaptureMouse();
        }
    }
}
