using System.Windows;
using System.Windows.Input;

namespace HRC.WPF
{
    public static class UIElementHelper
    {
        //
        // The UIElementItem that the mouse is currently directly over (or null).
        //
        static UIElement _currentItem;

        //
        // IsMouseDirectlyOverItem:  A DependencyProperty that will be true only on the
        // UIElementItem that the mouse is directly over.  I.e., this won't be set on that
        // parent item.
        //
        // This is the only public member, and is read-only.
        //

        // The property key (since this is a read-only DP)
        static readonly DependencyPropertyKey IsMouseDirectlyOverItemKey =
            DependencyProperty.RegisterAttachedReadOnly("IsMouseDirectlyOverItem",
                                                        typeof (bool),
                                                        typeof (UIElementHelper),
                                                        new FrameworkPropertyMetadata(null, CalculateIsMouseDirectlyOverItem));

        // The DP itself
        public static readonly DependencyProperty IsMouseDirectlyOverItemProperty =
            IsMouseDirectlyOverItemKey.DependencyProperty;

        // A strongly-typed getter for the property.
        public static bool GetIsMouseDirectlyOverItem(DependencyObject obj) { return (bool)obj.GetValue(IsMouseDirectlyOverItemProperty); }

        // A coercion method for the property
        static object CalculateIsMouseDirectlyOverItem(DependencyObject item, object value)
        {
            // This method is called when the IsMouseDirectlyOver property is being calculated
            // for a UIElementItem. 

            return item == _currentItem;
        }

        //
        // UpdateOverItem:  A private RoutedEvent used to find the nearest encapsulating
        // UIElementItem to the mouse's current position.
        //

        static readonly RoutedEvent UpdateOverItemEvent = EventManager.RegisterRoutedEvent("UpdateOverItem", RoutingStrategy.Bubble, typeof (RoutedEventHandler), typeof (UIElementHelper));

        //
        // Class constructor
        //

        static UIElementHelper()
        {
            // Get all Mouse enter/leave events for UIElementItem.
            EventManager.RegisterClassHandler(typeof (UIElement),
                                              UIElement.MouseEnterEvent,
                                              new MouseEventHandler(OnMouseTransition),
                                              true);
            EventManager.RegisterClassHandler(typeof (UIElement),
                                              UIElement.MouseLeaveEvent,
                                              new MouseEventHandler(OnMouseTransition),
                                              true);

            // Listen for the UpdateOverItemEvent on all UIElementItem's.
            EventManager.RegisterClassHandler(typeof (UIElement),
                                              UpdateOverItemEvent,
                                              new RoutedEventHandler(OnUpdateOverItem));
        }


        //
        // OnUpdateOverItem:  This method is a listener for the UpdateOverItemEvent.  When it is received,
        // it means that the sender is the closest UIElementItem to the mouse (closest in the sense of the
        // tree, not geographically).

        static void OnUpdateOverItem(object sender, RoutedEventArgs args)
        {
            // Mark this object as the tree view item over which the mouse
            // is currently positioned.
            _currentItem = sender as UIElement;

            // Tell that item to re-calculate the IsMouseDirectlyOverItem property
            _currentItem.InvalidateProperty(IsMouseDirectlyOverItemProperty);

            // Prevent this event from notifying other tree view items higher in the tree.
            args.Handled = true;
        }

        //
        // OnMouseTransition:  This method is a listener for both the MouseEnter event and
        // the MouseLeave event on UIElementItems.  It updates the _currentItem, and updates
        // the IsMouseDirectlyOverItem property on the previous UIElementItem and the new
        // UIElementItem.

        static void OnMouseTransition(object sender, MouseEventArgs args)
        {
            lock (IsMouseDirectlyOverItemProperty)
            {
                if (_currentItem != null)
                {
                    // Tell the item that previously had the mouse that it no longer does.
                    DependencyObject oldItem = _currentItem;
                    _currentItem = null;
                    oldItem.InvalidateProperty(IsMouseDirectlyOverItemProperty);
                }

                // Get the element that is currently under the mouse.
                var currentPosition = Mouse.DirectlyOver;

                // See if the mouse is still over something (any element, not just a tree view item).
                if (currentPosition == null) return;
                // Yes, the mouse is over something.
                // Raise an event from that point.  If a UIElementItem is anywhere above this point
                // in the tree, it will receive this event and update _currentItem.

                var newItemArgs = new RoutedEventArgs(UpdateOverItemEvent);
                currentPosition.RaiseEvent(newItemArgs);
            }
        }
    }
}
