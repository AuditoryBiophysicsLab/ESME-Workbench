using System;
using System.Windows;
using System.Windows.Interactivity;

namespace HRC.Triggers
{
    public class ValidationErrorEventTrigger : EventTriggerBase<DependencyObject>
    {
        protected override void OnAttached()
        {
            var behavior = AssociatedObject as Behavior;
            var associatedElement = AssociatedObject as FrameworkElement;

            if (behavior != null)
            {
                associatedElement = ((IAttachedObject)behavior).AssociatedObject as FrameworkElement;
            }
            if (associatedElement == null)
            {
                throw new ArgumentException("Validation Error Event trigger can only be associated to framework elements");
            }
            associatedElement.AddHandler(System.Windows.Controls.Validation.ErrorEvent, new RoutedEventHandler(OnValidationError));
        }
        void OnValidationError(object sender, RoutedEventArgs args)
        {
            base.OnEvent(args);
        }
        protected override string GetEventName()
        {
            return System.Windows.Controls.Validation.ErrorEvent.Name;
        }
    }
}