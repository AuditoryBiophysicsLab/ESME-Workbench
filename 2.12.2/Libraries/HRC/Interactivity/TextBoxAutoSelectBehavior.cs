using System.Windows.Interactivity;
using System.Windows.Controls.Primitives;

namespace HRC.Interactivity
{
    public class TextBoxAutoSelectBehavior : Behavior<TextBoxBase>
    {
        private TextBoxBase _attachedElement;

        protected override void OnAttached()
        {
            _attachedElement = AssociatedObject;
            TextBoxSelectionHandler.Attach(_attachedElement);
        }

        protected override void OnDetaching()
        {
            TextBoxSelectionHandler.Detach(_attachedElement);
        }
    }
}
