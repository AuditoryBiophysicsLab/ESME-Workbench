using System.Windows;
using System.Windows.Controls.Primitives;
using System.Windows.Input;

namespace HRC.Interactivity
{
    public static class TextBoxSelectionHandler
    {
        /// <summary>
        /// Is set to true in TextBox_PreviewMouseDown (if TextBox.IsKeyboardFocused = true) and 
        /// set to false in TextBox_LostMouseCapture with the purpose of stopping autoselection of 
        /// a TextBox's contents on initial mouse click if the TextBox is already focused.
        /// </summary>
        private static bool _alreadyHasFocus;
        /// <summary>
        /// Is set to true in TextBox_PreviewMouseDown and set to false in TextBox_LostMouseCapture
        /// with the purpose of enable autoselection of a TextBox's contents on initial mouse click.
        /// </summary>
        private static bool _isMouseEvent;

        /// <summary>
        /// Attaches the TextBox selection behavior to a style from code-behind.
        /// </summary>
        public static void AssignTo(Style style)
        {
            if (style != null)
            {
                //<EventSetter Event="PreviewMouseDown" Handler="{Binding Source={StaticResource TextBoxSelectionHandler}, Path=TextBox_PreviewMouseDown}" />
                //<EventSetter Event="LostMouseCapture" Handler="{Binding Source={StaticResource TextBoxSelectionHandler}, Path=TextBox_LostMouseCapture}" />
                //<EventSetter Event="GotKeyboardFocus" Handler="{Binding Source={StaticResource TextBoxSelectionHandler}, Path=TextBox_GotKeyboardFocus}" />
                var setter = new EventSetter(UIElement.PreviewMouseDownEvent, new MouseButtonEventHandler(TextBoxPreviewMouseDown));
                style.Setters.Add(setter);
                setter = new EventSetter(UIElement.LostMouseCaptureEvent, new MouseEventHandler(TextBoxLostMouseCapture));
                style.Setters.Add(setter);
                setter = new EventSetter(UIElement.GotKeyboardFocusEvent, new KeyboardFocusChangedEventHandler(TextBoxGotKeyboardFocus));
                style.Setters.Add(setter);
            }
        }

        /// <summary>
        /// Attaches the TextBox selection behavior.
        /// </summary>
        public static void Attach(TextBoxBase textBox)
        {
            if (textBox != null)
            {
                textBox.PreviewMouseDown += TextBoxPreviewMouseDown;
                textBox.LostMouseCapture += TextBoxLostMouseCapture;
                textBox.GotKeyboardFocus += TextBoxGotKeyboardFocus;
            }
        }

        /// <summary>
        /// Detaches the TextBox selection behavior.
        /// </summary>
        public static void Detach(TextBoxBase textBox)
        {
            if (textBox != null)
            {
                textBox.PreviewMouseDown -= TextBoxPreviewMouseDown;
                textBox.LostMouseCapture -= TextBoxLostMouseCapture;
                textBox.GotKeyboardFocus -= TextBoxGotKeyboardFocus;
            }
        }

        /// <summary>
        /// Autoselects the TextBox's contents unless the event handler is raised as a result of the
        /// user clicking on the TextBox (as compared to tabbing into it).
        /// </summary>
        public static void TextBoxGotKeyboardFocus(object sender, KeyboardFocusChangedEventArgs e)
        {
            //string elemName = ((FrameworkElement)e.OriginalSource).Name;
            //Debug.Print("{0}.{1}", elemName, e.RoutedEvent.Name);

            if (!_isMouseEvent)
            {
                var textBox = sender as TextBoxBase;
                if (textBox != null)
                {
                    //Debug.Print("Selecting text...");
                    textBox.SelectAll();
                }
            }
        }

        /// <summary>
        /// Clears the values of IsMouseEvent and AlreadyHasFocus set in the PreviewMouseDown event
        /// handler.
        /// </summary>
        public static void TextBoxLostMouseCapture(object sender, MouseEventArgs e)
        {
            //string elemName = ((FrameworkElement)e.OriginalSource).Name;
            //Debug.Print("{0}.{1}", elemName, e.RoutedEvent.Name);

            var textBox = sender as TextBoxBase;
            if (!_alreadyHasFocus && textBox != null)
            {
                //Debug.Print("Selecting text...");
                textBox.SelectAll();
            }

            _isMouseEvent = false;
            _alreadyHasFocus = false;
        }

        /// <summary>
        /// Sets IsMouseEvent to true with the purpose of enabling autoselection of a TextBox's 
        /// contents on initial mouse click.
        /// 
        /// Sets AlreadyHasFocus to true (if TextBox.IsKeyboardFocused = true) with the purpose 
        /// of stopping autoselection of a TextBox's contents on mouse click if the TextBox is 
        /// already focused.
        /// </summary>
        public static void TextBoxPreviewMouseDown(object sender, MouseButtonEventArgs e)
        {
            //string elemName = ((FrameworkElement)e.OriginalSource).Name;
            //Debug.Print("{0}.{1}", elemName, e.RoutedEvent.Name);

            _isMouseEvent = true;
            var textBox = sender as TextBoxBase;
            _alreadyHasFocus = textBox != null ? textBox.IsKeyboardFocused : false;
        }
    }
}
