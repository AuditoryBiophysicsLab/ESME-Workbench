using System.Windows.Interactivity;
using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace HRC.Interactivity
{
    public class RemoveElementAction : TargetedTriggerAction<FrameworkElement>
    {
        #region Property to Expose
        [Category("Remove Element")]

        public string ElementName
        {
            get { return (string)GetValue(ElementNameProperty); }
            set { SetCurrentValue(ElementNameProperty, value); }
        }

        public static readonly DependencyProperty ElementNameProperty = DependencyProperty.Register("Element Name", typeof(string), typeof(RemoveElementAction), new PropertyMetadata(ElementNamePropertyPropertyChanged));

        #endregion

        protected override void Invoke(object o)
        {
            var parent = Application.Current.MainWindow;
            var elementToRemove = parent.FindName(ElementName) as UIElement;
            if (elementToRemove != null)
            {
                try
                {
                    var parentOfElementToRemove = VisualTreeHelper.GetParent(elementToRemove) as Panel;
                    if (parentOfElementToRemove != null) parentOfElementToRemove.Children.Remove(elementToRemove);
                }
                catch (Exception e)
                {
                    var error = e.Message;
                    // More than likely, the parent was a UserControl
                }
            }
        }

        private static void ElementNamePropertyPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            // Do something
        }
    }
}
