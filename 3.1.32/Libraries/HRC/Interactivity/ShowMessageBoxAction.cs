using System.Windows.Interactivity;
using System.ComponentModel;
using System.Windows;

namespace HRC.Interactivity
{
    public class ShowMessageBoxAction : TargetedTriggerAction<FrameworkElement>
    {
        #region Property to Expose
        [Category("Message Box Properties")]

        public string MessageBoxText
        {
            get { return (string)GetValue(MessageBoxTextProperty); }
            set { SetCurrentValue(MessageBoxTextProperty, value); }
        }

        public static readonly DependencyProperty MessageBoxTextProperty = 
            DependencyProperty.Register("Storyboard Name", typeof(string), typeof(ShowMessageBoxAction), 
            new PropertyMetadata("Message"));

        #endregion

        protected override void Invoke(object o)
        {
            MessageBox.Show(MessageBoxText);
        }
    }
}
