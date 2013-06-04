using System.Windows.Interactivity;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media.Animation;

namespace HRC.Interactivity
{
    public class StopStoryboardAction : TargetedTriggerAction<FrameworkElement>
    {
        #region Property to Expose
        [Category("Storyboard Properties")]

        public string StoryboardName
        {
            get { return (string)GetValue(StoryboardNameProperty); }
            set { SetCurrentValue(StoryboardNameProperty, value); }
        }

        public static readonly DependencyProperty StoryboardNameProperty = DependencyProperty.Register("Storyboard Name", typeof(string), typeof(StopStoryboardAction), new PropertyMetadata(StoryboardNamePropertyChanged));

        #endregion

        protected override void Invoke(object o)
        {
            var parent = Application.Current.MainWindow;
            var invokedStoryboard = parent.Resources[StoryboardName] as Storyboard;
            if (invokedStoryboard != null) invokedStoryboard.Stop();
        }

        private static void StoryboardNamePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            // Do something
        }
    }
}
