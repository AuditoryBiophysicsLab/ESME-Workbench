using System.Windows.Interactivity;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media.Animation;

namespace HRC.Interactivity
{
    public class PlayStoryboardAction : TargetedTriggerAction<FrameworkElement>
    {
        #region Property to Expose
        [Category("PlayStoryboard Properties")]

        public string StoryboardName
        {
            get { return (string)GetValue(StoryboardNameProperty); }
            set { SetValue(StoryboardNameProperty, value); }
        }

        public static readonly DependencyProperty StoryboardNameProperty = DependencyProperty.Register("Storyboard Name", typeof(string), typeof(PlayStoryboardAction), new PropertyMetadata(StoryboardNamePropertyChanged));

        #endregion

        protected override void Invoke(object o)
        {
            var parent = Application.Current.MainWindow;
            var invokedStoryboard = parent.Resources[StoryboardName] as Storyboard;
            if (invokedStoryboard != null) invokedStoryboard.Begin();
        }

        private static void StoryboardNamePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {

        }
		
    }
}
