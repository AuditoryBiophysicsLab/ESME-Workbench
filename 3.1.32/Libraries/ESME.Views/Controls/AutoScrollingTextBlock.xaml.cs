using System.Windows;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Interaction logic for AutoScrollingTextBlock.xaml
    /// </summary>
    public partial class AutoScrollingTextBlock
    {
        public AutoScrollingTextBlock()
        {
            InitializeComponent();
        }

        #region dependency property bool AutoScrollingEnabled

        public static DependencyProperty AutoScrollingEnabledProperty = DependencyProperty.Register("AutoScrollingEnabled", typeof (bool), typeof (AutoScrollingTextBlock),
                                                                                 new FrameworkPropertyMetadata(true, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool AutoScrollingEnabled
        {
            get { return (bool)GetValue(AutoScrollingEnabledProperty); }
            set { SetCurrentValue(AutoScrollingEnabledProperty, value); }
        }

        #endregion

        #region dependency property string Text

        public static DependencyProperty TextProperty = DependencyProperty.Register("Text", typeof (string), typeof (AutoScrollingTextBlock),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, TextChanged));

        public string Text
        {
            get { return (string)GetValue(TextProperty); }
            set { SetCurrentValue(TextProperty, value); }
        }

        public static void TextChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var my = (AutoScrollingTextBlock)d;
            if (my.AutoScrollingEnabled) my.ScrollViewer.ScrollToBottom();
            else my.ScrollViewer.ScrollToVerticalOffset(my.ScrollViewer.VerticalOffset);
        }

        #endregion
    }
}
