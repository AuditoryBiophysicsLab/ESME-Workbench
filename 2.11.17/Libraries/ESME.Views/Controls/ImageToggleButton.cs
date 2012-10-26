using System.Windows;
using System.Windows.Controls.Primitives;
using System.Windows.Media;

namespace ESME.Views.Controls
{
    public class ImageToggleButton : ToggleButton
    {
        static ImageToggleButton() 
        {
            DefaultStyleKeyProperty.OverrideMetadata(typeof(ImageToggleButton), new FrameworkPropertyMetadata(typeof(ImageToggleButton)));
        }

        #region dependency property ImageSource ImageSource

        public static DependencyProperty ImageSourceProperty = DependencyProperty.Register("ImageSource",
                                                                                 typeof (ImageSource),
                                                                                 typeof (ImageToggleButton),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public ImageSource ImageSource
        {
            get { return (ImageSource)GetValue(ImageSourceProperty); }
            set { SetCurrentValue(ImageSourceProperty, value); }
        }

        #endregion

    }
}
