using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace ESME.Views.ColorPicker
{
    public class ColorSlider : Slider
    {
        #region Public Methods

        static ColorSlider()
        {
            DefaultStyleKeyProperty.OverrideMetadata(typeof(ColorSlider), new FrameworkPropertyMetadata(typeof(ColorSlider)));
        }

        #endregion

        #region Dependency Properties

        public Color LeftColor
        {
            get { return (Color)GetValue(LeftColorProperty); }
            set { SetCurrentValue(LeftColorProperty, value); }
        }
        public static readonly DependencyProperty LeftColorProperty =
            DependencyProperty.Register("LeftColor", typeof(Color), typeof(ColorSlider), new UIPropertyMetadata(Colors.Black));

        public Color RightColor
        {
            get { return (Color)GetValue(RightColorProperty); }
            set { SetCurrentValue(RightColorProperty, value); }
        }
        public static readonly DependencyProperty RightColorProperty =
            DependencyProperty.Register("RightColor", typeof(Color), typeof(ColorSlider), new UIPropertyMetadata(Colors.White));

        // TESTING
        //
        //public System.String Text
        //{
        //    get { return (System.String)GetValue(TextProperty); }
        //    set { SetCurrentValue(TextProperty, value); }
        //}

        //// Using a DependencyProperty as the backing store for Text.  This enables animation, styling, binding, etc...
        //public static readonly DependencyProperty TextProperty =
        //    DependencyProperty.Register("Text", typeof(System.String), typeof(ColorSlider), new UIPropertyMetadata(""));

        #endregion
    }
}
