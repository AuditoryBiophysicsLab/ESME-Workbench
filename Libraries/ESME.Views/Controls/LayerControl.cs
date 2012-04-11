using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
namespace ESME.Views.Controls
{
    public class LayerControl : Control
    {
        static LayerControl() 
        {
            DefaultStyleKeyProperty.OverrideMetadata(typeof(LayerControl), new FrameworkPropertyMetadata(typeof(LayerControl)));
        }

        #region dependency property bool? IsChecked

        public static DependencyProperty IsCheckedProperty = DependencyProperty.Register("IsChecked",
                                                                                         typeof (bool?),
                                                                                         typeof (LayerControl),
                                                                                         new FrameworkPropertyMetadata(true, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool? IsChecked
        {
            get { return (bool?)GetValue(IsCheckedProperty); }
            set { SetValue(IsCheckedProperty, value); }
        }

        #endregion

        #region dependency property Color LineColor

        public static DependencyProperty LineColorProperty = DependencyProperty.Register("LineColor",
                                                                                         typeof (Color),
                                                                                         typeof (LayerControl),
                                                                                         new FrameworkPropertyMetadata(Colors.Red, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Color LineColor
        {
            get { return (Color)GetValue(LineColorProperty); }
            set { SetValue(LineColorProperty, value); }
        }

        #endregion

        #region dependency property string LayerName

        public static DependencyProperty LayerNameProperty = DependencyProperty.Register("LayerName",
                                                                                         typeof (string),
                                                                                         typeof (LayerControl),
                                                                                         new FrameworkPropertyMetadata("LayerName", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string LayerName
        {
            get { return (string)GetValue(LayerNameProperty); }
            set { SetValue(LayerNameProperty, value); }
        }

        #endregion

        #region dependency property Visibility LineColorVisibility

        public static DependencyProperty LineColorVisibilityProperty = DependencyProperty.Register("LineColorVisibility",
                                                                                                   typeof (Visibility),
                                                                                                   typeof (LayerControl),
                                                                                                   new FrameworkPropertyMetadata(Visibility.Visible, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Visibility LineColorVisibility
        {
            get { return (Visibility)GetValue(LineColorVisibilityProperty); }
            set { SetValue(LineColorVisibilityProperty, value); }
        }

        #endregion


    }
}
