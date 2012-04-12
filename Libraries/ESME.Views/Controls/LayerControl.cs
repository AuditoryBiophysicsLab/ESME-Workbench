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

        #region dependency property Color LineOrSymbolColor

        public static DependencyProperty LineOrSymbolColorProperty = DependencyProperty.Register("LineOrSymbolColor",
                                                                                 typeof (Color),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(Colors.Red, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Color LineOrSymbolColor
        {
            get { return (Color)GetValue(LineOrSymbolColorProperty); }
            set { SetValue(LineOrSymbolColorProperty, value); }
        }

        #endregion

        #region dependency property double LineOrSymbolSize

        public static DependencyProperty LineOrSymbolSizeProperty = DependencyProperty.Register("LineOrSymbolSize",
                                                                                 typeof (double),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(1.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public double LineOrSymbolSize
        {
            get { return (double)GetValue(LineOrSymbolSizeProperty); }
            set { SetValue(LineOrSymbolSizeProperty, value); }
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
