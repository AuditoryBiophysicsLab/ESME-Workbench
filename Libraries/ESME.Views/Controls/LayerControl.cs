using System;
using System.Diagnostics;
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

        public LayerControl() {Debug.WriteLine(string.Format("{0}: LayerControl Constructor", DateTime.Now));}
        static void OnPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            //Debug.WriteLine("{0}: LayerName = {1},  Property = {2}, OldValue = {3}, NewValue = {4}", DateTime.Now, ((LayerControl)sender).LayerName, args.Property.Name, args.OldValue, args.NewValue);
        }

        #region dependency property bool IsMapLayer

        public static DependencyProperty IsMapLayerProperty = DependencyProperty.Register("IsMapLayer",
                                                                                 typeof (bool),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(true, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, IsMapLayerPropertyChanged));

        static void IsMapLayerPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            OnPropertyChanged(sender, args);
            var layerControl = (LayerControl)sender;
            var isMapLayer = (bool)args.NewValue;
            Debug.WriteLine("{0}: IsMapLayerPropertyChanged: LayerName = {1}, IsChecked(old) = {2}, IsChecked(new) = {3}", DateTime.Now, layerControl.LayerName, (bool)args.OldValue, (bool)args.NewValue);
            layerControl.CheckBoxVisibility = isMapLayer ? Visibility.Visible : Visibility.Collapsed;
            layerControl.LineColorVisibility = isMapLayer ? Visibility.Visible : Visibility.Collapsed;
        }

        public bool IsMapLayer
        {
            get { return (bool)GetValue(IsMapLayerProperty); }
            set { SetCurrentValue(IsMapLayerProperty, value); }
        }

        #endregion

        #region dependency property bool IsChecked

        public static DependencyProperty IsCheckedProperty = DependencyProperty.Register("IsChecked",
                                                                                         typeof (bool),
                                                                                         typeof (LayerControl),
                                                                                         new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnPropertyChanged));
        
        public bool IsChecked
        {
            get { return (bool)GetValue(IsCheckedProperty); }
            set { SetCurrentValue(IsCheckedProperty, value); }
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
            set { SetCurrentValue(LineOrSymbolColorProperty, value); }
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
            set { SetCurrentValue(LineOrSymbolSizeProperty, value); }
        }

        #endregion

        #region dependency property string LayerName

        public static DependencyProperty LayerNameProperty = DependencyProperty.Register("LayerName",
                                                                                         typeof (string),
                                                                                         typeof (LayerControl),
                                                                                         new FrameworkPropertyMetadata("LayerName", OnPropertyChanged));

        public string LayerName
        {
            get { return (string)GetValue(LayerNameProperty); }
            set { SetCurrentValue(LayerNameProperty, value); }
        }

        #endregion

        #region dependency property Visibility CheckBoxVisibility

        public static DependencyProperty CheckBoxVisibilityProperty = DependencyProperty.Register("CheckBoxVisibility",
                                                                                 typeof (Visibility),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(Visibility.Visible, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnPropertyChanged));

        public Visibility CheckBoxVisibility
        {
            get { return (Visibility)GetValue(CheckBoxVisibilityProperty); }
            set { SetCurrentValue(CheckBoxVisibilityProperty, value); }
        }

        #endregion

        #region dependency property Visibility LineColorVisibility

        public static DependencyProperty LineColorVisibilityProperty = DependencyProperty.Register("LineColorVisibility",
                                                                                                   typeof (Visibility),
                                                                                                   typeof (LayerControl),
                                                                                                   new FrameworkPropertyMetadata(Visibility.Visible, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnPropertyChanged));

        public Visibility LineColorVisibility
        {
            get { return (Visibility)GetValue(LineColorVisibilityProperty); }
            set { SetCurrentValue(LineColorVisibilityProperty, value); }
        }

        #endregion

        #region dependency property object ContextControl

        public static DependencyProperty ContextControlProperty = DependencyProperty.Register("ContextControl",
                                                                                 typeof (object),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public object ContextControl
        {
            get { return GetValue(ContextControlProperty); }
            set { SetCurrentValue(ContextControlProperty, value); }
        }

        #endregion

        #region dependency property bool IsSelected

        public static DependencyProperty IsSelectedProperty = DependencyProperty.Register("IsSelected",
                                                                                 typeof (bool),
                                                                                 typeof (LayerControl),
                                                                                 new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool IsSelected
        {
            get { return (bool)GetValue(IsSelectedProperty); }
            set { SetCurrentValue(IsSelectedProperty, value); }
        }

        #endregion


    }
}
