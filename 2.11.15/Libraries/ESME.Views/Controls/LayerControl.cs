using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using HRC.WPF;

namespace ESME.Views.Controls
{
    public class LayerControl : Control
    {
        static LayerControl() { DefaultStyleKeyProperty.OverrideMetadata(typeof(LayerControl), new FrameworkPropertyMetadata(typeof(LayerControl))); }
        public LayerControl()
        {
            LayerNameContentControl = _textBlock;
            _textBlock.MouseLeftButtonDown += (s, e) => BeginEditIfEnabled();
            _textBox.MouseLeftButtonUp += (s, e) => SetFocus(false);
            _textBox.KeyDown += (s, e) =>
            {
                switch (e.Key)
                {
                    case Key.Enter:
                        EndEdit(false);
                        e.Handled = true;
                        break;
                    case Key.Escape:
                        EndEdit(true);
                        e.Handled = true;
                        break;
                }
            };
            BindingOperations.SetBinding(this,
                                         IsTreeViewItemSelectedProperty,
                                         new Binding("IsTreeViewItemSelected")
                                         {
                                             RelativeSource = new RelativeSource(RelativeSourceMode.FindAncestor, typeof(TreeViewItem), 1),
                                             Path = new PropertyPath("IsSelected"),
                                             Mode = BindingMode.TwoWay,
                                         });
            BindingOperations.SetBinding(this,
                                         IsTreeViewItemExpandedProperty,
                                         new Binding("IsTreeViewItemExpanded")
                                         {
                                             RelativeSource = new RelativeSource(RelativeSourceMode.FindAncestor, typeof(TreeViewItem), 1),
                                             Path = new PropertyPath("IsExpanded"),
                                             Mode = BindingMode.TwoWay,
                                         });
            BindingOperations.SetBinding(this,
                                         TreeViewItemProperty,
                                         new Binding { RelativeSource = new RelativeSource(RelativeSourceMode.FindAncestor, typeof(TreeViewItem), 1) });
            _textBox.SetBinding(TextBox.TextProperty, new Binding("LayerName") { Source = this, Mode = BindingMode.TwoWay });
            _textBlock.SetBinding(TextBlock.TextProperty, new Binding("LayerName") { Source = this });
            SetCurrentValue(TheLayerControlProperty, this);
        }

        readonly TextBox _textBox = new TextBox { VerticalAlignment = VerticalAlignment.Top, VerticalContentAlignment = VerticalAlignment.Top };
        readonly TextBlock _textBlock = new TextBlock();
        string _originalContent;

        void BeginEditIfEnabled()
        {
            if (!IsLayerNameEditable || !IsTreeViewItemSelected) return;
            BeginEdit();
        }
        void BeginEdit()
        {
            _originalContent = _textBlock.Text;
            LayerNameContentControl = _textBox;
            UpdateLayout();
        }
        void SetFocus(bool invokeIfRequired)
        {
            if (invokeIfRequired) _textBox.Dispatcher.InvokeIfRequired(() =>
            {
                _textBox.Focus();
                _textBox.SelectAll();
            }, DispatcherPriority.Input);
            else
            {
                _textBox.Focus();
                _textBox.SelectAll();
            }
        }
        void EndEdit(bool restoreOriginalContent)
        {
            if (restoreOriginalContent) _textBox.Text = _originalContent;
            IsTreeViewItemSelected = false;
            LayerNameContentControl = _textBlock;
        }
        static void OnPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            var layerControl = (LayerControl)sender;
            Debug.WriteLine("OnPropertyChanged: IsLayerNameEditable: {0}, IsTreeViewItemSelected: {1}", layerControl.IsLayerNameEditable, layerControl.IsTreeViewItemSelected );
            if (!layerControl.IsLayerNameEditable || !layerControl.IsTreeViewItemSelected) layerControl.LayerNameContentControl = layerControl._textBlock;
        }

        #region dependency property bool IsMapLayer
        public static DependencyProperty IsMapLayerProperty = DependencyProperty.Register("IsMapLayer",
                                                                                          typeof(bool),
                                                                                          typeof(LayerControl),
                                                                                          new FrameworkPropertyMetadata(true,
                                                                                                                        FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                        IsMapLayerPropertyChanged));

        static void IsMapLayerPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            var layerControl = (LayerControl)sender;
            var isMapLayer = (bool)args.NewValue;
            //Debug.WriteLine("{0}: IsMapLayerPropertyChanged: LayerName = {1}, IsChecked(old) = {2}, IsChecked(new) = {3}",
            //                DateTime.Now,
            //                layerControl.LayerName,
            //                (bool)args.OldValue,
            //                (bool)args.NewValue);
            layerControl.CheckBoxVisibility = isMapLayer ? Visibility.Visible : Visibility.Collapsed;
            layerControl.LineColorVisibility = isMapLayer ? Visibility.Visible : Visibility.Collapsed;
        }

        public bool IsMapLayer { get { return (bool)GetValue(IsMapLayerProperty); } set { SetCurrentValue(IsMapLayerProperty, value); } }
        #endregion

        #region dependency property bool IsChecked
        public static DependencyProperty IsCheckedProperty = DependencyProperty.Register("IsChecked",
                                                                                         typeof(bool),
                                                                                         typeof(LayerControl),
                                                                                         new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool IsChecked { get { return (bool)GetValue(IsCheckedProperty); } set { SetCurrentValue(IsCheckedProperty, value); } }
        #endregion

        #region dependency property Color LineOrSymbolColor
        public static DependencyProperty LineOrSymbolColorProperty = DependencyProperty.Register("LineOrSymbolColor",
                                                                                                 typeof(Color),
                                                                                                 typeof(LayerControl),
                                                                                                 new FrameworkPropertyMetadata(Colors.Red, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Color LineOrSymbolColor { get { return (Color)GetValue(LineOrSymbolColorProperty); } set { SetCurrentValue(LineOrSymbolColorProperty, value); } }
        #endregion

        #region dependency property double LineOrSymbolSize
        public static DependencyProperty LineOrSymbolSizeProperty = DependencyProperty.Register("LineOrSymbolSize",
                                                                                                typeof(double),
                                                                                                typeof(LayerControl),
                                                                                                new FrameworkPropertyMetadata(1.0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public double LineOrSymbolSize { get { return (double)GetValue(LineOrSymbolSizeProperty); } set { SetCurrentValue(LineOrSymbolSizeProperty, value); } }
        #endregion

        #region dependency property string LayerName
        public static DependencyProperty LayerNameProperty = DependencyProperty.Register("LayerName",
                                                                                         typeof(string),
                                                                                         typeof(LayerControl));

        public string LayerName { get { return (string)GetValue(LayerNameProperty); } set { SetCurrentValue(LayerNameProperty, value); } }
        #endregion

        #region dependency property Visibility CheckBoxVisibility
        public static DependencyProperty CheckBoxVisibilityProperty = DependencyProperty.Register("CheckBoxVisibility",
                                                                                                  typeof(Visibility),
                                                                                                  typeof(LayerControl),
                                                                                                  new FrameworkPropertyMetadata(Visibility.Visible,
                                                                                                                                FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Visibility CheckBoxVisibility { get { return (Visibility)GetValue(CheckBoxVisibilityProperty); } set { SetCurrentValue(CheckBoxVisibilityProperty, value); } }
        #endregion

        #region dependency property Visibility LineColorVisibility
        public static DependencyProperty LineColorVisibilityProperty = DependencyProperty.Register("LineColorVisibility",
                                                                                                   typeof(Visibility),
                                                                                                   typeof(LayerControl),
                                                                                                   new FrameworkPropertyMetadata(Visibility.Visible,
                                                                                                                                 FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public Visibility LineColorVisibility { get { return (Visibility)GetValue(LineColorVisibilityProperty); } set { SetCurrentValue(LineColorVisibilityProperty, value); } }
        #endregion

        #region dependency property bool IsSelected

        public static DependencyProperty IsSelectedProperty = DependencyProperty.Register("IsSelected", typeof(bool), typeof(LayerControl), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, IsSelectedPropertyChanged));
        static void IsSelectedPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            var layerControl = (LayerControl)sender;
            layerControl.IsTreeViewItemSelected = layerControl.IsSelected;
        }

        public bool IsSelected { get { return (bool)GetValue(IsSelectedProperty); } set { SetValue(IsSelectedProperty, value); } }

        #endregion

        #region dependency property bool IsTreeViewItemSelected
        public static DependencyProperty IsTreeViewItemSelectedProperty = DependencyProperty.Register("IsTreeViewItemSelected",
                                                                                                      typeof(bool),
                                                                                                      typeof(LayerControl),
                                                                                                      new FrameworkPropertyMetadata(false,
                                                                                                                                    FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                                    OnPropertyChanged));

        public bool IsTreeViewItemSelected { get { return (bool)GetValue(IsTreeViewItemSelectedProperty); } set { SetCurrentValue(IsTreeViewItemSelectedProperty, value); } }
        #endregion

        #region dependency property bool IsExpanded

        public static DependencyProperty IsExpandedProperty = DependencyProperty.Register("IsExpanded", typeof(bool), typeof(LayerControl), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, IsExpandedPropertyChanged));
        static void IsExpandedPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            var layerControl = (LayerControl)sender;
            layerControl.IsTreeViewItemExpanded = layerControl.IsExpanded;
        }

        public bool IsExpanded { get { return (bool)GetValue(IsExpandedProperty); } set { SetValue(IsExpandedProperty, value); } }

        #endregion

        #region dependency property bool IsTreeViewItemExpanded

        public static DependencyProperty IsTreeViewItemExpandedProperty = DependencyProperty.Register("IsTreeViewItemExpanded", typeof(bool), typeof(LayerControl), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool IsTreeViewItemExpanded { get { return (bool)GetValue(IsTreeViewItemExpandedProperty); } set { SetValue(IsTreeViewItemExpandedProperty, value); } }

        #endregion

        #region dependency property bool IsLayerNameEditable
        public static DependencyProperty IsLayerNameEditableProperty = DependencyProperty.Register("IsLayerNameEditable",
                                                                                                   typeof(bool),
                                                                                                   typeof(LayerControl),
                                                                                                   new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnPropertyChanged));

        public bool IsLayerNameEditable { get { return (bool)GetValue(IsLayerNameEditableProperty); } set { SetValue(IsLayerNameEditableProperty, value); } }
        #endregion

        #region dependency property FrameworkElement LayerNameContentControl
        public static DependencyProperty LayerNameContentControlProperty = DependencyProperty.Register("LayerNameContentControl",
                                                                                                       typeof(FrameworkElement),
                                                                                                       typeof(LayerControl));

        public FrameworkElement LayerNameContentControl { get { return (FrameworkElement)GetValue(LayerNameContentControlProperty); } set { SetValue(LayerNameContentControlProperty, value); } }
        #endregion

        #region dependency property TreeViewItem TreeViewItem

        public static DependencyProperty TreeViewItemProperty = DependencyProperty.Register("TreeViewItem", typeof(TreeViewItem), typeof(LayerControl), new FrameworkPropertyMetadata(null, OnTreeViewItemPropertyChanged));
        static void OnTreeViewItemPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            var layerControl = (LayerControl)sender;
            if (layerControl.TreeViewItem != null) layerControl.TreeViewItem.KeyDown += (s, e) =>
            {
                switch (e.Key)
                {
                    case Key.F2:
                        layerControl.BeginEditIfEnabled();
                        layerControl.SetFocus(true);
                        e.Handled = true;
                        break;
                    case Key.Delete:
                        break;
                }
            };
        }

        public TreeViewItem TreeViewItem { get { return (TreeViewItem)GetValue(TreeViewItemProperty); } set { SetValue(TreeViewItemProperty, value); } }

        #endregion

        #region dependency property Control TheLayerControl

        public static DependencyProperty TheLayerControlProperty = DependencyProperty.Register("TheLayerControl", typeof(Control), typeof(LayerControl), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, TheLayerControlPropertyChanged));
        static void TheLayerControlPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            //Debug.WriteLine("TheLayerControlPropertyChanged");
            var layerControl = (LayerControl)sender;
            if (layerControl.TheLayerControl != layerControl)
            {
                //Debug.WriteLine("TheLayerControlPropertyChanged, restoring proper value");
                layerControl.TheLayerControl = layerControl;
            }
        }
        public void Expand() { if (TreeViewItem != null) TreeViewItem.IsExpanded = true; }
        public void Select() { if (TreeViewItem != null) TreeViewItem.IsSelected = true; }
        public void Edit()
        {
            BeginEdit();
            SetFocus(true);
        }

        public Control TheLayerControl { get { return (Control)GetValue(TheLayerControlProperty); } set { SetValue(TheLayerControlProperty, value); } }

        #endregion
    }
}