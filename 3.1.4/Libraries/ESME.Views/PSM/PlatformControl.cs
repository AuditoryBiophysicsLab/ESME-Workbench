using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;

namespace ESME.Views.PSM
{
    class PlatformControl : Control
    {
        static PlatformControl() { DefaultStyleKeyProperty.OverrideMetadata(typeof(PlatformControl), new FrameworkPropertyMetadata(typeof(PlatformControl))); }

        public PlatformControl()
        {
            LayerNameContentControl = _textBlock;
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
            _textBlock.SetBinding(TextBlock.TextProperty, new Binding("LayerName") { Source = this });
            //SetCurrentValue(TheLayerControlProperty, this);
        }

        readonly TextBlock _textBlock = new TextBlock { VerticalAlignment = VerticalAlignment.Top };

        #region dependency property bool IsTreeViewItemSelected
        public static readonly DependencyProperty IsTreeViewItemSelectedProperty =
            DependencyProperty.Register("IsTreeViewItemSelected", typeof(bool), typeof(PlatformControl), new PropertyMetadata(default(bool)));

        public bool IsTreeViewItemSelected
        {
            get { return (bool)GetValue(IsTreeViewItemSelectedProperty); }
            set { SetValue(IsTreeViewItemSelectedProperty, value); }
        } 
        #endregion

        #region dependency property bool IsTreeViewItemExpanded
        public static readonly DependencyProperty IsTreeViewItemExpandedProperty =
            DependencyProperty.Register("IsTreeViewItemExpanded", typeof (bool), typeof (PlatformControl), new PropertyMetadata(default(bool)));

        public bool IsTreeViewItemExpanded
        {
            get { return (bool)GetValue(IsTreeViewItemExpandedProperty); }
            set { SetValue(IsTreeViewItemExpandedProperty, value); }
        }
        #endregion

        #region dependency property string LayerNameProperty
        public static readonly DependencyProperty LayerNameProperty =
            DependencyProperty.Register("LayerName", typeof(string), typeof(PlatformControl), new PropertyMetadata(default(string)));

        public string LayerName
        {
            get { return (string)GetValue(LayerNameProperty); }
            set { SetValue(LayerNameProperty, value); }
        } 
        #endregion

        #region dependency property FrameworkElement LayerNameContentControl
        public static readonly DependencyProperty LayerNameContentControlProperty =
            DependencyProperty.Register("LayerNameContentControl", typeof (FrameworkElement), typeof (PlatformControl), new PropertyMetadata(default(FrameworkElement)));

        public FrameworkElement LayerNameContentControl
        {
            get { return (FrameworkElement)GetValue(LayerNameContentControlProperty); }
            set { SetValue(LayerNameContentControlProperty, value); }
        }
        #endregion
    }
}
