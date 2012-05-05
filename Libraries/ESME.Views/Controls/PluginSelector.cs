using System.Collections;
using System.Windows;
using System.Windows.Controls;

namespace ESME.Views.Controls
{
    public class PluginSelector : Control
    {
        static PluginSelector() { DefaultStyleKeyProperty.OverrideMetadata(typeof(PluginSelector), new FrameworkPropertyMetadata(typeof(PluginSelector))); }

        #region dependency property string PluginTypeName
        public static DependencyProperty PluginTypeNameProperty = DependencyProperty.Register("PluginTypeName", typeof(string), typeof(PluginSelector));

        public string PluginTypeName { get { return (string)GetValue(PluginTypeNameProperty); } set { SetCurrentValue(PluginTypeNameProperty, value); } }
        #endregion

        #region dependency property IESMEPlugin SelectedPlugin
        public static DependencyProperty SelectedPluginProperty = DependencyProperty.Register("SelectedPlugin",
                                                                                              typeof(IESMEPlugin),
                                                                                              typeof(PluginSelector),
                                                                                              new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.NotDataBindable));

        public IESMEPlugin SelectedPlugin { get { return (IESMEPlugin)GetValue(SelectedPluginProperty); } set { SetCurrentValue(SelectedPluginProperty, value); } }
        #endregion

        #region dependency property IEnumerable ItemsSource
        public static DependencyProperty ItemsSourceProperty = DependencyProperty.Register("ItemsSource", typeof(IEnumerable), typeof(PluginSelector));

        public IEnumerable ItemsSource { get { return (IEnumerable)GetValue(ItemsSourceProperty); } set { SetCurrentValue(ItemsSourceProperty, value); } }
        #endregion

        #region dependency property object SelectedItem
        public static DependencyProperty SelectedItemProperty = DependencyProperty.Register("SelectedItem",
                                                                                            typeof(object),
                                                                                            typeof(PluginSelector),
                                                                                            new FrameworkPropertyMetadata(null,
                                                                                                                          FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                                                                                                                          OnSelectedItemChanged));

        public object SelectedItem { get { return GetValue(SelectedItemProperty); } set { SetCurrentValue(SelectedItemProperty, value); } }

        static void OnSelectedItemChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var pluginSelector = (PluginSelector)d;
            pluginSelector.SelectedPlugin = e.NewValue == null ? null : (IESMEPlugin)e.NewValue;
        }
        #endregion

        #region dependency property SelectionMode SelectionMode
        public static DependencyProperty SelectionModeProperty = DependencyProperty.Register("SelectionMode",
                                                                                             typeof(SelectionMode),
                                                                                             typeof(PluginSelector),
                                                                                             new FrameworkPropertyMetadata(SelectionMode.Single));

        public SelectionMode SelectionMode { get { return (SelectionMode)GetValue(SelectionModeProperty); } set { SetValue(SelectionModeProperty, value); } }
        #endregion
    }

    public enum SelectionMode
    {
        Single,
        Multiple
    }
}