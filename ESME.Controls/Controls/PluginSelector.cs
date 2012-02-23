using System.Collections;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;

namespace ESME.Views.Controls
{
    public class PluginSelector : Control
    {
        #region dependency property string PluginTypeName

        public static DependencyProperty PluginTypeNameProperty = DependencyProperty.Register("PluginTypeName",
                                                                                              typeof (string),
                                                                                              typeof (PluginSelector));

        public string PluginTypeName
        {
            get { return (string)GetValue(PluginTypeNameProperty); }
            set { SetValue(PluginTypeNameProperty, value); }
        }

        #endregion

        #region dependency property IESMEPlugin SelectedPlugin

        public static DependencyProperty SelectedPluginProperty = DependencyProperty.Register("SelectedPlugin",
                                                                                 typeof (IESMEPlugin),
                                                                                 typeof (PluginSelector),
                                                                                 new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.NotDataBindable));

        public IESMEPlugin SelectedPlugin
        {
            get { return (IESMEPlugin)GetValue(SelectedPluginProperty); }
            set { SetValue(SelectedPluginProperty, value); }
        }

        #endregion

        #region dependency property IEnumerable ItemsSource

        public static DependencyProperty ItemsSourceProperty = DependencyProperty.Register("ItemsSource",
                                                                                           typeof(IEnumerable),
                                                                                           typeof(PluginSelector));

        public IEnumerable ItemsSource
        {
            get { return (IEnumerable)GetValue(ItemsSourceProperty); }
            set { SetValue(ItemsSourceProperty, value); }
        }

        #endregion

        #region dependency property object SelectedItem

        public static DependencyProperty SelectedItemProperty = DependencyProperty.Register("SelectedItem",
                                                                                            typeof(object),
                                                                                            typeof(PluginSelector),
                                                                                            new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnSelectedItemChanged));

        public object SelectedItem
        {
            get { return GetValue(SelectedItemProperty); }
            set { SetValue(SelectedItemProperty, value); }
        }

        static void OnSelectedItemChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var pluginSelector = (PluginSelector)d;
            pluginSelector.SelectedPlugin = e.NewValue == null ? null : ((KeyValuePair<string, IESMEPlugin>)e.NewValue).Value;
        }
        #endregion
    }
}
