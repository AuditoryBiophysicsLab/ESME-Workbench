using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;

namespace ESME.DataModel
{
    public static class ActivateModel
    {
        public static readonly DependencyProperty ModelProperty =
            DependencyProperty.RegisterAttached("Model", typeof(DataModelBase), typeof(ActivateModel),
            new PropertyMetadata(new PropertyChangedCallback(OnModelInvalidated)));

        public static DataModelBase GetModel(DependencyObject sender)
        {
            return (DataModelBase)sender.GetValue(ModelProperty);
        }

        public static void SetModel(DependencyObject sender, DataModelBase model)
        {
            sender.SetValue(ModelProperty, model);
        }

        private static void OnModelInvalidated(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs e)
        {
            FrameworkElement element = (FrameworkElement)dependencyObject;

            // Add handlers if necessary
            if (e.OldValue == null && e.NewValue != null)
            {
                element.Loaded += OnElementLoaded;
                element.Unloaded += OnElementUnloaded;
            }

            // Or, remove if necessary
            if (e.OldValue != null && e.NewValue == null)
            {
                element.Loaded -= OnElementLoaded;
                element.Unloaded -= OnElementUnloaded;
            }

            // If loaded, deactivate old model and activate new one
            if (element.IsLoaded)
            {
                if (e.OldValue != null)
                {
                    ((DataModelBase)e.OldValue).Deactivate();
                }

                if (e.NewValue != null)
                {
                    ((DataModelBase)e.NewValue).Activate();
                }
            }
        }
        static void OnElementLoaded(object sender, RoutedEventArgs e)
        {
            FrameworkElement element = (FrameworkElement)sender;
            DataModelBase model = GetModel(element);
            model.Activate();
        }

        static void OnElementUnloaded(object sender, RoutedEventArgs e)
        {
            FrameworkElement element = (FrameworkElement)sender;
            DataModelBase model = GetModel(element);
            model.Deactivate();
        }
    }
}
