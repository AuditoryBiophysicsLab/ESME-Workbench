using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;

namespace ESME.Environment.Descriptors
{
    public class DavesSelector : DataTemplateSelector
    {
        public DataTemplate SingleItemTemplate { get; set; }
        public DataTemplate MultiItemTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var items = (CollectionViewGroup)item;
            if (items.ItemCount == 1) return SingleItemTemplate;
            return MultiItemTemplate;
        }
    }

    public class DavesGroupSelector : DataTemplateSelector
    {
        public DataTemplate SingleItemTemplate { get; set; }
        public DataTemplate MultiItemTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var items = (CollectionViewGroup)item;
            if (items.ItemCount == 1) return SingleItemTemplate;
            return MultiItemTemplate;
        }
    }

    public class DavesConverter : IValueConverter
    {
        #region IValueConverter Members

        public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture) { return value; }

        public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {

            double passedValue = (double)value;
            double parameterValue = (double)parameter;
            return passedValue + parameterValue;
        }

        #endregion
    }
}
