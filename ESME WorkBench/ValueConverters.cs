using System;
using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace ESMEWorkBench
{
    [ValueConversion(typeof(double), typeof(GridLength))]
    public class GridLengthConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var result = new GridLength((double)value);
            return result;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var result = ((GridLength)value).Value;
            return result;
        }
    }
}
