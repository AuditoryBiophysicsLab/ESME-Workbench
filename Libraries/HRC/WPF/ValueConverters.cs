using System;
using System.Globalization;
using System.Reflection;
using System.Windows;
using System.Windows.Data;
using System.Windows.Media;
using System.Windows.Media.Animation;

namespace HRC.WPF
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

    public class NullToBooleanConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value == null) return false;
            var propertyInfo = value.GetType().GetProperty("Count");
            if (propertyInfo != null)
            {
                var count = (int)propertyInfo.GetValue(value, null);
                return count > 0;
            }
            if (!(value is bool)) return true;
            return value;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value;
        }
    }

    public class ObjectToBooleanConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value != null;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public class ObjectToVisibilityConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value == null ? Visibility.Hidden : Visibility.Visible;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }


    public class ProgressBarBrushConverter : IMultiValueConverter
    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            var type = typeof(double);
            if (((((values == null) || (values.Length != 5)) || ((values[0] == null) || (values[1] == null))) || (((values[2] == null) || (values[3] == null)) || ((values[4] == null) || !typeof(Brush).IsAssignableFrom(values[0].GetType())))) || ((!typeof(bool).IsAssignableFrom(values[1].GetType()) || !type.IsAssignableFrom(values[2].GetType())) || (!type.IsAssignableFrom(values[3].GetType()) || !type.IsAssignableFrom(values[4].GetType()))))
            {
                return null;
            }
            var brush = (Brush)values[0];
            var flag = (bool)values[1];
            var d = (double)values[2];
            var num2 = (double)values[3];
            var num3 = (double)values[4];
            if ((((d <= 0.0) || double.IsInfinity(d)) || (double.IsNaN(d) || (num2 <= 0.0))) || (double.IsInfinity(num2) || double.IsNaN(num2)))
            {
                return null;
            }
            var brush2 = new DrawingBrush();
            brush2.Viewport = brush2.Viewbox = new Rect(0.0, 0.0, d, num2);
            brush2.ViewportUnits = brush2.ViewboxUnits = BrushMappingMode.Absolute;
            brush2.TileMode = TileMode.None;
            brush2.Stretch = Stretch.None;
            var group = new DrawingGroup();
            var context = group.Open();
            var x = 0.0;
            const double width = 6.0;
            const double num6 = 2.0;
            const double num7 = width + num6;
            if (flag)
            {
                var num8 = (int)Math.Ceiling(d / num7);
                var num9 = -num8 * num7;
                var num10 = d * 0.3;
                brush2.Viewport = brush2.Viewbox = new Rect(num9, 0.0, num10 - num9, num2);
                var transform = new TranslateTransform();
                double num11 = num8 * 100;
                var animation = new DoubleAnimationUsingKeyFrames
                                {
                                    Duration = new Duration(TimeSpan.FromMilliseconds(num11)),
                                    RepeatBehavior = RepeatBehavior.Forever
                                };
                for (var i = 1; i <= num8; i++)
                {
                    var num13 = i * num7;
                    animation.KeyFrames.Add(new DiscreteDoubleKeyFrame(num13, KeyTime.Uniform));
                }
                transform.BeginAnimation(TranslateTransform.XProperty, animation);
                brush2.Transform = transform;
                while ((x + width) < num10)
                {
                    context.DrawRectangle(brush, null, new Rect(num9 + x, 0.0, width, num2));
                    x += num7;
                }
                d = num10;
                x = 0.0;
            }
            while ((x + width) < d)
            {
                context.DrawRectangle(brush, null, new Rect(x, 0.0, width, num2));
                x += num7;
            }
            var num14 = d - x;
            if ((!flag && (num14 > 0.0)) && (Math.Abs(d - num3) < 1E-05))
            {
                context.DrawRectangle(brush, null, new Rect(x, 0.0, num14, num2));
            }
            context.Close();
            brush2.Drawing = group;
            return brush2;
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
