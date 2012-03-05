using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using Point = System.Windows.Point;

namespace HRC.Navigation
{
#if false
    public class EarthCoordinate3DTypeConverter : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof (string)) return true;
            if (sourceType == typeof (EarthCoordinate)) return true;
            if (sourceType == typeof(PointF)) return true;
            if (sourceType == typeof(Point)) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override bool CanConvertTo(ITypeDescriptorContext context, Type destinationType)
        {
            if (destinationType == typeof(string)) return true;
            if (destinationType == typeof(EarthCoordinate3D)) return true;
            if (destinationType == typeof(PointF)) return true;
            if (destinationType == typeof(Point)) return true;

            return base.CanConvertTo(context, destinationType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            if (value is string)
            {
                var values = ((string) value).Split(new[]
                                                         {
                                                             '(', ')', ',', ' '
                                                         }, StringSplitOptions.RemoveEmptyEntries);

                double latitude, longitude, elevation;
                return ParseInto(values, out latitude, out longitude, out elevation) ? new EarthCoordinate3D(latitude, longitude, elevation) : null;
            }
            if (value is EarthCoordinate) return new EarthCoordinate3D(((EarthCoordinate) value).Latitude, ((EarthCoordinate) value).Longitude, 0);
            if (value is PointF) return new EarthCoordinate3D(((PointF)value).Y, ((PointF)value).X, 0);
            if (value is Point) return new EarthCoordinate3D(((Point)value).Y, ((Point)value).X, 0);

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            var source = (EarthCoordinate)value;
            if (destinationType == typeof (string)) return "(" + source.Latitude.ToString("##0.00##") + ", " + source.Longitude.ToString("##0.00##") + ")";
            if (destinationType == typeof (EarthCoordinate)) return new EarthCoordinate(source);
            if (destinationType == typeof(PointF)) return new PointF((float)source.Longitude, (float)source.Latitude);
            if (destinationType == typeof(Point)) return new Point(source.Longitude, source.Latitude);

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetPropertiesSupported(ITypeDescriptorContext context) { return true; }

        public override PropertyDescriptorCollection GetProperties(ITypeDescriptorContext context, object value, Attribute[] attributes)
        {
            return new PropertyDescriptorCollection(new PropertyDescriptor[]
                                                    {
                                                        new CoordinateProperty(typeof (EarthCoordinate), "Latitude", typeof (double)), new CoordinateProperty(typeof (EarthCoordinate), "Longitude", typeof (double)), new CoordinateProperty(typeof (EarthCoordinate), "Elevation", typeof (double))
                                                    });
        }

        public override bool IsValid(ITypeDescriptorContext context, object value)
        {
            if (value is string)
            {
                var values = ((string) value).Split(new[]
                                                         {
                                                             '(', ')', ',', ' '
                                                         }, StringSplitOptions.RemoveEmptyEntries);
                double latitude, longitude, elevation;
                return ParseInto(values, out latitude, out longitude, out elevation);
            }
            if (value is EarthCoordinate) return true;
            if (value is PointF) return true;
            if (value is Point) return true;
            return base.IsValid(context, value);
        }

        static bool ParseInto(string[] values, out double latitude, out double longitude, out double elevation)
        {
            latitude = longitude = elevation = double.NaN;

            return values.Length == 3 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude) && double.TryParse(values[2], out elevation);
        }

        #region Nested type: CoordinateProperty

        protected class CoordinateProperty : SimplePropertyDescriptor
        {
            public CoordinateProperty(Type componentType, string propertyName, Type propertyType) : base(componentType, propertyName, propertyType) { }

            public override object GetValue(object component)
            {
                var ec = (EarthCoordinate3D)component;
                switch (base.Name)
                {
                    case "Latitude":
                        return ec.Latitude;
                    case "Longitude":
                        return ec.Longitude;
                    case "Elevation":
                        return ec.Elevation;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }

            public override void SetValue(object component, object value)
            {
                var ec = (EarthCoordinate3D)component;
                switch (base.Name)
                {
                    case "Latitude":
                        ec.Latitude = (double) value;
                        break;
                    case "Longitude":
                        ec.Longitude = (double) value;
                        break;
                    case "Elevation":
                        ec.Elevation = (double) value;
                        break;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }
        }

        #endregion
    }
#endif
}