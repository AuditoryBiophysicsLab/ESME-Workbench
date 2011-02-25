using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Windows;
using Point = System.Windows.Point;

namespace HRC.Navigation
{
    public class EarthCoordinateTypeConverter : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string)) return true;
            if (sourceType == typeof(EarthCoordinate3D)) return true;
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

                double latitude, longitude;
                if (values.Length == 2 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude))
                {
                    var earthCoordinate = new EarthCoordinate(latitude, longitude);
                    return earthCoordinate;
                }
                return null;
            }
            if (value is EarthCoordinate3D) return new EarthCoordinate(((EarthCoordinate) value).Latitude, ((EarthCoordinate) value).Longitude);
            if (value is PointF) return new EarthCoordinate(((PointF)value).Y, ((PointF)value).X);
            if (value is Point) return new EarthCoordinate(((Point)value).Y, ((Point)value).X);

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            var source = (EarthCoordinate) value;
            if (destinationType == typeof (string)) return "(" + source.Latitude.ToString("##0.00##") + ", " + source.Longitude.ToString("##0.00##") + ")";
            if (destinationType == typeof (EarthCoordinate3D)) return new EarthCoordinate3D(source);
            if (destinationType == typeof (PointF)) return new PointF((float) source.Longitude, (float) source.Latitude);
            if (destinationType == typeof(Point)) return new Point(source.Longitude, source.Latitude);
            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetPropertiesSupported(ITypeDescriptorContext context)
        {
            return true;
            //return base.GetPropertiesSupported(context);
        }

        public override PropertyDescriptorCollection GetProperties(ITypeDescriptorContext context, object value, Attribute[] attributes)
        {
            PropertyDescriptor latitudeProperty = new CoordinateProperty(typeof (EarthCoordinate), "Latitude", typeof (double));
            PropertyDescriptor longitudeProperty = new CoordinateProperty(typeof (EarthCoordinate), "Longitude", typeof (double));
            var myProperties = new PropertyDescriptorCollection(new[]
                                                                {
                                                                    latitudeProperty, longitudeProperty
                                                                });
            return myProperties;

            //return base.GetProperties(context, value, attributes);
        }

        public override bool IsValid(ITypeDescriptorContext context, object value)
        {
            if (value is string)
            {
                var values = ((string) value).Split(new[]
                                                         {
                                                             '(', ')', ',', ' '
                                                         }, StringSplitOptions.RemoveEmptyEntries);

                double latitude, longitude;
                return values.Length == 2 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude);
            }
            if (value is EarthCoordinate3D) return true;
            if (value is PointF) return true;
            if (value is Point) return true;
            return base.IsValid(context, value);
        }

        #region Nested type: CoordinateProperty

        protected class CoordinateProperty : SimplePropertyDescriptor
        {
            public CoordinateProperty(Type componentType, string propertyName, Type propertyType) : base(componentType, propertyName, propertyType) { }

            public override object GetValue(object component)
            {
                if (component == null) return null;
                var ec = (EarthCoordinate) component;
                switch (base.Name)
                {
                    case "Latitude":
                        return ec.Latitude;
                    case "Longitude":
                        return ec.Longitude;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }

            public override void SetValue(object component, object value)
            {
                var ec = (EarthCoordinate)component;
                switch (base.Name)
                {
                    case "Latitude":
                        ec.Latitude = (double) value;
                        break;
                    case "Longitude":
                        ec.Longitude = (double) value;
                        break;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }
        }

        #endregion
    }
}