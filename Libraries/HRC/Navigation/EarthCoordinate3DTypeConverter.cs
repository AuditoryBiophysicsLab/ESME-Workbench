using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using System.Globalization;
using System.Drawing;

namespace HRC.Navigation
{
    public class EarthCoordinate3DTypeConverter : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string))
                return true;
            if (sourceType == typeof(EarthCoordinate))
                return true;
            if (sourceType == typeof(PointF))
                return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            if (value is string)
            {
                string[] values = ((string)value).Split(new char[] { '(', ')', ',', ' ' }, StringSplitOptions.RemoveEmptyEntries);

                double latitude, longitude, elevation;
                if (ParseInto(values, out latitude, out longitude, out elevation))
                    return new EarthCoordinate3D(latitude, longitude, elevation);
                return null;
            }
            else if (value is EarthCoordinate)
                return new EarthCoordinate3D(((EarthCoordinate)value).Latitude_degrees, ((EarthCoordinate)value).Longitude_degrees, 0);
            else if (value is PointF)
                return new EarthCoordinate3D(((PointF)value).Y, ((PointF)value).X, 0);

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            EarthCoordinate source = value as EarthCoordinate;
            if (destinationType == typeof(string))
                return "(" + source.Latitude_degrees.ToString("##0.00##") + ", " + source.Longitude_degrees.ToString("##0.00##") + ")";
            else if (destinationType == typeof(EarthCoordinate))
                return new EarthCoordinate(source);
            else if (destinationType == typeof(PointF))
                return new PointF((float)source.Longitude_degrees, (float)source.Latitude_degrees);

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetPropertiesSupported(ITypeDescriptorContext context)
        {
            return true;
        }

        public override PropertyDescriptorCollection GetProperties(ITypeDescriptorContext context, object value, Attribute[] attributes)
        {
            return new PropertyDescriptorCollection(new PropertyDescriptor[] 
                { 
                    new CoordinateProperty(typeof(EarthCoordinate), "Latitude", typeof(double)),
                    new CoordinateProperty(typeof(EarthCoordinate), "Longitude", typeof(double)), 
                    new CoordinateProperty(typeof(EarthCoordinate), "Elevation", typeof(double)) 
                });
        }

        public override bool IsValid(ITypeDescriptorContext context, object value)
        {
            if (value is string)
            {
                string[] values = ((string)value).Split(new char[] { '(', ')', ',', ' ' }, StringSplitOptions.RemoveEmptyEntries);
                double latitude, longitude, elevation;
                return ParseInto(values, out latitude, out longitude, out elevation);
            }
            else if (value is EarthCoordinate)
                return true;
            else if (value is PointF)
                return true;
            return base.IsValid(context, value);
        }

        private bool ParseInto(string[] values, out double latitude, out double longitude, out double elevation)
        {
            latitude = longitude = elevation = double.NaN;

            if (values.Length == 3 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude) && double.TryParse(values[2], out elevation))
                return true;
            return false;
        }

        protected class CoordinateProperty : TypeConverter.SimplePropertyDescriptor
        {
            public CoordinateProperty(Type componentType, string propertyName, Type propertyType)
                : base(componentType, propertyName, propertyType)
            {

            }

            public override object GetValue(object component)
            {
                EarthCoordinate3D ec = component as EarthCoordinate3D;
                switch (base.Name)
                {
                    case "Latitude":
                        return ec.Latitude_degrees;
                    case "Longitude":
                        return ec.Longitude_degrees;
                    case "Elevation":
                        return ec.Elevation_meters;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }

            public override void SetValue(object component, object value)
            {
                EarthCoordinate3D ec = component as EarthCoordinate3D;
                switch (base.Name)
                {
                    case "Latitude":
                        ec.Latitude_degrees = (double)value;
                        break;
                    case "Longitude":
                        ec.Longitude_degrees = (double)value;
                        break;
                    case "Elevation":
                        ec.Elevation_meters = (double)value;
                        break;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }
        }
    }
}
