using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using System.Globalization;
using System.Drawing;

namespace HRC.Navigation
{
    public class EarthCoordinateTypeConverter : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string))
                return true;
            if (sourceType == typeof(EarthCoordinate3D))
                return true;
            if (sourceType == typeof(PointF))
                return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            if (value is string)
            {
                string[] values = ((string)value).Split(new char[] { '(', ')',',', ' ' }, StringSplitOptions.RemoveEmptyEntries);
                
                double latitude, longitude;
                if (values.Length == 2 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude))
                {
                    EarthCoordinate earthCoordinate = new EarthCoordinate(latitude, longitude);
                    return (object)earthCoordinate;
                }
                return null;
            }
            else if (value is EarthCoordinate3D)
                return new EarthCoordinate(((EarthCoordinate)value).Latitude_degrees, ((EarthCoordinate)value).Longitude_degrees);
            else if (value is PointF)
                return new EarthCoordinate(((PointF)value).Y, ((PointF)value).X);

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            EarthCoordinate source = value as EarthCoordinate;
            if (destinationType == typeof(string))
                return "(" + source.Latitude_degrees.ToString("##0.00##") + ", " + source.Longitude_degrees.ToString("##0.00##") + ")";
            else if (destinationType == typeof(EarthCoordinate3D))
                return new EarthCoordinate3D(source);
            else if (destinationType == typeof(PointF))
                return new PointF((float)source.Longitude_degrees, (float)source.Latitude_degrees);

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetPropertiesSupported(ITypeDescriptorContext context)
        {
            return true;
            //return base.GetPropertiesSupported(context);
        }

        public override PropertyDescriptorCollection GetProperties(ITypeDescriptorContext context, object value, Attribute[] attributes)
        {
            PropertyDescriptor latitudeProperty = new CoordinateProperty(typeof(EarthCoordinate), "Latitude", typeof(double));
            PropertyDescriptor longitudeProperty = new CoordinateProperty(typeof(EarthCoordinate), "Longitude", typeof(double));
            PropertyDescriptorCollection myProperties = new PropertyDescriptorCollection(new PropertyDescriptor[] { latitudeProperty, longitudeProperty });
            return myProperties;
            
            //return base.GetProperties(context, value, attributes);
        }

        public override bool IsValid(ITypeDescriptorContext context, object value)
        {
            if (value is string)
            {
                string[] values = ((string)value).Split(new char[] { '(', ')', ',', ' ' }, StringSplitOptions.RemoveEmptyEntries);

                double latitude, longitude;
                if (values.Length == 2 && double.TryParse(values[0], out latitude) && double.TryParse(values[1], out longitude))
                    return true;
                else
                    return false;
            }
            else if (value is EarthCoordinate3D)
                return true;
            else if (value is PointF)
                return true;
            return base.IsValid(context, value);
        }

        protected class CoordinateProperty : TypeConverter.SimplePropertyDescriptor
        {
            public CoordinateProperty(Type componentType, string propertyName, Type propertyType)
                : base(componentType, propertyName, propertyType)
            {

            }

            public override object GetValue(object component)
            {
                if (component == null)
                    return null;
                EarthCoordinate ec = component as EarthCoordinate;
                switch (base.Name)
                {
                    case "Latitude":
                        return ec.Latitude_degrees;
                    case "Longitude":
                        return ec.Longitude_degrees;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }

            public override void SetValue(object component, object value)
            {
                EarthCoordinate ec = component as EarthCoordinate;
                switch (base.Name)
                {
                    case "Latitude":
                        ec.Latitude_degrees = (double)value;
                        break;
                    case "Longitude":
                        ec.Longitude_degrees = (double)value;
                        break;
                    default:
                        throw new ArgumentException("EarthCoordinateTypeConverter.GetValue: Unknown property " + base.Name);
                }
            }
        }
    }
}
