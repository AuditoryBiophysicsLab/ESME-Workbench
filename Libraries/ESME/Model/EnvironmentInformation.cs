using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Xml.Serialization;
using HRC.Navigation;
using ESME.Environment;
using System.ComponentModel;

namespace ESME.Model
{
    public class EnvironmentInformation : IDataErrorInfo
    {
        public string LocationName { get; set; }
        public string SoundSpeedFieldName { get; set; }
        public float WindSpeed_knots { get; set; }
        public SedimentType Sediment { get; set; }
        public SedimentType Basement { get; set; }
        [XmlIgnore]
        public SoundSpeedField SoundSpeedField { get; set; }
        [XmlIgnore]
        public Bathymetry Bathymetry { get; set; }

        public EnvironmentInformation(string LocationName, string SoundSpeedFieldName, float WindSpeed_knots)
            : this()
        {
            this.LocationName = LocationName;
            this.SoundSpeedFieldName = SoundSpeedFieldName;
            this.WindSpeed_knots = WindSpeed_knots;
        }

        public EnvironmentInformation()
        {
            LocationName = SoundSpeedFieldName = null;
            WindSpeed_knots = 0f;
            Sediment = Basement = null;
            SoundSpeedField = null;
            Bathymetry = null;
        }

        #region IDataErrorInfo Members

        string IDataErrorInfo.Error { get { return null; } }

        string IDataErrorInfo.this[string propertyName]
        {
            get { return this.GetValidationError(propertyName); }
        }

        #endregion // IDataErrorInfo Members

        #region Validation


        /// <summary>
        /// Returns true if this object has no validation errors.
        /// </summary>
        public bool IsValid
        {
            get
            {
                foreach (string property in ValidatedProperties)
                    if (GetValidationError(property) != null)
                        return false;

                return true;
            }
        }

        static readonly string[] ValidatedProperties = 
        { 
            "LocationName", 
            "WindSpeed_knots", 
            "Sediment",
        };

        string GetValidationError(string propertyName)
        {
            if (Array.IndexOf(ValidatedProperties, propertyName) < 0)
                return null;

            string error = null;

            switch (propertyName)
            {
                case "LocationName":
                    error = ValidateLocationName();
                    break;
                case "WindSpeed_knots":
                    error = ValidateWindSpeed_knots();
                    break;
                case "Sediment":
                    error = ValidateSediment();
                    break;
                default:
                    System.Diagnostics.Debug.Fail("Unexpected property being validated on EnvironmentInformation: " + propertyName);
                    break;
            }

            return error;
        }

        string ValidateLocationName()
        {
            if (IsStringMissing(LocationName))
                return Strings.EnvironmentInformation_location_empty;
            string LocationPath = Path.Combine(Globals.UserLocationsFolder, LocationName + ".eeb");
            if (!File.Exists(LocationPath))
                return Strings.EnvironmentInformation_location_not_found;
            else
            {
                DataFile df = DataFile.Open(LocationPath);
                this.SoundSpeedFieldName = df.Layers["soundspeed"].Name;
            }
            return null;
        }

        string ValidateWindSpeed_knots()
        {
            if ((WindSpeed_knots < 0) || (WindSpeed_knots > 250))
                return Strings.EnvironmentInformation_invalid_wind_speed;
            return null;
        }

        string ValidateSediment()
        {
            if (Sediment == null)
                return Strings.EnvironmentInformation_sediment_empty;
            return null;
        }

        static bool IsStringMissing(string value)
        {
            return ((String.IsNullOrEmpty(value)) || (value.Trim() == String.Empty));
        }
        #endregion // Validation
    }
}
