using System;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
using System.ComponentModel;

namespace ESME.Model
{
    public class EnvironmentInformation : IDataErrorInfo
    {
        public string LocationName { get; set; }
        public string SoundSpeedFieldName { get; set; }

        /// <summary>
        /// Wind speed, in knots
        /// </summary>
        public float WindSpeed { get; set; }
        
        public SedimentType Sediment { get; set; }
        public SedimentType Basement { get; set; }
        [XmlIgnore]
        public SoundSpeedField SoundSpeedField { get; set; }
        [XmlIgnore]
        public Environment2DData Bathymetry { get; set; }

        public EnvironmentInformation(string locationName, string soundSpeedFieldName, float windSpeed)
            : this()
        {
            LocationName = locationName;
            SoundSpeedFieldName = soundSpeedFieldName;
            WindSpeed = windSpeed;
        }

        public EnvironmentInformation()
        {
            LocationName = SoundSpeedFieldName = null;
            WindSpeed = 0f;
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
            "WindSpeed", 
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
                case "WindSpeed":
                    error = ValidateWindSpeed();
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
            var locationPath = Path.Combine(Globals.UserLocationsFolder, LocationName + ".eeb");
            if (!File.Exists(locationPath))
                return Strings.EnvironmentInformation_location_not_found;
            else
            {
                DataFile df = DataFile.Open(locationPath);
                this.SoundSpeedFieldName = df.Layers["soundspeed"].Name;
            }
            return null;
        }

        string ValidateWindSpeed()
        {
            if ((WindSpeed < 0) || (WindSpeed > 250))
                return Strings.EnvironmentInformation_invalid_wind_speed;
            return null;
        }

        string ValidateSediment()
        {
            return Sediment == null ? Strings.EnvironmentInformation_sediment_empty : null;
        }

        static bool IsStringMissing(string value)
        {
            return ((String.IsNullOrEmpty(value)) || (value.Trim() == String.Empty));
        }
        #endregion // Validation
    }
}
