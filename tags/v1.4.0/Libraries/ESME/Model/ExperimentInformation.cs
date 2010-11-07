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
using ESME.NEMO;
using System.ComponentModel;
using System.Diagnostics;
 
namespace ESME.Model
{
    public class ExperimentInformation : IDataErrorInfo
    {
        #region State Properties
        public string Name { get; set; }
        public string Description { get; set; }
        public string Author { get; set; }
        public DateTime Created { get; set; }
        public DateTime Modified { get; set; }
        public string NemoFileName { get; set; }
        public string NemoDataDirectory { get; set; }
        [XmlIgnore]
        public NemoFile NemoFile { get; private set; }

        [XmlIgnore]
        public string ExperimentDirectory { get { return Path.GetDirectoryName(Globals.FindExperimentFile(Name)); } }
        [XmlIgnore]
        public string ExperimentPath { get { return Globals.FindExperimentFile(Name); } }

        #endregion

        public ExperimentInformation()
        {
            Created = DateTime.Now;
        }

        internal void Initialize()
        {
            if ((NemoFileName != null) && (NemoDataDirectory != null))
                NemoFile = new NemoFile(NemoFileName, NemoDataDirectory);
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
            "Name", 
            "Description", 
            "Author",
            "NemoFileName",
            "NemoDataDirectory",
        };

        string GetValidationError(string propertyName)
        {
            if (Array.IndexOf(ValidatedProperties, propertyName) < 0)
                return null;

            string error = null;

            switch (propertyName)
            {
                case "Name":
                    error = ValidateName();
                    break;
                case "Description":
                    error = ValidateDescription();
                    break;
                case "Author":
                    error = ValidateAuthor();
                    break;
                case "NemoFileName":
                    error = ValidateNemoFileName();
                    break;
                case "NemoDataDirectory":
                    error = ValidateNemoDataDirectory();
                    break;
                default:
                    Debug.Fail("Unexpected property being validated on ExperimentInformation: " + propertyName);
                    break;
            }

            return error;
        }

        string ValidateName()
        {
            if (IsStringMissing(Name))
                return Strings.ExperimentInformation_missing_name;
            else if (this.Name.IndexOfAny(Path.GetInvalidFileNameChars()) > 0)
                return Strings.ExperimentInformation_invalid_name;
            else if (Directory.Exists(ExperimentDirectory))
                return Strings.ExperimentInformation_experiment_exists;
            else if (File.Exists(ExperimentPath))
                return Strings.ExperimentInformation_experiment_exists;

            return null;
        }

        string ValidateDescription()
        {
            if (IsStringMissing(Description))
                return Strings.ExperimentInformation_missing_description;
            return null;
        }

        string ValidateAuthor()
        {
            if (IsStringMissing(Author))
                return Strings.ExperimentInformation_missing_author;
            return null;
        }

        string ValidateNemoFileName()
        {
            if (!IsStringMissing(NemoFileName) && (this.NemoFileName.IndexOfAny(Path.GetInvalidPathChars()) > 0))
                return Strings.ExperimentInformation_invalid_nemo_filename;
            return null;
        }

        string ValidateNemoDataDirectory()
        {
            if (!IsStringMissing(NemoDataDirectory) && (this.NemoDataDirectory.IndexOfAny(Path.GetInvalidPathChars()) > 0))
                return Strings.ExperimentInformation_invalid_nemo_data_directory;
            if (!IsStringMissing(NemoFileName) && IsStringMissing(NemoDataDirectory))
                return Strings.ExperimentInformation_nemo_data_directory_required;
            return null;
        }

        static bool IsStringMissing(string value)
        {
            return ((String.IsNullOrEmpty(value)) || (value.Trim() == String.Empty));
        }
        #endregion // Validation
    }
}
