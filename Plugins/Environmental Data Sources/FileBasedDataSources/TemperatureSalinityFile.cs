using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using FileBasedDataSources.Controls;
using HRC.Navigation;
using HRC.Utility;

namespace FileBasedDataSources
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.SoundSpeed,
        Name = "Temperature and salinity file",
        Description = "Text file containing temperature and salinity data")]
    public sealed class TemperatureSalinityFile : FileBasedDataSource<SoundSpeed>
    {
        public TemperatureSalinityFile()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The temperature and salinity file";
            ControlCaption = "Temperature and salinity file";
            DialogTitle = "Please locate the temperature and salinity file";
            FilenameFilter = "Temperature and salinity files (*.tmpsal)|*.tmpsal|All files (*.*)|*.*";

            ConfigurationControl = new SoundspeedFileSelectionControl { DataContext = this };
            IsTimeVariantData = true;
            IsSelectable = true;
        }

        [XmlIgnore]
        public override string Xml
        {
            get { return new XmlSerializer<TemperatureSalinityFile> { Data = this }.SaveToXml(); }
            set
            {
                var settings = XmlSerializer<TemperatureSalinityFile>.LoadFromXml(value);
                DataLocation = settings.DataLocation;
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<TemperatureSalinityFile> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<TemperatureSalinityFile>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            DataLocation = settings.DataLocation;
        }

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            var soundSpeed = new SoundSpeed();
            soundSpeed.Add(new SoundSpeedField { TimePeriod = timePeriod });
            using (var reader = new StreamReader(new FileStream(DataLocation, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var lineNumber = 0;
                while (true)
                {
                    var fields = ReadFieldsFromLine(reader, ref lineNumber);
                    if (fields == null) break;

                    if (fields.Length != 2) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Expected two comma-separated fields on this line", lineNumber));

                    var profile = new SoundSpeedProfile(ParseGeo(fields[0], fields[1], "Temperature and salinity file", lineNumber));

                    // Here, we read depth/speed pairs until we get an empty line
                    while (true)
                    {
                        fields = ReadFieldsFromLine(reader, ref lineNumber);
                        if (fields == null || fields.Length == 0) break;

                        if (fields.Length != 3) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Expected three comma-separated fields on this line", lineNumber));

                        float depth;
                        if (!float.TryParse(fields[0], out depth)) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Depth field is not a floating point number", lineNumber));
                        if (float.IsNaN(depth) || depth < 0 || depth > 11000) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Depth must be between 0 and 11000", lineNumber));

                        float temperature;
                        if (!float.TryParse(fields[1], out temperature)) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Speed field is not a floating point number", lineNumber));
                        if (float.IsNaN(temperature) || temperature < -20 || temperature > 100) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Temperature must be between -20 and 100", lineNumber));

                        float salinity;
                        if (!float.TryParse(fields[1], out salinity)) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Speed field is not a floating point number", lineNumber));
                        if (float.IsNaN(salinity) || salinity < 0 || salinity > 100) throw new FormatException(string.Format("Temperature and salinity file was not in the expected format at line {0}: Salinity must be between 0 and 100", lineNumber));

                        profile.Add(new SoundSpeedSample(depth, ChenMilleroLi.SoundSpeed(profile, depth, temperature, salinity)));
                    }
                    if (geoRect.Contains(profile)) soundSpeed[timePeriod].EnvironmentData.Add(profile);
                }
            }
            return soundSpeed;
        }
    }
}