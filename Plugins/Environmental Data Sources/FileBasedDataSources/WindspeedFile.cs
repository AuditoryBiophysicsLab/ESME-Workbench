using System;
using System.ComponentModel.Composition;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Plugins;
using FileBasedDataSources.Controls;
using HRC.Navigation;
using HRC.Utility;

namespace FileBasedDataSources
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Wind,
                           Name = "Wind speed file",
                           Description = "Text file containing latitude/longitude/windspeed tuples for the appropriate time period")]
    public sealed class WindspeedFile : FileBasedDataSource<Wind>
    {
        public WindspeedFile()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The wind speed file";
            ControlCaption = "Wind speed file";
            DialogTitle = "Please locate the wind speed file";
            FilenameFilter = "Wind file (*.wind)|*.wind|All files (*.*)|*.*";
            ConfigurationControl = new BasicFileSelectionControl { DataContext = this };

            IsTimeVariantData = true;
            IsSelectable = true;
        }

        [XmlIgnore]
        public override string Xml
        {
            get { return new XmlSerializer<WindspeedFile> { Data = this }.SaveToXml(); }
            set
            {
                var settings = XmlSerializer<WindspeedFile>.LoadFromXml(value);
                DataLocation = settings.DataLocation;
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<WindspeedFile> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<WindspeedFile>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            DataLocation = settings.DataLocation;
        }

        public override Wind Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            var wind = new Wind();
            wind.TimePeriods.Add(new TimePeriodEnvironmentData<WindSample>() { TimePeriod = timePeriod });
            using (var reader = new StreamReader(new FileStream(DataLocation, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var lineNumber = 0;
                while (true)
                {
                    var fields = ReadFieldsFromLine(reader, ref lineNumber);
                    if (fields == null) break;

                    if (fields.Length != 3) throw new FormatException(string.Format("Wind speed file was not in the expected format at line {0}: Should be three comma-separated fields per line", lineNumber));
                    var geo = ParseGeo(fields[0], fields[1], "Wind speed file", lineNumber);

                    float windSpeed;
                    if (!float.TryParse(fields[2], out windSpeed)) throw new FormatException(string.Format("Wind speed file was not in the expected format at line {0}: Sediment type field is not an integer", lineNumber));
                    if (float.IsNaN(windSpeed) || float.IsInfinity(windSpeed) || Math.Abs(windSpeed) > 100) throw new FormatException(string.Format("Wind speed file was not in the expected format at line {0}: Wind speed cannot exceed 100 m/s", lineNumber));

                    var sample = new WindSample(geo, windSpeed);
                    if (geoRect.Contains(sample)) wind[timePeriod].EnvironmentData.Add(sample);
                }
            }
            return wind;
        }
    }
}