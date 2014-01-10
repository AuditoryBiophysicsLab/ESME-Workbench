using System;
using System.ComponentModel.Composition;
using System.IO;
using ESME.Environment;
using ESME.Plugins;
using FileBasedDataSources.Controls;
using HRC.Navigation;
using HRC.Utility;

namespace FileBasedDataSources
{
    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.SoundSpeed,
                           Name = "Sound speed file",
                           Description = "Text file containing sound speed data")]
    public sealed class SoundspeedFile : FileBasedDataSource<SoundSpeed>
    {
        public SoundspeedFile()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The sound speed file";
            ControlCaption = "Sound speed file";
            DialogTitle = "Please locate the sound speed file";
            FilenameFilter = "Sound speed files (*.soundspeed)|*.soundspeed|All files (*.*)|*.*";

            ConfigurationControl = new BasicFileSelectionControl { DataContext = this };
            IsTimeVariantData = true;
            IsSelectable = true;
        }

        public override string Xml { get; set; }

        protected override void Save()
        {
            var serializer = new XmlSerializer<SoundspeedFile> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<SoundspeedFile>.LoadExistingFile(ConfigurationFile, null);
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

                    if (fields.Length != 2) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Expected two comma-separated fields on this line", lineNumber));

                    var profile = new SoundSpeedProfile(ParseGeo(fields[0], fields[1], "Sound speed file", lineNumber));

                    // Here, we read depth/speed pairs until we get an empty line
                    while (true)
                    {
                        fields = ReadFieldsFromLine(reader, ref lineNumber);
                        if (fields == null || fields.Length == 0) break;

                        if (fields.Length != 2) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Expected two comma-separated fields on this line", lineNumber));

                        float depth;
                        if (!float.TryParse(fields[0], out depth)) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Depth field is not a floating point number", lineNumber));
                        if (float.IsNaN(depth) || depth < 0 || depth > 11000) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Depth must be between 0 and 11000", lineNumber));

                        float speed;
                        if (!float.TryParse(fields[1], out speed)) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Speed field is not a floating point number", lineNumber));
                        if (float.IsNaN(speed) || speed < 1000 || speed > 2000) throw new FormatException(string.Format("Sound speed file was not in the expected format at line {0}: Speed must be between 1000 and 2000", lineNumber));
                        profile.Add(new SoundSpeedSample(depth, speed));
                    }
                    if (geoRect.Contains(profile)) soundSpeed[timePeriod].EnvironmentData.Add(profile);
                }
            }
            return soundSpeed;
        }
    }
}
