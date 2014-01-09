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
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Bathymetry,
                           Name = "Bathymetry file",
                           Description = "Text file containing latitude/longitude/depth tuples")]
    public sealed class BathymetryFile : FileBasedDataSource<Bathymetry>
    {
        public BathymetryFile()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The bathymetry file";
            ControlCaption = "Bathymetry file";
            DialogTitle = "Please locate the bathymetry file";
            FilenameFilter = "Bathymetry files (*.bathymetry)|*.bathymetry|All files (*.*)|*.*";

            ConfigurationControl = new BasicFileSelectionControl { DataContext = this };
            IsTimeVariantData = false;
            IsSelectable = true;
        }

        [XmlIgnore] 
        public override string Xml
        {
            get { return new XmlSerializer<BathymetryFile> { Data = this }.SaveToXml(); }
            set
            {
                var settings = XmlSerializer<BathymetryFile>.LoadFromXml(value);
                DataLocation = settings.DataLocation;
            }
        }

protected override void Save()
        {
            var serializer = new XmlSerializer<BathymetryFile> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<BathymetryFile>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            DataLocation = settings.DataLocation;
        }

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            var bathymetry = new Bathymetry();
            using (var reader = new StreamReader(new FileStream(DataLocation, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var lineNumber = 0;
                while (true)
                {
                    var fields = ReadFieldsFromLine(reader, ref lineNumber);
                    if (fields == null) break;

                    if (fields.Length != 3) throw new FormatException(string.Format("Bathymetry file was not in the expected format at line {0}: Should be three comma-separated fields per line", lineNumber));
                    var geo = ParseGeo(fields[0], fields[1], "Bathymetry file", lineNumber);

                    float depth;
                    if (!float.TryParse(fields[2], out depth)) throw new FormatException(string.Format("Bathymetry file was not in the expected format at line {0}: Depth field is not a floating point number", lineNumber));
                    if (float.IsNaN(depth) || depth > 11000) throw new FormatException(string.Format("Bathymetry file was not in the expected format at line {0}: Depth must be less than 11000", lineNumber));

                    var sample = new Geo<float>(geo, depth);
                    if (geoRect.Contains(sample)) bathymetry.Samples.Add(sample);
                }
            }
            return bathymetry;
        }
    }
}
