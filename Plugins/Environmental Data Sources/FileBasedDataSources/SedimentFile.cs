using System;
using System.Collections.Generic;
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
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "Sediment file",
                           Description = "Text file containing latitude/longitude/sediment-type tuples")]
    public sealed class SedimentFile : FileBasedDataSource<Sediment>
    {
        public SedimentFile()
        {
            SetPropertiesFromAttributes(GetType());
            DataLocationHelp = "The sediment file";
            ControlCaption = "Sediment file";
            DialogTitle = "Please locate the sediment file";
            FilenameFilter = "Sediment files (*.sediment)|*.sediment|All files (*.*)|*.*";

            ConfigurationControl = new BasicFileSelectionControl { DataContext = this };
            IsTimeVariantData = false;
            IsSelectable = true;
        }

        [XmlIgnore]
        public override string Xml
        {
            get { return new XmlSerializer<SedimentFile> { Data = this }.SaveToXml(); }
            set
            {
                var settings = XmlSerializer<SedimentFile>.LoadFromXml(value);
                DataLocation = settings.DataLocation;
            }
        }

        protected override void Save()
        {
            var serializer = new XmlSerializer<SedimentFile> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<SedimentFile>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            DataLocation = settings.DataLocation;
        }

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            var sediment = new Sediment();
            using (var reader = new StreamReader(new FileStream(DataLocation, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var lineNumber = 0;
                while (true)
                {
                    var fields = ReadFieldsFromLine(reader, ref lineNumber);
                    if (fields == null) break;

                    if (fields.Length != 3) throw new FormatException(string.Format("Sediment file was not in the expected format at line {0}: Should be three comma-separated fields per line", lineNumber));
                    var geo = ParseGeo(fields[0], fields[1], "Sediment file", lineNumber);

                    short sedimentType;
                    if (!short.TryParse(fields[2], out sedimentType)) throw new FormatException(string.Format("Sediment file was not in the expected format at line {0}: Sediment type field is not an integer", lineNumber));
                    if (BottomSedimentTypeTable.SedimentTypes[sedimentType] == null) throw new FormatException(string.Format("Sediment file was not in the expected format at line {0}: Sediment type {1} not found in lookup table", lineNumber, sedimentType));

                    var sample = new SedimentSample(geo, new SedimentSampleBase { SampleValue = sedimentType });
                    if (geoRect.Contains(sample)) sediment.Samples.Add(sample);
                }
            }
            return sediment;
        }
    }

    [Serializable]
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "User-defined sediment",
                           Description = "Choose a single sediment type from a list")]
    public sealed class UserDefinedSediment : FileBasedDataSource<Sediment>
    {
        public UserDefinedSediment()
        {
            SetPropertiesFromAttributes(GetType());

            ConfigurationControl = new UserDefinedSedimentControl { DataContext = this };
            IsTimeVariantData = false;
            IsSelectable = true;
        }

        public Dictionary<int, string> ValidSedimentTypes { get { return BottomSedimentTypeTable.SedimentNames; } }
        public int SelectedSedimentType { get; set; }
        public override string Xml { get; set; }

        protected override void Save()
        {
            var serializer = new XmlSerializer<UserDefinedSediment> { Data = this };
            serializer.Save(ConfigurationFile, null);
        }

        public override void LoadSettings()
        {
            var settings = XmlSerializer<UserDefinedSediment>.LoadExistingFile(ConfigurationFile, null);
            if (settings == null) return;
            SelectedSedimentType = settings.SelectedSedimentType;
        }

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            var sediment = new Sediment();
            var latitudeSteps = (int)geoRect.Height * 4;
            var longitudeSteps = (int)geoRect.Width * 4;
            for (var latitude = geoRect.South; latitude <= geoRect.North; latitude += geoRect.Height / latitudeSteps) 
                for (var longitude = geoRect.West; longitude <= geoRect.East; longitude += geoRect.Width / longitudeSteps) 
                    sediment.Samples.Add(new SedimentSample(new Geo(latitude, longitude), new SedimentSampleBase { SampleValue = (short)SelectedSedimentType }));
            return sediment;
        }
    }
}