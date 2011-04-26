using System.Collections.Generic;
using System.IO;
using System.Text;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss.CASS
{
    public class CASSOutput
    {
        #region Public Properties

        #region file header

        public string RunDateTime { get; private set; }
        public string OperatingSystemName { get; private set; }
        public string SystemNodeName { get; private set; }
        public string OperatingSystemRelease { get; private set; }
        public string OperatingSystemVersion { get; private set; }
        public string MachineType { get; private set; }
        public string ProcessorType { get; private set; }

        public string Title { get; private set; }
        public string SiteName { get; private set; }
        public float SiteRefLatLocation { get; private set; }
        public string SiteRefLatLocationUnits { get; private set; }
        public float SiteRefLonLocation { get; private set; }
        public string SiteRefLonLocationUnits { get; private set; }

        public float SourceRefLatLocation { get; private set; }
        public string SourceRefLatLocationUnits { get; private set; }
        public float SourceRefLonLocation { get; private set; }
        public string SourceRefLonLocationUnits { get; private set; }

        public string PlatformName { get; private set; }
        public string SourceName { get; private set; }
        public string ModeName { get; private set; }
        public float Frequency { get; private set; }
        public string FrequencyUnits { get; private set; }
        public float DepressionElevationAngle { get; private set; }
        public string DepressionElevationAngleUnits { get; private set; }
        public float VerticalBeamPattern { get; private set; }
        public string VerticalBeamPatternUnits { get; private set; }
        public float SourceDepth { get; private set; }
        public string SourceDepthUnits { get; private set; }
        public float SourceLevel { get; private set; }
        public string SourceLevelUnits { get; private set; }
        public float MinWaterDepth { get; private set; }
        public string MinWaterDepthUnits { get; private set; }
        public float MaxWaterDepth { get; private set; }
        public string MaxWaterDepthUnits { get; private set; }
        public float WaterDepthIncrement { get; private set; }
        public string WaterDepthIncrementUnits { get; private set; }
        public float MinRangeDistance { get; private set; }
        public string MinRangeDistanceUnits { get; private set; }
        public float MaxRangeDistance { get; private set; }
        public string MaxRangeDistanceUnits { get; private set; }
        public float RangeDistanceIncrement { get; private set; }
        public string RangeDistanceIncrementUnits { get; private set; }

        public string BottomType { get; private set; }
        public string Season { get; private set; }
        public float WindSpeed { get; private set; }
        public string WindSpeedUnits { get; private set; }

        public float CASSLevel { get; private set; }

        #endregion

        #region bearing header

        public int RadialCount { get; private set; }
        public float[] RadialBearings { get; private set; }

        #endregion

        #region range header

        public int RangeCellCount { get; private set; }
        public float[] RangeCells { get; private set; }

        #endregion

        #region depth header

        public int DepthCellCount { get; private set; }
        public float[] DepthCells { get; private set; }

        #endregion

        public List<float[,]> Pressures { get; private set; }

        #endregion

        public string Filename { get; private set; }

        public static CASSOutput Load(string fileName)
        {
            var result = new CASSOutput();
            result.Filename = fileName;
            using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read)))
            {
                #region file header read

                result.RunDateTime = ParseCASSString(reader, 25, '\0');
                result.OperatingSystemName = ParseCASSString(reader, 25, '\0');
                result.SystemNodeName = ParseCASSString(reader, 25, '\0');
                result.OperatingSystemRelease = ParseCASSString(reader, 25, '\0');
                result.OperatingSystemVersion = ParseCASSString(reader, 25, '\0');
                result.MachineType = ParseCASSString(reader, 25, '\0');
                result.ProcessorType = ParseCASSString(reader, 25, '\0');

                result.Title = ParseCASSString(reader, 50, '\0');
                result.SiteName = ParseCASSString(reader, 50, '\0');

                result.SiteRefLatLocation = reader.ReadSingle();
                result.SiteRefLatLocationUnits = ParseCASSString(reader, 10, '\0');
                result.SiteRefLonLocation = reader.ReadSingle();
                result.SiteRefLonLocationUnits = ParseCASSString(reader, 10, '\0');

                result.SourceRefLatLocation = reader.ReadSingle();
                result.SourceRefLatLocationUnits = ParseCASSString(reader, 10, '\0');
                result.SourceRefLonLocation = reader.ReadSingle();
                result.SourceRefLonLocationUnits = ParseCASSString(reader, 10, '\0');

                result.PlatformName = ParseCASSString(reader, 50, '\0');
                result.SourceName = ParseCASSString(reader, 50, '\0');
                result.ModeName = ParseCASSString(reader, 50, '\0');

                result.Frequency = reader.ReadSingle();
                result.FrequencyUnits = ParseCASSString(reader, 10, '\0');

                result.DepressionElevationAngle = reader.ReadSingle();
                result.DepressionElevationAngleUnits = ParseCASSString(reader, 10, '\0');

                result.VerticalBeamPattern = reader.ReadSingle();
                result.VerticalBeamPatternUnits = ParseCASSString(reader, 10, '\0');

                result.SourceDepth = reader.ReadSingle();
                result.SourceDepthUnits = ParseCASSString(reader, 10, '\0');

                result.SourceLevel = reader.ReadSingle();
                result.SourceLevelUnits = ParseCASSString(reader, 10, '\0');

                result.MinWaterDepth = reader.ReadSingle();
                result.MinWaterDepthUnits = ParseCASSString(reader, 10, '\0');

                result.MaxWaterDepth = reader.ReadSingle();
                result.MaxWaterDepthUnits = ParseCASSString(reader, 10, '\0');

                result.WaterDepthIncrement = reader.ReadSingle();
                result.WaterDepthIncrementUnits = ParseCASSString(reader, 10, '\0');

                result.MinRangeDistance = reader.ReadSingle();
                result.MinRangeDistanceUnits = ParseCASSString(reader, 10, '\0');

                result.MaxRangeDistance = reader.ReadSingle();
                result.MaxRangeDistanceUnits = ParseCASSString(reader, 10, '\0');

                result.RangeDistanceIncrement = reader.ReadSingle();
                result.RangeDistanceIncrementUnits = ParseCASSString(reader, 10, '\0');

                result.BottomType = ParseCASSString(reader, 50, '\0');
                result.Season = ParseCASSString(reader, 10, '\0');
                result.WindSpeed = reader.ReadSingle();
                result.WindSpeedUnits = ParseCASSString(reader, 10, '\0');

                result.CASSLevel = reader.ReadSingle();

                if (reader.BaseStream.Position != 713) throw new FileFormatException("CASSOutput: file header of incorrect length.");

                #endregion

                #region bearing header read

                result.RadialCount = (int)reader.ReadSingle(); //note: comes in as a float, cast to int. See PH's docs
                result.RadialBearings = new float[result.RadialCount];
                for (int i = 0; i < result.RadialCount; i++) result.RadialBearings[i] = reader.ReadSingle();

                #endregion

                #region range header read

                result.RangeCellCount = (int)reader.ReadSingle();
                result.RangeCells = new float[result.RangeCellCount];
                for (int i = 0; i < result.RangeCellCount; i++) result.RangeCells[i] = reader.ReadSingle();

                #endregion

                #region depth header read

                result.DepthCellCount = (int)reader.ReadSingle();
                result.DepthCells = new float[result.DepthCellCount];
                for (int i = 0; i < result.DepthCellCount; i++) result.DepthCells[i] = reader.ReadSingle();

                #endregion

                #region pressure data read

                result.Pressures = new List<float[,]>();
                foreach (float bearing in result.RadialBearings)
                {
                    var pressure = new float[result.DepthCellCount, result.RangeCellCount];

                    for (var i = 0; i < result.RangeCellCount; i++)
                    {
                        for (var j = 0; j < result.DepthCellCount; j++)
                        {
                            pressure[j, i] = reader.ReadSingle();
                        }
                    }
                    result.Pressures.Add(pressure);
                }

                #endregion
            }

            return result;
        }

        public static void Write(string cassOutputFileName, TransmissionLossField transmissionLossField)
        {
            using (var writer = new BinaryWriter(new FileStream(cassOutputFileName, FileMode.Create, FileAccess.Write)))
            {
                #region file header write
                WriteCASSField(writer, "Run Date and Time", 25);
                WriteCASSField(writer, "OS Name", 25);
                WriteCASSField(writer, "Computer Node Name", 25);
                WriteCASSField(writer, "OS Release", 25);
                WriteCASSField(writer, "OS Version", 25);
                WriteCASSField(writer, "Machine Type", 25);
                WriteCASSField(writer, "Processor Type", 25);
                WriteCASSField(writer, "Title", 50);
                WriteCASSField(writer, "Site Name", 50);
                
                float siteRefLatLocation = 0;
                writer.Write(siteRefLatLocation);
                WriteCASSField(writer, "SiteRefLatLocation Units", 10);
                
                float siteRefLonLocation = 0;
                writer.Write(siteRefLonLocation);
                WriteCASSField(writer, "SiteRefLonLocation Units", 10);

                float sourceRefLatLocation = transmissionLossField.Latitude;
                writer.Write(sourceRefLatLocation);
                WriteCASSField(writer, "DEG", 10);

                float sourceRefLonLocation = transmissionLossField.Longitude;
                writer.Write(sourceRefLonLocation);
                WriteCASSField(writer, "DEG", 10);

                WriteCASSField(writer, "Platform Name", 50);
                WriteCASSField(writer, "Source Name", 50);
                WriteCASSField(writer, "Mode Name", 50);

                float frequency = 0;
                writer.Write(frequency);
                WriteCASSField(writer, "Frequency Units", 10);

                writer.Write(transmissionLossField.DepressionElevationAngle);
                WriteCASSField(writer, "D/E Angle Units", 10);

                writer.Write(transmissionLossField.VerticalBeamWidth);
                WriteCASSField(writer, "verticalBeamPattern Units", 10);
                
                writer.Write(transmissionLossField.SourceDepth);
                WriteCASSField(writer, "sourceDepth Units", 10);

                writer.Write(transmissionLossField.SourceLevel);
                WriteCASSField(writer, "sourceLevel Units", 10);

                float minWaterDepth = 0;
                writer.Write(minWaterDepth);
                WriteCASSField(writer, "minWaterDepth Units", 10);

                float maxWaterDepth = 0;
                writer.Write(maxWaterDepth);
                WriteCASSField(writer, "maxWaterDepth Units", 10);

                float waterDepthIncrement = 0;
                writer.Write(waterDepthIncrement);
                WriteCASSField(writer, "waterDepthIncrement Units", 10);

                float minRangeDistance = 0;
                writer.Write(minRangeDistance);
                WriteCASSField(writer, "minRangeDistance Units", 10);

                float maxRangeDistance = 0;
                writer.Write(maxRangeDistance);
                WriteCASSField(writer, "maxRangeDistance Units", 10);

                float rangeDistanceIncrement = 0;
                writer.Write(rangeDistanceIncrement);
                WriteCASSField(writer, "rangeDistanceIncrement Units", 10);

                WriteCASSField(writer, "Bottom Type", 50);
                WriteCASSField(writer, "Season", 10);
                float windSpeed = 0;
                writer.Write(windSpeed);
                WriteCASSField(writer, "windSpeed Units", 10);

                float cassLevel = 0;
                writer.Write(cassLevel);

                if(writer.BaseStream.Position != 713) throw new FileFormatException("Cass Write: header is of incorrect length.");
                #endregion
                #region bearing header

                float radialCount = transmissionLossField.Radials.Length;
                writer.Write(radialCount);
                foreach (var radial in transmissionLossField.Radials)
                {
                    writer.Write(radial.BearingFromSource);
                }

                #endregion
                #region range header

                float rangeCount = transmissionLossField.Ranges.Length;
                writer.Write(rangeCount);
                foreach (float range in transmissionLossField.Ranges)
                {
                    writer.Write(range);
                }

                #endregion
                #region depth header

                float depthCount = transmissionLossField.Depths.Length;
                writer.Write(depthCount);
                foreach (float depth in transmissionLossField.Depths)
                {
                    writer.Write(depth);
                }

                #endregion
                #region payload pressure data
                foreach (var radial in transmissionLossField.Radials)
                {
                    for (var i = 0; i < radial.Ranges.Length; i++)
                    {
                        for (var j = 0; j < radial.Depths.Length; j++)
                        {
                            writer.Write(radial.TransmissionLoss[j,i]);
                        }
                    }
                }
                #endregion
            }
        }

        static string ParseCASSString(BinaryReader r, int charLength, char paddingChar)
        {
            var result = Encoding.ASCII.GetString(r.ReadBytes(charLength)).TrimEnd(paddingChar);
            return !string.IsNullOrEmpty(result) ? result : null;
        }

        static void WriteCASSField(BinaryWriter w, string s, int fieldLength)
        {
            var buf = new byte[fieldLength];
            for (var i = 0; i < fieldLength; i++) buf[i] = 0;
            var result = Encoding.ASCII.GetBytes(s, 0, s.Length <= fieldLength ? s.Length : fieldLength, buf, 0);
            w.Write(buf, 0, fieldLength);
        }
    }
}