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

                result.RunDateTime = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.OperatingSystemName = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.SystemNodeName = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.OperatingSystemRelease = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.OperatingSystemVersion = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.MachineType = Encoding.ASCII.GetString(reader.ReadBytes(25));
                result.ProcessorType = Encoding.ASCII.GetString(reader.ReadBytes(25));

                result.Title = Encoding.ASCII.GetString(reader.ReadBytes(50));
                result.SiteName = Encoding.ASCII.GetString(reader.ReadBytes(50));

                result.SiteRefLatLocation = reader.ReadSingle();
                result.SiteRefLatLocationUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));
                result.SiteRefLonLocation = reader.ReadSingle();
                result.SiteRefLonLocationUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.SourceRefLatLocation = reader.ReadSingle();
                result.SourceRefLatLocationUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));
                result.SourceRefLonLocation = reader.ReadSingle();
                result.SourceRefLonLocationUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.PlatformName = Encoding.ASCII.GetString(reader.ReadBytes(50));
                result.SourceName = Encoding.ASCII.GetString(reader.ReadBytes(50));
                result.ModeName = Encoding.ASCII.GetString(reader.ReadBytes(50));

                result.Frequency = reader.ReadSingle();
                result.FrequencyUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.DepressionElevationAngle = reader.ReadSingle();
                result.DepressionElevationAngleUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.VerticalBeamPattern = reader.ReadSingle();
                result.VerticalBeamPatternUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.SourceDepth = reader.ReadSingle();
                result.SourceDepthUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.SourceLevel = reader.ReadSingle();
                result.SourceLevelUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.MinWaterDepth = reader.ReadSingle();
                result.MinWaterDepthUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.MaxWaterDepth = reader.ReadSingle();
                result.MaxWaterDepthUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.WaterDepthIncrement = reader.ReadSingle();
                result.WaterDepthIncrementUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.MinRangeDistance = reader.ReadSingle();
                result.MinRangeDistanceUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.MaxRangeDistance = reader.ReadSingle();
                result.MaxRangeDistanceUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.RangeDistanceIncrement = reader.ReadSingle();
                result.RangeDistanceIncrementUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.BottomType = Encoding.ASCII.GetString(reader.ReadBytes(50));
                result.Season = Encoding.ASCII.GetString(reader.ReadBytes(10));
                result.WindSpeed = reader.ReadSingle();
                result.WindSpeedUnits = Encoding.ASCII.GetString(reader.ReadBytes(10));

                result.CASSLevel = reader.ReadSingle();

                if (reader.BaseStream.Position != 713) throw new FileFormatException("CASSOutput: file header of incorrect length.");

                #endregion

                #region bearing header read

                result.RadialCount = (int) reader.ReadSingle(); //note: comes in as a float, cast to int. See PH's docs
                result.RadialBearings = new float[result.RadialCount];
                for (int i = 0; i < result.RadialCount; i++) result.RadialBearings[i] = reader.ReadSingle();

                #endregion

                #region range header read

                result.RangeCellCount = (int) reader.ReadSingle();
                result.RangeCells = new float[result.RangeCellCount];
                for (int i = 0; i < result.RangeCellCount; i++) result.RangeCells[i] = reader.ReadSingle();

                #endregion

                #region depth header read

                result.DepthCellCount = (int) reader.ReadSingle();
                result.DepthCells = new float[result.DepthCellCount];
                for (int i = 0; i < result.DepthCellCount; i++) result.DepthCells[i] = reader.ReadSingle();

                #endregion

                #region pressure data read

                result.Pressures = new List<float[,]>();
                foreach (float bearing in result.RadialBearings)
                {
                    var pressure = new float[result.DepthCellCount,result.RangeCellCount];

                    for (int i = 0; i < result.RangeCellCount; i++)
                    {
                        for (int j = 0; j < result.DepthCellCount; j++)
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
    }
}