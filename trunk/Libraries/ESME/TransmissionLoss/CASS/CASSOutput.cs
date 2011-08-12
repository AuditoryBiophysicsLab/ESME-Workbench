﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Timers;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss.CASS
{
    public class CASSOutput
    {
        #region Public Properties

        #region file header

        public string RunDateTime { get; set; }
        public string OperatingSystemName { get; set; }
        public string SystemNodeName { get; set; }
        public string OperatingSystemRelease { get; set; }
        public string OperatingSystemVersion { get; set; }
        public string MachineType { get; set; }
        public string ProcessorType { get; set; }

        public string Title { get; set; }
        public string SiteName { get; set; }
        public float SiteRefLatLocation { get; set; }
        public string SiteRefLatLocationUnits { get; set; }
        public float SiteRefLonLocation { get; set; }
        public string SiteRefLonLocationUnits { get; set; }

        public float SourceRefLatLocation { get; set; }
        public string SourceRefLatLocationUnits { get; set; }
        public float SourceRefLonLocation { get; set; }
        public string SourceRefLonLocationUnits { get; set; }

        public string PlatformName { get; set; }
        public string SourceName { get; set; }
        public string ModeName { get; set; }
        public float Frequency { get; set; }
        public string FrequencyUnits { get; set; }
        public float DepressionElevationAngle { get; set; }
        public string DepressionElevationAngleUnits { get; set; }
        public float VerticalBeamPattern { get; set; }
        public string VerticalBeamPatternUnits { get; set; }
        public float SourceDepth { get; set; }
        public string SourceDepthUnits { get; set; }
        public float SourceLevel { get; set; }
        public string SourceLevelUnits { get; set; }
        public float MinWaterDepth { get; set; }
        public string MinWaterDepthUnits { get; set; }
        public float MaxWaterDepth { get; set; }
        public string MaxWaterDepthUnits { get; set; }
        public float WaterDepthIncrement { get; set; }
        public string WaterDepthIncrementUnits { get; set; }
        public float MinRangeDistance { get; set; }
        public string MinRangeDistanceUnits { get; set; }
        public float MaxRangeDistance { get; set; }
        public string MaxRangeDistanceUnits { get; set; }
        public float RangeDistanceIncrement { get; set; }
        public string RangeDistanceIncrementUnits { get; set; }

        public string BottomType { get; set; }
        public string Season { get; set; }
        public float WindSpeed { get; set; }
        public string WindSpeedUnits { get; set; }

        public float CASSLevel { get; set; }

        #endregion

        #region bearing header

        public int RadialCount { get; set; }
        public float[] RadialBearings { get; set; }

        #endregion

        #region range header

        public int RangeCellCount { get; set; }
        public float[] RangeCells { get; set; }

        #endregion

        #region depth header

        public int DepthCellCount { get; set; }
        public float[] DepthCells { get; set; }

        #endregion

        public List<float[,]> Pressures { get; set; }

        #endregion

        public string Filename { get; set; }

        public static CASSOutput Load(string fileName, bool headerOnly)
        {
            var result = new CASSOutput {Filename = fileName};
            using (var reader = new BinaryReader(new FileStream(fileName, FileMode.Open, FileAccess.Read)))
            {
                result.ReadFileHeader(reader);
                if (headerOnly) return result;
                
                
                #region bearing header read

                result.RadialCount = (int)reader.ReadSingle(); //note: comes in as a float, cast to int. See PH's docs
                result.RadialBearings = new float[result.RadialCount];
                for (var i = 0; i < result.RadialCount; i++) result.RadialBearings[i] = reader.ReadSingle();

                #endregion

                #region range header read

                result.RangeCellCount = (int)reader.ReadSingle();
                result.RangeCells = new float[result.RangeCellCount];
                for (var i = 0; i < result.RangeCellCount; i++) result.RangeCells[i] = reader.ReadSingle();

                #endregion

                #region depth header read

                result.DepthCellCount = (int)reader.ReadSingle();
                result.DepthCells = new float[result.DepthCellCount];
                for (var i = 0; i < result.DepthCellCount; i++) result.DepthCells[i] = reader.ReadSingle();

                #endregion

                #region pressure data read

                result.Pressures = new List<float[,]>();
                foreach (var pressure in result.RadialBearings.Select(bearing => new float[result.DepthCellCount,result.RangeCellCount])) 
                {
                    for (var i = 0; i < result.RangeCellCount; i++)
                        for (var j = 0; j < result.DepthCellCount; j++)
                            pressure[j, i] = reader.ReadSingle();
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
                WriteCASSField(writer, System.Environment.OSVersion.Version.ToString(), 25);
                WriteCASSField(writer, System.Environment.MachineName, 25);
                WriteCASSField(writer, System.Environment.OSVersion.ServicePack, 25);
                WriteCASSField(writer, System.Environment.OSVersion.Platform.ToString(), 25);
                WriteCASSField(writer, "Machine Type", 25);
                WriteCASSField(writer, "Processor Type", 25);
                WriteCASSField(writer, "Title", 50);
                WriteCASSField(writer, "Site Name", 50);

                const float siteRefLatLocation = 0;
                writer.Write(siteRefLatLocation);
                WriteCASSField(writer, "DEG", 10);

                const float siteRefLonLocation = 0;
                writer.Write(siteRefLonLocation);
                WriteCASSField(writer, "DEG", 10);

                var sourceRefLatLocation = transmissionLossField.Latitude;
                writer.Write(sourceRefLatLocation);
                WriteCASSField(writer, "DEG", 10);

                var sourceRefLonLocation = transmissionLossField.Longitude;
                writer.Write(sourceRefLonLocation);
                WriteCASSField(writer, "DEG", 10);

                WriteCASSField(writer, "Platform Name", 50);
                WriteCASSField(writer, "Source Name", 50);
                WriteCASSField(writer, "Mode Name", 50);

                writer.Write(transmissionLossField.HighFrequency); //todo: yes?
                WriteCASSField(writer, "HZ", 10);

                writer.Write(transmissionLossField.DepressionElevationAngle);
                WriteCASSField(writer, "DEG", 10);

                writer.Write(transmissionLossField.VerticalBeamWidth);
                WriteCASSField(writer, "DEG", 10);

                writer.Write(transmissionLossField.SourceDepth);
                WriteCASSField(writer, "M", 10);

                writer.Write(transmissionLossField.SourceLevel);
                WriteCASSField(writer, "DB", 10);

                writer.Write(transmissionLossField.Depths[0]);
                WriteCASSField(writer, "M", 10);

                writer.Write(transmissionLossField.Depths[transmissionLossField.Depths.Count - 1]);
                WriteCASSField(writer, "M", 10);

                var waterDepthIncrement = transmissionLossField.Depths[1] - transmissionLossField.Depths[0];
                writer.Write(waterDepthIncrement);
                WriteCASSField(writer, "M", 10);

                writer.Write(transmissionLossField.Ranges[0]);
                WriteCASSField(writer, "M", 10);


                writer.Write(transmissionLossField.Ranges[transmissionLossField.Ranges.Count - 1]);
                WriteCASSField(writer, "M", 10);

                var rangeDistanceIncrement = transmissionLossField.Ranges[1] - transmissionLossField.Ranges[0];
                writer.Write(rangeDistanceIncrement);
                WriteCASSField(writer, "M", 10);

                WriteCASSField(writer, "Bottom Type", 50);
                WriteCASSField(writer, "Season", 10);
                const float windSpeed = 0;
                writer.Write(windSpeed);
                WriteCASSField(writer, "windSpeed Units", 10);

                const float cassLevel = 0;
                writer.Write(cassLevel);

                if (writer.BaseStream.Position != 713) throw new FileFormatException("Cass Write: header is of incorrect length.");
                #endregion
                #region bearing header

                float radialCount = transmissionLossField.Radials.Count;
                writer.Write(radialCount);
                foreach (var radial in transmissionLossField.Radials)
                {
                    writer.Write(radial.BearingFromSource);
                }

                #endregion
                #region range header

                float rangeCount = transmissionLossField.Ranges.Count;
                writer.Write(rangeCount);
                foreach (var range in transmissionLossField.Ranges)
                {
                    writer.Write(range);
                }

                #endregion
                #region depth header

                float depthCount = transmissionLossField.Depths.Count;
                writer.Write(depthCount);
                foreach (var depth in transmissionLossField.Depths)
                {
                    writer.Write(depth);
                }

                #endregion
                #region payload pressure data
                foreach (var radial in transmissionLossField.Radials)
                {
                    for (var i = 0; i < radial.Ranges.Count; i++)
                    {
                        for (var j = 0; j < radial.Depths.Count; j++)
                        {
                            writer.Write(radial.TransmissionLoss[j, i]);
                        }
                    }
                }
                #endregion
            }
        }

        public void Write(string cassOutputFileName = null)
        {
            if (cassOutputFileName == null) cassOutputFileName = Filename;
            using (var writer = new BinaryWriter(new FileStream(cassOutputFileName, FileMode.Create, FileAccess.Write)))
            {
                #region file header write
                WriteCASSField(writer, RunDateTime, 25);
                WriteCASSField(writer, OperatingSystemName, 25);
                WriteCASSField(writer, SystemNodeName, 25);
                WriteCASSField(writer, OperatingSystemRelease, 25);
                WriteCASSField(writer, OperatingSystemVersion, 25);
                WriteCASSField(writer, MachineType, 25);
                WriteCASSField(writer, ProcessorType, 25);
                WriteCASSField(writer, Title, 50);
                WriteCASSField(writer, SiteName, 50);
                writer.Write(SiteRefLatLocation);
                WriteCASSField(writer, "DEG", 10);
                writer.Write(SiteRefLonLocation);
                WriteCASSField(writer, "DEG", 10);

                writer.Write(SourceRefLatLocation);
                WriteCASSField(writer, "DEG", 10);
                writer.Write(SourceRefLonLocation);
                WriteCASSField(writer, "DEG", 10);

                WriteCASSField(writer, PlatformName, 50);
                WriteCASSField(writer, SourceName, 50);
                WriteCASSField(writer, ModeName, 50);

                writer.Write(Frequency);
                WriteCASSField(writer, "HZ", 10);

                writer.Write(DepressionElevationAngle);
                WriteCASSField(writer, "DEG", 10);

                writer.Write(VerticalBeamPattern);
                WriteCASSField(writer, "DEG", 10);

                writer.Write(SourceDepth);
                WriteCASSField(writer, "M", 10);

                writer.Write(SourceLevel);
                WriteCASSField(writer, "DB", 10);

                writer.Write(MinWaterDepth);
                WriteCASSField(writer, "M", 10);

                writer.Write(MaxWaterDepth);
                WriteCASSField(writer, "M", 10);

                writer.Write(WaterDepthIncrement);
                WriteCASSField(writer, "M", 10);

                writer.Write(MinRangeDistance);
                WriteCASSField(writer, "M", 10);

                writer.Write(MaxRangeDistance);
                WriteCASSField(writer, "M", 10);

                writer.Write(RangeDistanceIncrement);
                WriteCASSField(writer, "M", 10);

                WriteCASSField(writer, BottomType, 50);
                WriteCASSField(writer, Season, 10);
                writer.Write(WindSpeed);
                WriteCASSField(writer, "M/S", 10);

                writer.Write(CASSLevel);

                if (writer.BaseStream.Position != 713) throw new FileFormatException("Cass Write: header is of incorrect length.");
                #endregion

                #region bearing header

                writer.Write((float)RadialCount);
                foreach (var radial in RadialBearings) writer.Write(radial);

                #endregion

                #region range header

                writer.Write((float)RangeCellCount);
                foreach (var range in RangeCells) writer.Write(range);

                #endregion

                #region depth header

                writer.Write((float)DepthCellCount);
                foreach (var depth in DepthCells) writer.Write(depth);

                #endregion
                
                #region payload pressure data

                foreach (var radial in Pressures)
                    for (var i = 0; i < radial.GetLength(1); i++)
                        for (var j = 0; j < radial.GetLength(0); j++) writer.Write(radial[j, i]);

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
            s = s ?? "";
            Encoding.ASCII.GetBytes(s, 0, s.Length <= fieldLength ? s.Length : fieldLength, buf, 0);
            w.Write(buf, 0, fieldLength);
        }

        void ReadFileHeader(BinaryReader reader)
        {
            #region file header read

            RunDateTime = ParseCASSString(reader, 25, '\0');
            OperatingSystemName = ParseCASSString(reader, 25, '\0');
            SystemNodeName = ParseCASSString(reader, 25, '\0');
            OperatingSystemRelease = ParseCASSString(reader, 25, '\0');
            OperatingSystemVersion = ParseCASSString(reader, 25, '\0');
            MachineType = ParseCASSString(reader, 25, '\0');
            ProcessorType = ParseCASSString(reader, 25, '\0');

            Title = ParseCASSString(reader, 50, '\0');
            SiteName = ParseCASSString(reader, 50, '\0');

            SiteRefLatLocation = reader.ReadSingle();
            SiteRefLatLocationUnits = ParseCASSString(reader, 10, '\0');
            SiteRefLonLocation = reader.ReadSingle();
            SiteRefLonLocationUnits = ParseCASSString(reader, 10, '\0');

            SourceRefLatLocation = reader.ReadSingle();
            SourceRefLatLocationUnits = ParseCASSString(reader, 10, '\0');
            SourceRefLonLocation = reader.ReadSingle();
            SourceRefLonLocationUnits = ParseCASSString(reader, 10, '\0');

            PlatformName = ParseCASSString(reader, 50, '\0');
            SourceName = ParseCASSString(reader, 50, '\0');
            ModeName = ParseCASSString(reader, 50, '\0');

            Frequency = reader.ReadSingle();
            FrequencyUnits = ParseCASSString(reader, 10, '\0');

            DepressionElevationAngle = reader.ReadSingle();
            DepressionElevationAngleUnits = ParseCASSString(reader, 10, '\0');

            VerticalBeamPattern = reader.ReadSingle();
            VerticalBeamPatternUnits = ParseCASSString(reader, 10, '\0');

            SourceDepth = reader.ReadSingle();
            SourceDepthUnits = ParseCASSString(reader, 10, '\0');

            SourceLevel = reader.ReadSingle();
            SourceLevelUnits = ParseCASSString(reader, 10, '\0');

            MinWaterDepth = reader.ReadSingle();
            MinWaterDepthUnits = ParseCASSString(reader, 10, '\0');

            MaxWaterDepth = reader.ReadSingle();
            MaxWaterDepthUnits = ParseCASSString(reader, 10, '\0');

            WaterDepthIncrement = reader.ReadSingle();
            WaterDepthIncrementUnits = ParseCASSString(reader, 10, '\0');

            MinRangeDistance = reader.ReadSingle();
            MinRangeDistanceUnits = ParseCASSString(reader, 10, '\0');

            MaxRangeDistance = reader.ReadSingle();
            MaxRangeDistanceUnits = ParseCASSString(reader, 10, '\0');

            RangeDistanceIncrement = reader.ReadSingle();
            RangeDistanceIncrementUnits = ParseCASSString(reader, 10, '\0');

            BottomType = ParseCASSString(reader, 50, '\0');
            Season = ParseCASSString(reader, 10, '\0');
            WindSpeed = reader.ReadSingle();
            WindSpeedUnits = ParseCASSString(reader, 10, '\0');

            CASSLevel = reader.ReadSingle();

            if (reader.BaseStream.Position != 713) throw new FileFormatException("CASSOutput: file header of incorrect length.");
            

            #endregion
        }
    }

    public class CASSOutputs : List<CASSOutput>
    {
        public CASSOutputs(string directoryToScan, string filePattern, EventHandler listUpdatedHandler = null)
        {
            _directory = directoryToScan;
            _pattern = filePattern;
            if (listUpdatedHandler != null)
            {
                ListUpdated += listUpdatedHandler;
                Task.Factory.StartNew(() =>
                {
                    SetWatch();
                    Refresh();
                });
            }
            else
            {
                SetWatch();
                Refresh();
            }
        }

        readonly string _directory;
        readonly string _pattern;

        public void RefreshInBackground() { Task.Factory.StartNew(Refresh); }

        public void Refresh()
        {
            if (_isRefreshing) return;
            _isRefreshing = true;
            Clear();
            var files = Directory.EnumerateFiles(_directory, _pattern);
            foreach (var file in files)
                Add(CASSOutput.Load(file, true));
            OnListUpdated();
            _isRefreshing = false;
        }

        public event EventHandler ListUpdated;

        protected virtual void OnListUpdated()
        {
            if (ListUpdated != null) ListUpdated(this, new EventArgs());
        }

        FileSystemWatcher _dirWatcher;
        Timer _dirTimer;
        bool _isRefreshing;

        void SetWatch()
        {
            _dirWatcher = new FileSystemWatcher(_directory, _pattern)
            {
                EnableRaisingEvents = true,
                NotifyFilter = (NotifyFilters.FileName | NotifyFilters.DirectoryName),
            };
            _dirWatcher.Created += DirectoryChanged;
            _dirWatcher.Deleted += DirectoryChanged;
        }

        void DirectoryChanged(object sender, FileSystemEventArgs e)
        {
            Debug.WriteLine("[Raw] Directory: " + e.Name + " " + e.ChangeType);
            if (_dirTimer != null) return;
            _dirTimer = new Timer(1000) { AutoReset = false, Enabled = true };
            _dirTimer.Elapsed += (s1, e1) =>
            {
                _dirTimer = null;
                Debug.WriteLine("Directory: " + e.Name + " " + e.ChangeType);
                Task.Factory.StartNew(Refresh);
            };
        }

    }
}