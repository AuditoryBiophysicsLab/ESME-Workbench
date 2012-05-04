using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss.CASS
{
    public class CASSOutput : ValidatingViewModel, IEquatable<AcousticProperties>, ISupportValidation
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

        public Geo Geo { get; set; }

        #endregion

        public string Filename { get; set; }

        #region Calculated properties, not written into the file header

        #region public AcousticProperties AcousticProperties { get; set; }

        public AcousticProperties AcousticProperties
        {
            get
            {
                return _acousticProperties ?? (_acousticProperties = new AcousticProperties
                {
                    DepressionElevationAngle = DepressionElevationAngle,
                    HighFrequency = Frequency,
                    LowFrequency = 0,
                    SourceDepth = SourceDepth,
                    VerticalBeamWidth = VerticalBeamPattern,
                });
            }
        }

        public bool Equals(AcousticProperties other) { return AcousticProperties.Equals(other); }

        AcousticProperties _acousticProperties;

        #endregion

        [XmlIgnore]
        public string RadialBearingsString
        {
            get
            {
                if (_radialBearingsString != null) return _radialBearingsString;
                var sb = new StringBuilder();
                foreach (var bearing in RadialBearings)
                    sb.AppendFormat("{0:0.##}, ", bearing);
                sb.Remove(sb.Length - 2, 2);    // Remove last comma and space
                _radialBearingsString = sb.ToString();
                return _radialBearingsString;
            }
        }

        [XmlIgnore]
        string _radialBearingsString;

        [XmlIgnore]
        public FileInfo FileInfo
        {
            get
            {
                return _fileInfo ?? (_fileInfo = new FileInfo(Filename));
            }
        }

        [XmlIgnore]
        FileInfo _fileInfo;

        [XmlIgnore]
        public int CellCount { get { return RadialCount * DepthCellCount * RangeCellCount; } }

        #endregion

        #region Validation
        [XmlIgnore]
        public WeakReference<Bathymetry> Bathymetry
        {
            get { return _bathymetry ?? (_bathymetry = new WeakReference<Bathymetry>(null)); }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
            }
        }

        WeakReference<Bathymetry> _bathymetry;

        #region public string ValidationErrorText { get; set; }
        [XmlIgnore]
        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            private set
            {
                _validationErrorText = value;
            }
        }

        string _validationErrorText;

        #endregion

        public void Validate()
        {
            if ((Bathymetry == null) || (Bathymetry.Target == null))
            {
                ValidationErrorText = "Unable to validate";
                return;
            }

            var bathymetry = Bathymetry.Target;
            if (!bathymetry.Samples.GeoRect.Contains(Geo))
            {
                ValidationErrorText = "Propagation point not contained within bathymetry bounds";
                return;
            }

            var errors = new StringBuilder();

            foreach (var radialBearing in RadialBearings)
            {
                var radialEndPoint = Geo.Offset(Geo.KilometersToRadians(MaxRangeDistance / 1000f), Geo.DegreesToRadians(radialBearing));
                if (!bathymetry.Samples.GeoRect.Contains(radialEndPoint))
                {
                    //Console.WriteLine("Source name {0} location ({1}, {2}) bearing {3} endpoint ({4}, {5}) outside of bathymetry", Name, Latitude, Longitude, radialBearing, radialEndPoint.Latitude, radialEndPoint.Longitude);
                    errors.AppendLine(string.Format("Radial with bearing {0} extends beyond bathymetry bounds", radialBearing));
                }
            }
            if (float.IsNaN(ThresholdRadius)) errors.AppendLine("Checking radials for threshold value, please wait...");
            else if (ThresholdRadius >= MaxRangeDistance) errors.AppendLine("Calculation radius too small");
            ValidationErrorText = errors.ToString().Trim();
        }

        #endregion

        [XmlIgnore]
        public float ThresholdRadius { get; private set; }
        [XmlIgnore]
        public float[] ThresholdRadii { get; private set; }

        public void CheckThreshold(float threshold)
        {
            if (SourceLevel < threshold)
            {
                ThresholdRadius = 0;
                return;
            }
            var unloadAfterCheck = false;
            try
            {
                if (Pressures == null)
                {
                    Load();
                    unloadAfterCheck = true;
                }
            }
            catch (Exception e)
            {
                if (!string.IsNullOrEmpty(ValidationErrorText)) ValidationErrorText += "\n";
                ValidationErrorText += "Error loading radial data: " + e.Message + "\nPropagation file is corrupted.";
                return;    
            }
            if (Pressures == null) throw new ApplicationException("No radial data found");
            ThresholdRadii = new float[Pressures.Count];
            for (var i = 0; i < Pressures.Count; i++) ThresholdRadii[i] = RangeAtThreshold(Pressures[i], threshold);
            ThresholdRadius = ThresholdRadii.Max();
            if (unloadAfterCheck) Pressures = null;
        }

        float RangeAtThreshold(float[,] radial, float thresholdValue)
        {
            var rangeAtThreshold = MaxRangeDistance;
            var initialCheck = true;
            for (var rangeIndex = RangeCellCount - 1; rangeIndex > 0; rangeIndex--)    // Start at the end and work backwards
            {
                var maxPressure = float.MinValue;
                for (var depthIndex = 0; depthIndex < DepthCellCount; depthIndex++)
                    maxPressure = Math.Max(maxPressure, radial[depthIndex, rangeIndex]);
                if ((initialCheck) && (maxPressure >= thresholdValue)) return rangeAtThreshold;
                initialCheck = false;
                if (maxPressure >= thresholdValue) return rangeAtThreshold;
                rangeAtThreshold = RangeDistanceIncrement * rangeIndex;
            }
            return 0;
        }

        public static CASSOutput FromBinaryFile(string fileName, bool headerOnly)
        {
            FileStream stream = null;
            var count = 10;
            var lastException = new Exception("No exception.  Isn't that wierd?");
            while (count > 0)
            {
                try
                {
                    stream = new FileStream(fileName, FileMode.Open, FileAccess.Read);
                    break;
                }
                catch (Exception e)
                {
                    Thread.Sleep(100);
                    lastException = e;
                }
                count--;
            }
            if (stream == null) throw (lastException);
            using (var reader = new BinaryReader(stream))
            {
                var result = new CASSOutput { Filename = fileName };
                result.Load(reader, headerOnly);
                return result;
            }
        }

        public void Load(string fileName = null, bool headerOnly = false)
        {
            if (fileName != null) Filename = fileName;
            if (string.IsNullOrEmpty(Filename)) throw new NullReferenceException("Filename is null or empty");

            FileStream stream = null;
            var count = 10;
            var lastException = new Exception("No exception.  Isn't that wierd?");
            while (count > 0)
            {
                try
                {
                    stream = new FileStream(Filename, FileMode.Open, FileAccess.Read);
                    break;
                }
                catch (Exception e)
                {
                    Thread.Sleep(100);
                    lastException = e;
                }
                count--;
            }
            if (stream == null) throw (lastException);
            using (var reader = new BinaryReader(stream))
            {
                Load(reader, headerOnly);
            }
        }

        public void ToBinaryFile(string cassOutputFileName = null)
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

        void Load(BinaryReader reader, bool headerOnly)
        {
            ReadFileHeader(reader);
            if (headerOnly) return;
            ReadFileContents(reader);
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

            Geo = new Geo(SourceRefLatLocation, SourceRefLonLocation);

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

            #region bearing header read

            RadialCount = (int)reader.ReadSingle(); //comes in as a float, cast to int. See Peter Hulton's docs
            RadialBearings = new float[RadialCount];
            for (var i = 0; i < RadialCount; i++) RadialBearings[i] = reader.ReadSingle();

            #endregion

            #region range header read

            RangeCellCount = (int)reader.ReadSingle();
            RangeCells = new float[RangeCellCount];
            for (var i = 0; i < RangeCellCount; i++) RangeCells[i] = reader.ReadSingle();

            #endregion

            #region depth header read

            DepthCellCount = (int)reader.ReadSingle();
            DepthCells = new float[DepthCellCount];
            for (var i = 0; i < DepthCellCount; i++) DepthCells[i] = reader.ReadSingle();

            #endregion


            #endregion
        }

        void ReadFileContents(BinaryReader reader)
        {
            Pressures = new List<float[,]>();
            foreach (var pressure in RadialBearings.Select(bearing => new float[DepthCellCount, RangeCellCount]))
            {
                for (var i = 0; i < RangeCellCount; i++)
                    for (var j = 0; j < DepthCellCount; j++)
                        pressure[j, i] = reader.ReadSingle();
                Pressures.Add(pressure);
            }
        }
    }

    public class CASSOutputs : ObservableList<CASSOutput>, IDisposable
    {
        public CASSOutputs(string directoryToScan, string filePattern, NotifyCollectionChangedEventHandler collectionChangedHandler = null, List<AcousticProperties> propertiesToMatch = null)
        {
            _directory = directoryToScan;
            _pattern = filePattern;
            _propertiesToMatch = propertiesToMatch;

            if (collectionChangedHandler != null)
            {
                CollectionChanged += collectionChangedHandler;
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
        readonly List<AcousticProperties> _propertiesToMatch;
        readonly object _lockObject = new object();

        public void Refresh()
        {
            if (_isRefreshing) return;
            _isRefreshing = true;
            Clear();
            var files = Directory.EnumerateFiles(_directory, _pattern);
            foreach (var file in files)
                Add(file);
            _isRefreshing = false;
        }

        void Remove(string fileName)
        {
            lock (_lockObject)
            {
                var target = Find(item => item.Filename == fileName);
                if (target == null) return;
                Remove(target);
            }
        }

        void Add(string fileName)
        {
            var target = Find(item => item.Filename == fileName);
            if (target != null) return;
            target = CASSOutput.FromBinaryFile(fileName, true);
            lock (_lockObject)
            {
                if (_propertiesToMatch == null)
                {
                    Add(target);
                }
                else if (_propertiesToMatch.Any(property => property.Equals(target.AcousticProperties)))
                {
                    Add(target);
                }
            }
        }

        FileSystemWatcher _dirWatcher;
        bool _isRefreshing;

        void SetWatch()
        {
            Directory.CreateDirectory(_directory);
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
            //Debug.WriteLine("[Raw] Directory: " + e.Name + " " + e.ChangeType);
            switch (e.ChangeType)
            {
                case WatcherChangeTypes.Created:
                    Task.Factory.StartNew(() => Add(e.FullPath));
                    break;
                case WatcherChangeTypes.Deleted:
                    Task.Factory.StartNew(() => Remove(e.FullPath));
                    break;
            }
        }

        bool _disposed;
        public void Dispose() 
        {
            Dispose(true);
            GC.SuppressFinalize(this); 
        }

        protected virtual void Dispose(bool disposing)
        {
            // If you need thread safety, use a lock around these 
            // operations, as well as in your methods that use the resource.
            if (!_disposed)
            {
                if (disposing)
                {
                    if (_dirWatcher != null)
                    {
                        _dirWatcher.EnableRaisingEvents = false;
                        _dirWatcher.Dispose();
                    }
                    ClearCollectionChangedHandlers();
                }

                // Indicate that the instance has been disposed.
                _dirWatcher = null;
                _disposed = true;
            }
        }
    }
}