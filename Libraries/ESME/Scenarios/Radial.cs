using System;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using ESME.Database;
using ESME.Locations;
using ESME.TransmissionLoss.Bellhop;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Scenarios
{
    [NotifyPropertyChanged]
    public class Radial : IHaveGuid
    {
        public Radial()
        {
            // For reasons I don't fully understand, using the WeakEventListener pattern here (via PropertyObserver<Radial>)
            // causes some kind of aberrant behavior whereby adding a new AnalysisPoint won't cause the TL's to calculate
            // properly.  Doing it old-school like this DOES seem to work.
            Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
            //((INotifyPropertyChanged)this).PropertyChanged += PropertyChangedHandler;
            PropertyChangedEventManager.AddHandler((INotifyPropertyChanged)this, PropertyChangedHandler, "Bearing");
            WeakEventManager<Radial, PropertyChangedEventArgs>.AddHandler(this, "PropertyChanged", PropertyChangedHandler);
        }

        void PropertyChangedHandler(object sender, PropertyChangedEventArgs args)
        {
            switch (args.PropertyName)
            {
                case "Bearing":
                case "Length":
                case "TransmissionLoss":
                    CheckForErrors();
                    break;
                case "IsDeleted":
                    ((INotifyPropertyChanged)sender).PropertyChanged -= PropertyChangedHandler;
                    break;
            }
        }

        public Radial(Radial radial) : this()
        {
            IsCalculated = radial.IsCalculated;
            CalculationStarted = new DateTime(radial.CalculationStarted.Ticks);
            CalculationCompleted = new DateTime(radial.CalculationCompleted.Ticks);
            Bearing = radial.Bearing;
            Length = radial.Length;
        }

        public void CopyFiles(Radial radial)
        {
            var files = Directory.GetFiles(Path.GetDirectoryName(radial.BasePath), Path.GetFileNameWithoutExtension(radial.BasePath) + ".*");
            foreach (var file in files) File.Copy(file, BasePath + Path.GetExtension(file));
        }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public bool IsCalculated { get; set; }
        public string Filename { get; set; }
        public DbDateTime CalculationStarted { get; set; }
        public DbDateTime CalculationCompleted { get; set; }

        /// <summary>
        ///   In degrees, clockwise from true north
        /// </summary>
        [Affects("Segment")]public double Bearing { get; set; }

        /// <summary>
        ///   In meters
        /// </summary>
        [Affects("Segment")]public double Length { get; set; }

        public virtual TransmissionLoss TransmissionLoss { get; set; }

        /// <summary>
        /// Returns the size on disk of all files used by this radial
        /// </summary>
        [NotMapped]
        public long FileSize
        {
            get
            {
                if (string.IsNullOrEmpty(Filename) || TransmissionLoss == null || TransmissionLoss.AnalysisPoint == null || TransmissionLoss.AnalysisPoint.Scenario == null || string.IsNullOrEmpty(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath)) return 0;
                if (!Directory.Exists(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath)) return 0;
                var files = Directory.GetFiles(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath, Filename + ".*");
                return files.Select(file => new FileInfo(file)).Select(fi => fi.Length).Sum();
            }
        }

        [NotMapped] public string BasePath
        {
            get
            {
                if (Filename == null || Filename.EndsWith(".shd")) Filename = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
                return TransmissionLoss == null || TransmissionLoss.AnalysisPoint == null ? null : Path.Combine(TransmissionLoss.AnalysisPoint.Scenario.StorageDirectoryPath, Filename);
            }
        }

        [NotMapped] public float[] Ranges
        {
            get
            {
                if (_ranges == null) ReadAxisFile();
                return _ranges;
            }
        }
        float[] _ranges;

        [NotMapped] public float[] Depths
        {
            get
            {
                if (_depths == null) ReadAxisFile();
                return _depths;
            }
        }
        float[] _depths;

        public void Delete()
        {
            if (IsDeleted) return;
            IsDeleted = true;
            ((INotifyPropertyChanged)this).PropertyChanged -= PropertyChangedHandler;
            CleanupFiles();
            Globals.TransmissionLossCalculatorService.Remove(this);
            TransmissionLoss.Radials.Remove(this);
            Globals.MasterDatabaseService.Context.Radials.Remove(this);
        }

        public void CleanupFiles()
        {
            var files = Directory.GetFiles(Path.GetDirectoryName(BasePath), Path.GetFileNameWithoutExtension(BasePath) + ".*");
            foreach (var file in files) FileDeleteWithRetry(file);
        }

        static readonly TimeSpan FileDeleteDelayTime = new TimeSpan(0, 0, 0, 0, 50);
        static async void FileDeleteWithRetry(string fileName, int retryCount = 20)
        {
            while (retryCount > 0)
            {
                var deleteFailed = false;
                try
                {
                    if (File.Exists(fileName)) File.Delete(fileName);
                }
                catch (IOException)
                {
                    deleteFailed = true;
                }
                catch (UnauthorizedAccessException)
                {
                    deleteFailed = true;
                }
                if (!deleteFailed) break;
                await Task.Delay(FileDeleteDelayTime);
                retryCount--;
            }
        }

        [NotMapped] public BottomProfilePoint[] BottomProfile { get { return _bottomProfile ?? (_bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty")); } }
        BottomProfilePoint[] _bottomProfile;

        [NotMapped] public GeoSegment Segment { get { return new GeoSegment(TransmissionLoss.AnalysisPoint.Geo, Length, Bearing); } }

        [NotMapped] public bool HasErrors { get; private set; }

        [NotMapped] public string Errors { get; private set; }

        public void CheckForErrors()
        {
            if (TransmissionLoss == null || TransmissionLoss.AnalysisPoint == null || TransmissionLoss.AnalysisPoint.Geo == null || TransmissionLoss.AnalysisPoint.Scenario.Location == null) return;
            if (Length <= 0)
            {
                Errors = String.Format("Radial at bearing {0:0.##} has an invalid length ({1:0:0.#}m)", Bearing, Length);
                HasErrors = true;
            }
            else
            {
                var segment = Segment;
                var geoRect = (GeoRect)TransmissionLoss.AnalysisPoint.Scenario.Location.GeoRect;
                if (!geoRect.Contains(segment[1]))
                {
                    Errors = String.Format("Radial at bearing {0:0.##} degrees extends outside the location boundary", Bearing);
                    //Debug.WriteLine(Errors);
                    HasErrors = true;
                }
                else
                {
                    Errors = string.Empty;
                    HasErrors = false;
                }
            }
            TransmissionLoss.CheckForErrors();
        }

        [NotMapped] public bool IsDeleted { get; private set; }

        [NotMapped] public float[] BottomDepths
        {
            get
            {
                if (_bottomDepths == null) ReadAxisFile();
                return _bottomDepths;
            }
        }
        float[] _bottomDepths;

        [NotMapped] public float[] MinimumTransmissionLossValues
        {
            get
            {
                if (_minimumTransmissionLossValues == null) ReadAxisFile();
                return _minimumTransmissionLossValues;
            }
        }

        float[] _minimumTransmissionLossValues;

        [NotMapped] public float[] MaximumTransmissionLossValues
        {
            get
            {
                if (_maximumTransmissionLossValues == null) ReadAxisFile();
                return _maximumTransmissionLossValues;
            }
        }

        float[] _maximumTransmissionLossValues;

        [NotMapped] public float[] MeanTransmissionLossValues
        {
            get
            {
                if (_meanTransmissionLossValues == null) ReadAxisFile();
                return _meanTransmissionLossValues;
            }
        }

        float[] _meanTransmissionLossValues;

        public async Task<Radial> LoadAsync()
        {
            var result = new Task(() =>
            {
                if (_bottomProfile == null) _bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty");
                if (_ranges == null || _depths == null) ReadAxisFile();
                if (_shadeFile == null) _shadeFile = ShadeFile.Read(BasePath + ".shd", (float)Bearing);
                if (_bottomDepths != null) _shadeFile.BottomDepths = _bottomDepths;
            });
            result.Start();
            await Task.WhenAll(result);
            return this;
        }

        ShadeFile _shadeFile;

        [NotMapped]
        public ShadeFile ShadeFile
        {
            get
            {
                if (_shadeFile != null) return _shadeFile;
                _shadeFile = ShadeFile.Read(BasePath + ".shd", (float)Bearing);
                return _shadeFile;
            }
        }

        public bool ExtractAxisData(int debugIndex = -1)
        {
            try
            {
                try
                {
                    if (BasePath == null || File.Exists(BasePath + ".axs") || !File.Exists(BasePath + ".shd")) return false;
                    if (_shadeFile == null) _shadeFile = ShadeFile.Read(BasePath + ".shd", (float)Bearing);
                    if (_bottomDepths != null) _shadeFile.BottomDepths = _bottomDepths;
                }
                catch (EndOfStreamException)
                {
                    FileDeleteWithRetry(BasePath + ".shd");
                    Globals.TransmissionLossCalculatorService.Add(this);
                    return false;
                }
                _ranges = _shadeFile.ReceiverRanges.ToArray();
                _depths = _shadeFile.ReceiverDepths.ToArray();
                IsCalculated = true;
                _bottomProfile = ESME.TransmissionLoss.Bellhop.BottomProfile.FromBellhopFile(BasePath + ".bty");
                _minimumTransmissionLossValues = new float[Ranges.Length];
                _maximumTransmissionLossValues = new float[Ranges.Length];
                _meanTransmissionLossValues = new float[Ranges.Length];
                _bottomDepths = new float[Ranges.Length];
                for (var rangeIndex = 0; rangeIndex < Ranges.Length; rangeIndex++)
                {
                    //if (debugIndex >= 0 && rangeIndex == debugIndex) Debugger.Break();
                    // Updated to ignore values below the bottom
                    var curRange = _ranges[rangeIndex];
                    var depthAtThisRange = (from profilePoint in BottomProfile
                                            let desiredRange = Math.Abs((profilePoint.Range * 1000) - curRange)
                                            orderby desiredRange
                                            select profilePoint.Depth).Take(2).Min();
                    _bottomDepths[rangeIndex] = (float)depthAtThisRange;
                    if (depthAtThisRange <= _depths[0])
                    {
                        MinimumTransmissionLossValues[rangeIndex] = float.NaN;
                        MaximumTransmissionLossValues[rangeIndex] = float.NaN;
                        MeanTransmissionLossValues[rangeIndex] = float.NaN;
                        continue;
                    }
                    var bottomDepthIndex = _depths.ToList().IndexOf((from depth in _depths
                                                                     let curDepth = depthAtThisRange - depth
                                                                     where curDepth > 0
                                                                     orderby curDepth
                                                                     select depth).First());
                    var tlValuesAboveBottom = _shadeFile[rangeIndex].Take(bottomDepthIndex).Where(v => !float.IsNaN(v) && !float.IsInfinity(v)).ToList();
                    if (tlValuesAboveBottom.Count == 0)
                    {
                        MinimumTransmissionLossValues[rangeIndex] = float.NaN;
                        MaximumTransmissionLossValues[rangeIndex] = float.NaN;
                        MeanTransmissionLossValues[rangeIndex] = float.NaN;
                        continue;
                    }
                    if (debugIndex >= 0 && rangeIndex == debugIndex)
                    {
                        var maxTransmissionLoss = tlValuesAboveBottom.Max();
                        var maxTransmissionLossDepthIndex = _shadeFile[rangeIndex].IndexOf(maxTransmissionLoss);
                        var maxTransmissionLossDepth = _depths[maxTransmissionLossDepthIndex];
                        Debug.WriteLine("Maximum TL value for this field found at radial bearing {0}, range {1}, depth {2}, TL {3}, bottom depth at this range {4}", Bearing, curRange, maxTransmissionLossDepth, maxTransmissionLoss, depthAtThisRange);
                    }
                    MinimumTransmissionLossValues[rangeIndex] = tlValuesAboveBottom.Min();
                    MaximumTransmissionLossValues[rangeIndex] = tlValuesAboveBottom.Max();
                    MeanTransmissionLossValues[rangeIndex] = tlValuesAboveBottom.Average();
                }
                _shadeFile.BottomDepths = _bottomDepths;
                using (var writer = new BinaryWriter(new FileStream(BasePath + ".axs", FileMode.Create)))
                {
                    writer.Write(_ranges.Length);
                    foreach (var range in _ranges) writer.Write(range);
                    writer.Write(_depths.Length);
                    foreach (var depth in _depths) writer.Write(depth);
                    writer.Write(_minimumTransmissionLossValues.Length);
                    foreach (var tl in _minimumTransmissionLossValues) writer.Write(tl);
                    writer.Write(_maximumTransmissionLossValues.Length);
                    foreach (var tl in _maximumTransmissionLossValues) writer.Write(tl);
                    writer.Write(_meanTransmissionLossValues.Length);
                    foreach (var tl in _meanTransmissionLossValues) writer.Write(tl);
                    writer.Write(_bottomDepths.Length);
                    foreach (var depth in _bottomDepths) writer.Write(depth);
                }
                //MediatorMessage.Send(MediatorMessage.TransmissionLossLayerChanged, TransmissionLoss);
                return true;
            }
            catch (Exception)
            {
                if (IsDeleted) return false;
                throw;
            }
        }

        public void ReleaseAxisData()
        {
            _ranges = null;
            _depths = null;
            _bottomProfile = null;
            _minimumTransmissionLossValues = null;
            _maximumTransmissionLossValues = null;
            _shadeFile = null;
        }

        void ReadAxisFile()
        {
            if (!File.Exists(BasePath + ".axs")) ExtractAxisData();
            try
            {
                using (var reader = new BinaryReader(new FileStream(BasePath + ".axs", FileMode.Open)))
                {
                    _ranges = new float[reader.ReadInt32()];
                    for (var i = 0; i < _ranges.Length; i++) _ranges[i] = reader.ReadSingle();
                    _depths = new float[reader.ReadInt32()];
                    for (var i = 0; i < _depths.Length; i++) _depths[i] = reader.ReadSingle();

                    _minimumTransmissionLossValues = new float[reader.ReadInt32()];
                    for (var i = 0; i < _minimumTransmissionLossValues.Length; i++) _minimumTransmissionLossValues[i] = reader.ReadSingle();

                    _maximumTransmissionLossValues = new float[reader.ReadInt32()];
                    for (var i = 0; i < _maximumTransmissionLossValues.Length; i++) _maximumTransmissionLossValues[i] = reader.ReadSingle();

                    _meanTransmissionLossValues = new float[reader.ReadInt32()];
                    for (var i = 0; i < _meanTransmissionLossValues.Length; i++) _meanTransmissionLossValues[i] = reader.ReadSingle();

                    _bottomDepths = new float[reader.ReadInt32()];
                    for (var i = 0; i < _bottomDepths.Length; i++) _bottomDepths[i] = reader.ReadSingle();
                }
            }
            catch (EndOfStreamException)
            {
                ExtractAxisData();
            }
        }

        public void Recalculate()
        {
            IsCalculated = false;
            _bottomProfile = null;
            _shadeFile = null;
            _depths = _ranges = null;
            _minimumTransmissionLossValues = _maximumTransmissionLossValues = _meanTransmissionLossValues = null;
            var files = Directory.GetFiles(Path.GetDirectoryName(BasePath), Path.GetFileNameWithoutExtension(BasePath) + ".*");
            foreach (var file in files) FileDeleteWithRetry(file);
            Globals.TransmissionLossCalculatorService.Add(this);
        }

        public override string ToString() { return string.Format("Bearing: {0:0.#}deg | Length: {1:0.#}m", Bearing, Length); }
    }
}