using System;
using System.Collections.Generic;
using System.IO;
using System.Windows;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.RAM;
using HRC.Navigation;
using HRC.Utility;
using FileFormatException = System.IO.FileFormatException;
using RangeComplex = ESME.Environment.Descriptors.RangeComplex;

namespace ESME.TransmissionLoss
{
    public abstract class TransmissionLossRunFile : IEquatable<TransmissionLossField>
    {
        protected static readonly List<Type> ReferencedTypes = new List<Type> { typeof(TransmissionLossAlgorithm), typeof(EarthCoordinate), typeof(AnalysisPoint), typeof(AcousticProperties), typeof(TransmissionLossRunFile), typeof(Point) };

        protected TransmissionLossRunFile()
        {
            TransmissionLossRunFileRadials = new List<TransmissionLossRunFileRadial>();
        }

        public static TransmissionLossRunFile Create(TransmissionLossAlgorithm algorithm, SoundSource soundSource, string rangeComplexName, RangeComplexes rangeComplexes, string platformName, string sourceName, string modeName, string timePeriodName, RangeComplex rangeComplex)
        {
            TransmissionLossRunFile result = null;
            string fileType = null;
            switch (algorithm)
            {
                case TransmissionLossAlgorithm.Bellhop:
                    result = new BellhopRunFile
                    {
                        BellhopSettings = Globals.AppSettings.BellhopSettings,
                        WaterDepthIncrement = Globals.AppSettings.BellhopSettings.DepthCellSize,
                        RangeDistanceIncrement = Globals.AppSettings.BellhopSettings.RangeCellSize,
                    };
                    fileType = "bellhop";
                    break;
                case TransmissionLossAlgorithm.RAMGEO:
                    result = new RamRunFile
                    {
                        RAMSettings = Globals.AppSettings.RAMSettings,
                        WaterDepthIncrement = Globals.AppSettings.RAMSettings.DepthStepSize,
                        RangeDistanceIncrement = Globals.AppSettings.RAMSettings.RangeStepSize,
                    };
                    fileType = "ramgeo";
                    break;
                case TransmissionLossAlgorithm.RAM:
                case TransmissionLossAlgorithm.REFMS:
                case TransmissionLossAlgorithm.CASS:
                    throw new FileFormatException(string.Format("TransmissionLossRunFile.Create: Transmission loss algorithm {0} is not supported", algorithm));
            }
            if (result == null) throw new ApplicationException("Result is null");
            result.TransmissionLossAlgorithm = algorithm;
            result.ScenarioDataDirectory = Globals.AppSettings.ScenarioDataDirectory;
            result.TransmissionLossJob = new TransmissionLossJob
            {
                SoundSource = soundSource,
                PlatformName = platformName,
                SourceName = sourceName,
                ModeName = modeName,
                TimePeriodName = timePeriodName,
            };
            result.RangeComplexName = rangeComplexes.SelectedRangeComplex.Name;
            result.TimePeriod = rangeComplexes.SelectedTimePeriod;
            result.AreaName = rangeComplexes.SelectedArea.Name;
            result.BathymetryResolution = rangeComplexes.SelectedBathymetry.Name;
            result.ReferenceLocation = new EarthCoordinate(rangeComplex.RangeComplexMetadata.Latitude, rangeComplex.RangeComplexMetadata.Longitude);
            var lat = soundSource.Latitude;
            var lon = soundSource.Longitude;
            var locationString = string.Format("{0}{1:0.####}_{2}{3:0.####}",
                                               lat >= 0 ? "n" : "s", Math.Abs(lat),
                                               lon >= 0 ? "e" : "w", Math.Abs(lon));
            result.Filename = string.Format("{0}_{1}_{2}_{3}_{4}_{5}.{6}", rangeComplexName, platformName, sourceName, modeName,
                timePeriodName, locationString, fileType);

            return result;
        }

        public static TransmissionLossRunFile Load(string filename)
        {
            if (string.IsNullOrEmpty(filename)) throw new ArgumentNullException("filename", "TransmissionLossRunFile.Load: filename is null");
            TransmissionLossRunFile result;
            switch (Path.GetExtension(filename).ToLower())
            {
                case ".bellhop":
                    result = XmlSerializer<BellhopRunFile>.Load(filename, ReferencedTypes);
                    break;
                case ".ramgeo":
                    result = XmlSerializer<RamRunFile>.Load(filename, ReferencedTypes);
                    break;
                default:
                    throw new FileFormatException(string.Format("TransmissionLossRunFile.Load: Transmission loss algorithm {0} is not supported", Path.GetExtension(filename).ToLower()));
            }
            result.Filename = filename;
            return result;
        }

        public string ScenarioDataDirectory { get; set; }
        public string RangeComplexName { get; set; }
        public string AreaName { get; set; }
        public string BathymetryResolution { get; set; }
        public TimePeriod TimePeriod { get; set; }
        public TransmissionLossAlgorithm TransmissionLossAlgorithm { get; set; }
        public float WaterDepthIncrement { get; set; }
        public float RangeDistanceIncrement { get; set; }
        public EarthCoordinate ReferenceLocation { get; set; }
#if false
        public static TransmissionLossRunFile Create(TransmissionLossAlgorithm transmissionLossAlgorithm, TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, AppSettings appSettings)
        {
            var result = new TransmissionLossRunFile
            {
                TransmissionLossAlgorithm = transmissionLossAlgorithm,
                TransmissionLossJob = transmissionLossJob,
            };
            switch (transmissionLossAlgorithm)
            {
                case TransmissionLossAlgorithm.Bellhop:
                    result = BellhopRunFile.Create(transmissionLossJob, environmentInformation, appSettings);
                    break;
                case TransmissionLossAlgorithm.RAMGEO:
                    result = RamRunFile.Create(transmissionLossJob, environmentInformation, appSettings);
                    break;
                case TransmissionLossAlgorithm.CASS:
                case TransmissionLossAlgorithm.REFMS:
                case TransmissionLossAlgorithm.RAM:
                default:
                    throw new NotImplementedException(string.Format("Creating a TransmissionLossRunFile using an algorithm of {0} is not currently supported", transmissionLossAlgorithm));
            }
            result.Filename = transmissionLossJob.Filename;
            return result;
        }
#endif

        public abstract void Save(string fileName = null);

        TransmissionLossJob _transmissionLossJob;
        public TransmissionLossJob TransmissionLossJob
        {
            get { return _transmissionLossJob; }
            set
            {
                if (value == _transmissionLossJob) return;
                _transmissionLossJob = value;
                Metadata = _transmissionLossJob.Metadata;
            }
        }

        protected string RandomFileName(string path, string extension)
        {
            return Path.Combine(path, Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + extension);
        }

        public string Metadata { get; set; }
        [XmlIgnore]
        public string Filename { get; set; }

        public List<TransmissionLossRunFileRadial> TransmissionLossRunFileRadials { get; set; }
        public bool Equals(TransmissionLossField other) { return other.Equals(TransmissionLossJob.SoundSource); }
    }
}
