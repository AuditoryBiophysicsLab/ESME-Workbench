using System;
using System.Collections.Generic;
using System.IO;
using System.Windows;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.RAM;
using HRC.Navigation;
using HRC.Utility;
using FileFormatException = System.IO.FileFormatException;

namespace ESME.TransmissionLoss
{
    public abstract class TransmissionLossRunFile : IEquatable<TransmissionLossField>
    {
        protected static readonly List<Type> ReferencedTypes = new List<Type> { typeof(TransmissionLossAlgorithm), typeof(Geo), typeof(AnalysisPoint), typeof(AcousticProperties), typeof(TransmissionLossRunFile), typeof(Point) };

        protected TransmissionLossRunFile()
        {
            TransmissionLossRunFileRadials = new List<TransmissionLossRunFileRadial>();
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
        public Geo ReferenceLocation { get; set; }

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
