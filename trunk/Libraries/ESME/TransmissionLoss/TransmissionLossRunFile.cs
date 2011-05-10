using System;
using System.Collections.Generic;
using System.IO;
using System.Windows;
using ESME.Data;
using ESME.Model;
using ESME.TransmissionLoss.Bellhop;
using ESME.TransmissionLoss.RAM;
using HRC.Navigation;

namespace ESME.TransmissionLoss
{
    public abstract class TransmissionLossRunFile : IHasIDField
    {
        protected TransmissionLossRunFile()
        {
            TransmissionLossRunFileRadials = new List<TransmissionLossRunFileRadial>();
        }

        public static Type[] ReferencedTypes
        {
            get
            {
                return _referencedTypes ?? (_referencedTypes = new[]
                                                               {
                                                                   typeof (EarthCoordinate), typeof (AnalysisPoint), typeof (AcousticProperties), typeof (TransmissionLossRunFile), typeof(BellhopRunFile), typeof(RamRunFile), typeof(BellhopRunFileRadial), typeof(RamRunFileRadial), typeof(Point)
                                                               });
            }
        }

        static Type[] _referencedTypes;

        public static TransmissionLossRunFile Load(string filename)
        {
            if (filename == null) throw new ArgumentNullException(@"TransmissionLossRunFile.Load: filename is null");
            var extension = Path.GetExtension(filename).ToLower();
            switch (extension)
            {
                case ".bellhop":
                    return BellhopRunFile.Load(filename);
                case ".ram":
                    return RamRunFile.Load(filename);
                case ".cass":
                default:
                    throw new NotImplementedException(string.Format("Loading a TransmissionLossRunFile with an extension of \"{0}\" is not supported", extension));
            }
        }

        public static TransmissionLossRunFile Create(TransmissionLossAlgorithm transmissionLossAlgorithm, TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, AppSettings appSettings)
        {
            TransmissionLossRunFile result;
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
                default:
                    throw new NotImplementedException(string.Format("Creating a TransmissionLossRunFile using an algorithm of {0} is not currently supported", transmissionLossAlgorithm));
            }
            result.Filename = transmissionLossJob.Filename;
            return result;
        }

        public abstract void Save(string path);

        TransmissionLossJob _transmissionLossJob;
        public TransmissionLossJob TransmissionLossJob
        {
            get { return _transmissionLossJob; }
            set
            {
                if (value == _transmissionLossJob) return;
                _transmissionLossJob = value;
                Name = _transmissionLossJob.Name;
                Metadata = _transmissionLossJob.Metadata;
            }
        }

        protected string RandomFileName(string path, string extension)
        {
            return Path.Combine(path, Path.GetFileNameWithoutExtension(Path.GetRandomFileName()) + extension);
        }

        public string Name { get; set; }
        public string Metadata { get; set; }
        public ulong IDField { get; set; }
        public string Filename { get; set; }

        public List<TransmissionLossRunFileRadial> TransmissionLossRunFileRadials { get; set; }
    }
}
