using System;
using System.Collections.Generic;
using System.IO;
using ESME.Environment;
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
                                                                   typeof (EarthCoordinate), typeof (AnalysisPoint), typeof (AcousticProperties), typeof (TransmissionLossRunFile), typeof(BellhopRunFile), typeof(RamRunFile), typeof(BellhopRunFileRadial), typeof(RamRunFileRadial)
                                                               });
            }
        }

        static Type[] _referencedTypes;

        public static TransmissionLossRunFile Load(string filename)
        {
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

        public static TransmissionLossRunFile Create(TransmissionLossAlgorithm transmissionLossAlgorithm, TransmissionLossJob transmissionLossJob, EnvironmentInformation environmentInformation, TransmissionLossSettings transmissionLossSettings)
        {
            switch (transmissionLossAlgorithm)
            {
                case TransmissionLossAlgorithm.Bellhop:
                    return BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                case TransmissionLossAlgorithm.RAM:
                    return RamRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                case TransmissionLossAlgorithm.CASS:
                case TransmissionLossAlgorithm.REFMS:
                default:
                    throw new NotImplementedException(string.Format("Creating a TransmissionLossRunFile using an algorithm of {0} is not currently supported", transmissionLossAlgorithm));
            }
        }
#if false
        public static TransmissionLossRunFile Create(TransmissionLossAlgorithm transmissionLossAlgorithm, SoundSource soundSource, Environment2DData bathymetry, Environment2DData bottomType, SoundSpeedField soundSpeedField, Environment2DData wind, TransmissionLossSettings transmissionLossSettings)
        {
            switch (transmissionLossAlgorithm)
            {
                case TransmissionLossAlgorithm.Bellhop:
                    return BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                case TransmissionLossAlgorithm.RAM:
                    return RamRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                case TransmissionLossAlgorithm.CASS:
                case TransmissionLossAlgorithm.REFMS:
                default:
                    throw new NotImplementedException(string.Format("Creating a TransmissionLossRunFile using an algorithm of {0} is not currently supported", transmissionLossAlgorithm));
            }
        }
#endif

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

        public List<TransmissionLossRunFileRadial> TransmissionLossRunFileRadials { get; set; }
    }
}
