using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using ESME.Model;
using FileFormatException = System.IO.FileFormatException;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossField
    {
        public float LatitudeDegrees { get; private set; }
        public float LongitudeDegrees { get; private set; }
        public float SourceDepthMeters { get; private set; }
        public float VerticalBeamWidthDegrees { get; private set; }
        public float VerticalLookAngleDegrees { get; private set; }
        public float LowFrequencyHz { get; private set; }
        public float HighFrequencyHz { get; private set; }
        public float MaxCalculationDepthMeters { get; private set; }
        public long RadiusMeters { get; private set; }
        public float[] DepthsMeters { get;  set; }
        public float[] RangesMeters { get;  set; }
        public TransmissionLossRadial[] Radials { get; private set; }
        public string Filename { get; set; }

        public static TransmissionLossField LoadHeader(string filename)
        {
            return new TransmissionLossField(filename, true);
        }

        public static TransmissionLossField Load(string filename)
        {
            return new TransmissionLossField(filename, false);
        }

#if true
        public TransmissionLossField(BellhopRunFile runFile)
        {
            LatitudeDegrees = (float) runFile.TransmissionLossJob.NewAnalysisPoint.Location.Latitude_degrees;
            LongitudeDegrees = (float) runFile.TransmissionLossJob.NewAnalysisPoint.Location.Longitude_degrees;
            SourceDepthMeters = runFile.TransmissionLossJob.AcousticProperties.SourceDepth_meters;
            VerticalBeamWidthDegrees = runFile.TransmissionLossJob.AcousticProperties.VerticalBeamWidth_degrees;
            VerticalLookAngleDegrees =
                runFile.TransmissionLossJob.AcousticProperties.DepressionElevationAngle_degrees;
            LowFrequencyHz = runFile.TransmissionLossJob.AcousticProperties.LowFrequency_Hz;
            HighFrequencyHz = runFile.TransmissionLossJob.AcousticProperties.HighFrequency_Hz;
            MaxCalculationDepthMeters = runFile.TransmissionLossJob.MaxDepth;
            RadiusMeters = runFile.TransmissionLossJob.Radius;
            //DepthsMeters = runFile.
            //RangesMeters = runFile.
            //Filename = Path.Combine(Field.DataDirectoryPath, Field.BinaryFileName);
        }
#endif

        public TransmissionLossField(string filename, bool loadHeadersOnly)
        {
            Filename = filename;
            Load(loadHeadersOnly);
        }

        public void Load(bool loadHeadersOnly)
        {
            int i;
            bool eof = false;

            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Load()");
            using (var stream = new BinaryReader(File.Open(Filename, FileMode.Open)))
            {
                if (stream.ReadUInt32() != Magic)
                    throw new FileFormatException(
                        "Attempted to read invalid data into a TransmissionLossFieldData object");
                LatitudeDegrees = stream.ReadSingle();
                LongitudeDegrees = stream.ReadSingle();
                SourceDepthMeters = stream.ReadSingle();
                VerticalBeamWidthDegrees = stream.ReadSingle();
                VerticalLookAngleDegrees = stream.ReadSingle();
                LowFrequencyHz = stream.ReadSingle();
                HighFrequencyHz = stream.ReadSingle();
                MaxCalculationDepthMeters = stream.ReadSingle();
                RadiusMeters = stream.ReadInt32();
                DepthsMeters = new float[stream.ReadInt32()];
                for (i = 0; i < DepthsMeters.Length; i++)
                    DepthsMeters[i] = stream.ReadSingle();
                RangesMeters = new float[stream.ReadInt32()];
                for (i = 0; i < RangesMeters.Length; i++)
                    RangesMeters[i] = stream.ReadSingle();
                _mSaved = true;
                _mRadials.Clear();
                try
                {
                    while (!eof)
                        _mRadials.Add(new TransmissionLossRadial(stream, loadHeadersOnly));
                }
                catch (EndOfStreamException)
                {
                    eof = true;
                }
                _mRadials.Sort();
                Radials = _mRadials.ToArray();
            }
        }

        public void AddRadial(TransmissionLossRadial radial)
        {
            if (radial.TransmissionLoss_dBSPL == null)
                throw new ApplicationException(
                    "TransmissionLossFieldData: Attempt to add a new radial that is not completely loaded into memory.  This operation is not supported.");
            _mRadials.Add(radial);
            _mRadials.Sort();
            Radials = _mRadials.ToArray();
        }

        public void Save(bool discardRadialDataAfterSave)
        {
            bool saveSucceeded = false;
            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Save()");
            if (!Directory.Exists(Path.GetDirectoryName(Filename)))
                Directory.CreateDirectory(Path.GetDirectoryName(Filename));

            for (int retry = 0; retry < 10; retry++)
            {
                try
                {
                    using (var stream = new BinaryWriter(File.Open(Filename, FileMode.OpenOrCreate, FileAccess.Write)))
                    {
                        if (!_mSaved)
                        {
                            stream.Write(Magic);
                            stream.Write(LatitudeDegrees);
                            stream.Write(LongitudeDegrees);
                            stream.Write(SourceDepthMeters);
                            stream.Write(VerticalBeamWidthDegrees);
                            stream.Write(VerticalLookAngleDegrees);
                            stream.Write(LowFrequencyHz);
                            stream.Write(HighFrequencyHz);
                            stream.Write(MaxCalculationDepthMeters);
                            stream.Write(RadiusMeters);
                            stream.Write(DepthsMeters.Length);
                            foreach (float depth in DepthsMeters)
                                stream.Write(depth);
                            stream.Write(RangesMeters.Length);
                            foreach (float range in RangesMeters)
                                stream.Write(range);
                            _mSaved = true;
                        }
                        foreach (TransmissionLossRadial radial in _mRadials)
                            radial.Save(stream, discardRadialDataAfterSave);
                        saveSucceeded = true;
                        break;
                    }
                }
                catch (IOException)
                {
                    Thread.Sleep(1000);
                }
            }
            if (!saveSucceeded)
                throw new IOException("FieldData: Could not save file.  Retry count exhausted.");
        }

        private const UInt32 Magic = 0x99f84752;
        private readonly List<TransmissionLossRadial> _mRadials = new List<TransmissionLossRadial>();
        private bool _mSaved;
    }
}