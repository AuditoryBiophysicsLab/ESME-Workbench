using System;
using System.Collections.Generic;
using System.IO;
using ESME.Model;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossField : IHasIDField
    {
        public string Name { get; private set; }
        public string Metadata { get; private set; }
        public ulong IDField { get; set; }
        public float SourceLevel { get; private set; }
        public float Latitude { get; private set; }
        public float Longitude { get; private set; }
        public float SourceDepth { get; private set; }
        public float VerticalBeamWidth { get; private set; }
        public float VerticalLookAngle { get; private set; }
        public float LowFrequency { get; private set; }
        public float HighFrequency { get; private set; }
        public float MaxCalculationDepth { get; private set; }
        public int Radius { get; private set; }
        public float[] Depths { get;  set; }
        public float[] Ranges { get;  set; }
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

        public TransmissionLossField(BellhopRunFile runFile)
        {
            Name = runFile.Name ?? "";
            Metadata = runFile.Metadata ?? "";
            SourceLevel = runFile.TransmissionLossJob.SourceLevel;
            Latitude = (float) runFile.TransmissionLossJob.AnalysisPoint.Location.Latitude_degrees;
            Longitude = (float) runFile.TransmissionLossJob.AnalysisPoint.Location.Longitude_degrees;
            SourceDepth = runFile.TransmissionLossJob.AcousticProperties.SourceDepth;
            VerticalBeamWidth = runFile.TransmissionLossJob.AcousticProperties.VerticalBeamWidth;
            VerticalLookAngle = runFile.TransmissionLossJob.AcousticProperties.DepressionElevationAngle;
            LowFrequency = runFile.TransmissionLossJob.AcousticProperties.LowFrequency;
            HighFrequency = runFile.TransmissionLossJob.AcousticProperties.HighFrequency;
            MaxCalculationDepth = runFile.TransmissionLossJob.MaxDepth;
            Radius = runFile.TransmissionLossJob.Radius;
            IDField = runFile.IDField;
            //Depths = runFile.
            //Ranges = runFile.
            //Filename = Path.Combine(Field.DataDirectoryPath, Field.BinaryFileName);
        }

        public TransmissionLossField()
        {} 

        public TransmissionLossField(string filename, bool loadHeadersOnly)
        {
            Filename = filename;
            Load(loadHeadersOnly);
        }

        public void Load(bool loadHeadersOnly)
        {
            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Load()");
            using (var stream = new BinaryReader(File.Open(Filename, FileMode.Open)))
            {
                if (stream.ReadUInt32() != Magic)
                    throw new FileFormatException(
                        "Attempted to read invalid data into a TransmissionLossFieldData object");
                Name = stream.ReadString();
                Metadata = stream.ReadString();
                SourceLevel = stream.ReadSingle();
                IDField = stream.ReadUInt64();
                Latitude = stream.ReadSingle();
                Longitude = stream.ReadSingle();
                SourceDepth = stream.ReadSingle();
                VerticalBeamWidth = stream.ReadSingle();
                VerticalLookAngle = stream.ReadSingle();
                LowFrequency = stream.ReadSingle();
                HighFrequency = stream.ReadSingle();
                MaxCalculationDepth = stream.ReadSingle();
                Radius = stream.ReadInt32();
                var depthCount = stream.ReadInt32();
                Depths = new float[depthCount];
                for (var i = 0; i < Depths.Length; i++)
                    Depths[i] = stream.ReadSingle();
                var rangeCount = stream.ReadInt32();
                Ranges = new float[rangeCount];
                for (var i = 0; i < Ranges.Length; i++)
                    Ranges[i] = stream.ReadSingle();
                var radialCount = stream.ReadInt32();
                for (var j = 0; j < radialCount; j++)
                    _mRadials.Add(new TransmissionLossRadial(stream)
                                 {
                                     Ranges = Ranges,
                                     Depths = Depths
                                 });
                _mSaved = true;
                _mRadials.Sort();
                Radials = _mRadials.ToArray();
            }
        }

        public void AddRadial(TransmissionLossRadial radial)
        {
            if (radial.TransmissionLoss == null)
                throw new ApplicationException(
                    "TransmissionLossFieldData: Attempt to add a new radial that is not completely loaded into memory.  This operation is not supported.");
            if (((Depths == null) || (Ranges == null)) &&
                ((radial.Depths != null) && (radial.Ranges != null)))
            {
                Depths = radial.Depths;
                Ranges = radial.Ranges;
            }
            _mRadials.Add(radial);
            _mRadials.Sort();
            Radials = _mRadials.ToArray();
        }

        public void Save()
        {
            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Save()");
            if (!Directory.Exists(Path.GetDirectoryName(Filename)))
                Directory.CreateDirectory(Path.GetDirectoryName(Filename));

            using (var stream = new BinaryWriter(File.Open(Filename, FileMode.Create, FileAccess.Write)))
            {
                if (!_mSaved)
                {
                    stream.Write(Magic);
                    stream.Write(Name);
                    stream.Write(Metadata);
                    stream.Write(SourceLevel);
                    stream.Write(IDField);
                    stream.Write(Latitude);
                    stream.Write(Longitude);
                    stream.Write(SourceDepth);
                    stream.Write(VerticalBeamWidth);
                    stream.Write(VerticalLookAngle);
                    stream.Write(LowFrequency);
                    stream.Write(HighFrequency);
                    stream.Write(MaxCalculationDepth);
                    stream.Write(Radius);
                    stream.Write(Depths.Length);
                    foreach (var depth in Depths)
                        stream.Write(depth);
                    stream.Write(Ranges.Length);
                    foreach (var range in Ranges)
                        stream.Write(range);
                    _mSaved = true;
                }
                stream.Write(_mRadials.Count);
                foreach (var radial in _mRadials)
                    radial.Save(stream);
            }
        }

        private const UInt32 Magic = 0x93f34525;
        private readonly List<TransmissionLossRadial> _mRadials = new List<TransmissionLossRadial>();
        private bool _mSaved;

    }
#if false
    //todo: impelement properly -- > called in ESME_Experiment.cs
    public class TransmissionLossFieldList : List<TransmissionLossField>
    {
        internal void Initialize(NewAnalysisPointList analysisPoints)
        {

            foreach (var f in this)
                f.AnalysisPointID = analysisPoints.Find(a => a.IDField == f.AnalysisPointID);
        }

    } 
#endif
}