using System;
using System.Collections.Generic;
using System.IO;
using ESME.Model;
using ESME.NEMO;
using ESME.TransmissionLoss.Bellhop;
using HRC.Navigation;
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
        public float DepressionElevationAngle { get; private set; }
        public float LowFrequency { get; private set; }
        public float HighFrequency { get; private set; }
        public float MaxCalculationDepth { get; private set; }
        public int Radius { get; private set; }
        public float[] Depths { get;  set; }
        public float[] Ranges { get;  set; }
        public TransmissionLossRadial[] Radials { get; private set; }
        public string Filename { get; set; }
        public EarthCoordinate EarthCoordinate { get; private set; }

        public static TransmissionLossField LoadHeader(string filename)
        {
            return new TransmissionLossField(filename, true);
        }

        public static TransmissionLossField Load(string filename)
        {
            return new TransmissionLossField(filename, false);
        }

        public bool IsAcousticMatchFor(NemoMode nemoMode)
        {
            //if (nemoMode.PSMName == "SH60B|ALFS|Search") System.Diagnostics.Debugger.Break();
            return (FloatMatch(LowFrequency, nemoMode.LowFrequency, 0.1f)) &&
                   (FloatMatch(HighFrequency, nemoMode.HighFrequency, 0.1f)) &&
                   (FloatMatch(SourceDepth, nemoMode.SourceDepth, 0.1f)) &&
                   (FloatMatch(VerticalBeamWidth, nemoMode.VerticalBeamWidth, 0.1f)) &&
                   (FloatMatch(DepressionElevationAngle, nemoMode.DepressionElevationAngle, 0.1f)) &&
                   (Radius >= nemoMode.Radius) && 
                   (SourceLevel >= nemoMode.SourceLevel);
        }

        static bool FloatMatch(float value1, float value2, float tolerance)
        {
            return Math.Abs(value1 - value2) <= tolerance;
        }

        public float Lookup(float bearing, float range, float depth)
        {
            for (var slice = 0; slice < _pieSlices.Count; slice++)
            {
                var pieSlice = _pieSlices[slice];
                if (pieSlice.Contains(bearing))
                {
                    int sourceRadialIndex;
                    var isLeft = pieSlice.IsLeftCloserTo(bearing);
                    if (slice < (_pieSlices.Count - 1))
                    {
                        if (isLeft) sourceRadialIndex = slice;
                        else sourceRadialIndex = slice + 1;
                    }
                    else
                    {
                        if (isLeft) sourceRadialIndex = _pieSlices.Count - 1;
                        else sourceRadialIndex = 0;
                    }
                    var sourceRadial = Radials[sourceRadialIndex];

                    int sourceRangeIndex;
                    for (sourceRangeIndex = 0; sourceRangeIndex < sourceRadial.Ranges.Length - 1; sourceRangeIndex++)
                        if (sourceRadial.Ranges[sourceRangeIndex + 1] >= range) break;
                    //if (sourceRangeIndex == sourceRadialRanges.Length - 1)

                    
                    int sourceDepthIndex;
                    for (sourceDepthIndex = 0; sourceDepthIndex < sourceRadial.Depths.Length - 1; sourceDepthIndex++)
                        if (sourceRadial.Depths[sourceDepthIndex + 1] >= depth) break;
                    
                    return sourceRadial[sourceDepthIndex, sourceRangeIndex];
                }
            }
            throw new ApplicationException(string.Format("Error looking up value in TransmissionLossField: This field does not contain bearing {0}, range {1}, depth {2}", bearing, range, depth));
        }

        public TransmissionLossField(TransmissionLossRunFile runFile)
        {
            Name = runFile.Name ?? "";
            Metadata = runFile.Metadata ?? "";
            SourceLevel = runFile.TransmissionLossJob.SourceLevel;
            Latitude = (float) runFile.TransmissionLossJob.AnalysisPoint.EarthCoordinate.Latitude_degrees;
            Longitude = (float) runFile.TransmissionLossJob.AnalysisPoint.EarthCoordinate.Longitude_degrees;
            EarthCoordinate = new EarthCoordinate(Latitude, Longitude);
            SourceDepth = runFile.TransmissionLossJob.AcousticProperties.SourceDepth;
            VerticalBeamWidth = runFile.TransmissionLossJob.AcousticProperties.VerticalBeamWidth;
            DepressionElevationAngle = runFile.TransmissionLossJob.AcousticProperties.DepressionElevationAngle;
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

        void Load(bool loadHeadersOnly)
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
                EarthCoordinate = new EarthCoordinate(Latitude, Longitude);
                SourceDepth = stream.ReadSingle();
                VerticalBeamWidth = stream.ReadSingle();
                DepressionElevationAngle = stream.ReadSingle();
                LowFrequency = stream.ReadSingle();
                HighFrequency = stream.ReadSingle();
                MaxCalculationDepth = stream.ReadSingle();
                Radius = stream.ReadInt32();
                if (loadHeadersOnly) return;
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
                    _radials.Add(new TransmissionLossRadial(stream)
                                 {
                                     Ranges = Ranges,
                                     Depths = Depths
                                 });
                for (var j = 0; j < radialCount; j++)
                    _pieSlices.Add(new PieSlice(_radials[j].BearingFromSource, 360.0 / _radials.Count));
                _saved = true;
                _radials.Sort();
                Radials = _radials.ToArray();
                _dataIsLoaded = true;
            }
        }

        public void LoadData()
        {
            if (!_dataIsLoaded)
                Load(false);
        }

        public void DiscardData()
        {
            _dataIsLoaded = false;
            Depths = null;
            Ranges = null;
            _radials = new List<TransmissionLossRadial>(); ;
            _pieSlices = new List<PieSlice>();
            Radials = null;
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
            _radials.Add(radial);
            _radials.Sort();
            Radials = _radials.ToArray();
        }

        public void Save()
        {
            if (Filename == null)
                throw new FileNotFoundException("TransmissionLossFieldData: Specify a filename before calling Save()");
            if (!Directory.Exists(Path.GetDirectoryName(Filename)))
                Directory.CreateDirectory(Path.GetDirectoryName(Filename));

            using (var stream = new BinaryWriter(File.Open(Filename, FileMode.Create, FileAccess.Write)))
            {
                if (!_saved)
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
                    stream.Write(DepressionElevationAngle);
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
                    _saved = true;
                }
                stream.Write(_radials.Count);
                foreach (var radial in _radials)
                    radial.Save(stream);
            }
        }

        const UInt32 Magic = 0x93f34525;
        List<TransmissionLossRadial> _radials = new List<TransmissionLossRadial>();
        List<PieSlice> _pieSlices = new List<PieSlice>();
        bool _dataIsLoaded;
        bool _saved;

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