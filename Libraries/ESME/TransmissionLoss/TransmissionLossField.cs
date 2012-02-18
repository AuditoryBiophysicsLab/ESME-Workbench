using System;
using System.Collections.Generic;
using System.IO;
using ESME.NEMO;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;
using FileFormatException = ESME.Model.FileFormatException;

namespace ESME.TransmissionLoss
{
    public class TransmissionLossField : IEquatable<SoundSource>
    {
        public string Name { get; set; }
        public string Metadata { get; set; }
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
        public List<float> Depths { get; set; }
        public List<float> Ranges { get; set; }
        public List<TransmissionLossRadial> Radials { get; private set; }
        public string Filename { get; set; }
        public Geo Geo { get; private set; }
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
                    for (sourceRangeIndex = 0; sourceRangeIndex < sourceRadial.Ranges.Count - 1; sourceRangeIndex++)
                        if (sourceRadial.Ranges[sourceRangeIndex + 1] >= range) break;
                    //if (sourceRangeIndex == sourceRadialRanges.Length - 1)

                    
                    int sourceDepthIndex;
                    for (sourceDepthIndex = 0; sourceDepthIndex < sourceRadial.Depths.Count - 1; sourceDepthIndex++)
                        if (sourceRadial.Depths[sourceDepthIndex + 1] >= depth) break;
                    
                    return sourceRadial[sourceDepthIndex, sourceRangeIndex];
                }
            }
            throw new ApplicationException(string.Format("Error looking up value in TransmissionLossField: This field does not contain bearing {0}, range {1}, depth {2}", bearing, range, depth));
        }

        public TransmissionLossField(TransmissionLossRunFile runFile)
        {
            Metadata = runFile.Metadata ?? "";
            SourceLevel = runFile.TransmissionLossJob.SoundSource.SourceLevel;
            Latitude = (float)runFile.TransmissionLossJob.SoundSource.Latitude;
            Longitude = (float)runFile.TransmissionLossJob.SoundSource.Longitude;
            Geo = new EarthCoordinate(Latitude, Longitude);
            SourceDepth = runFile.TransmissionLossJob.SoundSource.AcousticProperties.SourceDepth;
            VerticalBeamWidth = runFile.TransmissionLossJob.SoundSource.AcousticProperties.VerticalBeamWidth;
            DepressionElevationAngle = runFile.TransmissionLossJob.SoundSource.AcousticProperties.DepressionElevationAngle;
            LowFrequency = runFile.TransmissionLossJob.SoundSource.AcousticProperties.LowFrequency;
            HighFrequency = runFile.TransmissionLossJob.SoundSource.AcousticProperties.HighFrequency;
            MaxCalculationDepth = runFile.TransmissionLossJob.MaxDepth;
            Radius = runFile.TransmissionLossJob.SoundSource.Radius;
            //Depths = runFile.
            //Ranges = runFile.
            //Filename = Path.Combine(Field.DataDirectoryPath, Field.BinaryFileName);
        }
        
        public TransmissionLossField() {  }

        public static TransmissionLossField FromCASS(string cassFileName, bool headerOnly)
        {
            var cassOutput = CASSOutput.FromBinaryFile(cassFileName, headerOnly);
            return FromCASS(cassOutput);
        }

        public static TransmissionLossField FromCASS(CASSOutput output)
        {
            var result = new TransmissionLossField
            {
                    Name = string.Format("CASS|{0}|{1}|{2}|{3}|{4}", output.PlatformName, output.SourceName, output.ModeName, output.SourceRefLatLocation, output.SourceRefLonLocation),
                    Metadata = string.Format("Imported from CASS output file {0} on {1}", output.Filename, DateTime.Now),
                    SourceLevel = output.SourceLevel,
                    Latitude = output.SourceRefLatLocation,
                    Longitude = output.SourceRefLonLocation,
                    SourceDepth = output.SourceDepth,
                    VerticalBeamWidth = output.VerticalBeamPattern,
                    DepressionElevationAngle = output.DepressionElevationAngle,
                    LowFrequency = output.Frequency,
                    HighFrequency = output.Frequency,
                    MaxCalculationDepth = output.MaxWaterDepth,
                    Radius = (int)output.MaxRangeDistance
            };

            if (output.RadialCount == 0) return result; // because only the header was loaded.
            result.Depths = new List<float>(output.DepthCells);
            result.Ranges = new List<float>(output.RangeCells);
            result.Radials = new List<TransmissionLossRadial>();
            for (var i = 0; i < output.Pressures.Count; i++) result.Radials.Add(new TransmissionLossRadial(output.RadialBearings[i],output.Pressures[i],result.Depths,result.Ranges,output.SourceLevel));
            result.Geo = new EarthCoordinate(result.Latitude,result.Longitude);
            result._dataIsLoaded = true;
            return result;
        }

        public TransmissionLossField(string filename, bool loadHeadersOnly)
        {
            Filename = filename;
            Load(loadHeadersOnly);
        }

        void Load(bool loadHeadersOnly)
        {
            if(_dataIsLoaded) return;
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
                Latitude = stream.ReadSingle();
                Longitude = stream.ReadSingle();
                Geo = new EarthCoordinate(Latitude, Longitude);
                SourceDepth = stream.ReadSingle();
                VerticalBeamWidth = stream.ReadSingle();
                DepressionElevationAngle = stream.ReadSingle();
                LowFrequency = stream.ReadSingle();
                HighFrequency = stream.ReadSingle();
                MaxCalculationDepth = stream.ReadSingle();
                Radius = stream.ReadInt32();
                if (loadHeadersOnly) return;
                var depthCount = stream.ReadInt32();
                Depths = new List<float>();
                for (var i = 0; i < depthCount; i++)
                    Depths.Add(stream.ReadSingle());
                var rangeCount = stream.ReadInt32();
                Ranges = new List<float>();
                for (var i = 0; i < rangeCount; i++)
                    Ranges.Add(stream.ReadSingle());
                var radialCount = stream.ReadInt32();
                Radials = new List<TransmissionLossRadial>();
                for (var j = 0; j < radialCount; j++)
                    Radials.Add(new TransmissionLossRadial(stream)
                                 {
                                     Ranges = Ranges,
                                     Depths = Depths
                                 });
                for (var j = 0; j < radialCount; j++)
                    _pieSlices.Add(new PieSlice(Radials[j].BearingFromSource, 360.0 / radialCount));
                _saved = true;
                Radials.Sort();
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
            Radials = new List<TransmissionLossRadial>();
            _pieSlices = new List<PieSlice>();
            Radials = null;
        }

        public void AddRadial(TransmissionLossRadial radial)
        {
            if (radial.TransmissionLoss == null)
                throw new ApplicationException("TransmissionLossFieldData: Attempt to add a new radial that is not completely loaded into memory.  This operation is not supported.");
            if (((Depths == null) || (Ranges == null)) &&
                ((radial.Depths != null) && (radial.Ranges != null)))
            {
                Depths = radial.Depths;
                Ranges = radial.Ranges;
            }
            if (Radials == null) Radials = new List<TransmissionLossRadial>();
            Radials.Add(radial);
            Radials.Sort();
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
                    stream.Write(Latitude);
                    stream.Write(Longitude);
                    stream.Write(SourceDepth);
                    stream.Write(VerticalBeamWidth);
                    stream.Write(DepressionElevationAngle);
                    stream.Write(LowFrequency);
                    stream.Write(HighFrequency);
                    stream.Write(MaxCalculationDepth);
                    stream.Write(Radius);
                    stream.Write(Depths.Count);
                    foreach (var depth in Depths)
                        stream.Write(depth);
                    stream.Write(Ranges.Count);
                    foreach (var range in Ranges)
                        stream.Write(range);
                    _saved = true;
                }
                stream.Write(Radials.Count);
                foreach (var radial in Radials)
                    radial.Save(stream);
            }
        }

        const UInt32 Magic = 0x93f34525;
        List<PieSlice> _pieSlices = new List<PieSlice>();
        bool _dataIsLoaded;
        bool _saved;

        public bool Equals(SoundSource soundSource)
        {
            var location = new EarthCoordinate(Latitude, Longitude);
            return (SourceLevel.Equals(soundSource.SourceLevel) && location.Equals(soundSource) && soundSource.AcousticProperties.Equals(this));
        }
    }
}