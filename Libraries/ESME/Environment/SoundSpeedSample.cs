using System;
using System.IO;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface ISoundSpeedSample<T>
    {
        T Extend(T shallowerSample, float extendedSampleDepth);
        T Deserialize(BinaryReader reader);
    }
    public class SoundSpeedSample : IComparable<SoundSpeedSample>
    {
        /// <summary>
        /// Construct a SoundSpeedSample
        /// </summary>
        /// <param name="depth">Depth of the sample, in meters</param>
        /// <param name="soundSpeed">Sound speed, in meters per second</param>
        public SoundSpeedSample(float depth, float soundSpeed)
        {
            Depth = depth;
            SoundSpeed = soundSpeed;
        }

        public SoundSpeedSample() 
        {
            Depth = float.NaN;
            SoundSpeed = float.NaN;
        }

        /// <summary>
        /// Depth, in meters
        /// </summary>
        public float Depth { get; set; }

        /// <summary>
        /// Sound speed, in meters per second
        /// </summary>
        public float SoundSpeed { get; set; }

        /// <summary>
        /// Read a SoundSpeedSample from a BinaryReader
        /// </summary>
        /// <param name="reader"></param>
        /// <returns></returns>
        public static SoundSpeedSample DeserializeStatic(BinaryReader reader) { return new SoundSpeedSample(reader.ReadSingle(), reader.ReadSingle()); }
        public SoundSpeedSample Deserialize(BinaryReader reader) { return DeserializeStatic(reader); }

        /// <summary>
        /// Write a SoundSpeedSample to a BinaryWriter
        /// </summary>
        /// <param name="writer"></param>
        public void Serialize(BinaryWriter writer)
        {
            writer.Write(Depth);
            writer.Write(SoundSpeed);
        }

        internal virtual void Calculate(Geo location){}

        public int CompareTo(SoundSpeedSample other) { return Depth.CompareTo(other.Depth); }

        public static SoundSpeedSample ExtendStatic(SoundSpeedSample shallowerSample, SoundSpeedSample deeperSample, float extendedSampleDepth)
        {
            if (deeperSample.Depth <= shallowerSample.Depth) throw new ArgumentException("shallowerSample must be at a shallower (lower) depth than deeperSample", "shallowerSample");
            if (extendedSampleDepth <= deeperSample.Depth) throw new ArgumentException("deeperSample must be at a shallower (lower) depth than extendedSampleDepth", "deeperSample");
            return new SoundSpeedSample(extendedSampleDepth, deeperSample.SoundSpeed);
        }

        public SoundSpeedSample Extend(SoundSpeedSample shallowerSample, float extendedSampleDepth)
        {
            return ExtendStatic(shallowerSample, this, extendedSampleDepth);
        }
    }

    public class AverageSoundSpeedSample : SoundSpeedSample
    {
        public AverageSoundSpeedSample()
        {
            Value = 0;
            Count = 0;
        }

        public AverageSoundSpeedSample(SoundSpeedSample sample)
        {
            Value = sample.SoundSpeed;
            Count = 1;
        }

        public float Value { get; set; }
        public int Count { get; private set; }

        public void Add(SoundSpeedSample sample)
        {
            Value += sample.SoundSpeed;
            Count++;
        }

        public new float SoundSpeed
        {
            get { return Value / Count; }
            set { throw new InvalidOperationException("Cannot set SoundSpeed in an AverageSoundSpeed object"); }
        }
    }

    public class GDEMSoundSpeedSample : SoundSpeedSample
    {
        public GDEMSoundSpeedSample() 
        {
            Temperature = float.NaN;
            Salinity = float.NaN;
        }

        public GDEMSoundSpeedSample(float depth, float temperature, float salinity) : base(depth, float.NaN)
        {
            Temperature = temperature;
            Salinity = salinity;
        }

        /// <summary>
        /// Temperature, in Celsius
        /// </summary>
        public float Temperature { get; set; }

        /// <summary>
        /// Salinity, in parts per thousand (PPT)
        /// </summary>
        public float Salinity { get; set; }

        /// <summary>
        /// Read a GDEMSoundSpeedSample from a BinaryReader
        /// </summary>
        /// <param name="reader"></param>
        /// <returns></returns>
        public static new GDEMSoundSpeedSample DeserializeStatic(BinaryReader reader)
        {
            var result = (GDEMSoundSpeedSample)SoundSpeedSample.DeserializeStatic(reader);
            result.Temperature = reader.ReadSingle();
            result.Salinity = reader.ReadSingle();
            return result;
        }
        public new GDEMSoundSpeedSample Deserialize(BinaryReader reader) { return DeserializeStatic(reader); }

        /// <summary>
        /// Write a GDEMSoundSpeedSample to a BinaryWriter
        /// </summary>
        /// <param name="writer"></param>
        public new void Serialize(BinaryWriter writer)
        {
            base.Serialize(writer);
            writer.Write(Temperature);
            writer.Write(Salinity);
        }

        internal override void Calculate(Geo location)
        {
            if (float.IsNaN(Depth) || float.IsNaN(Temperature) || float.IsNaN(Salinity)) return;
            SoundSpeed = ChenMilleroLi.SoundSpeed(location, Depth, Temperature, Salinity);
        }

        public static GDEMSoundSpeedSample ExtendStatic(GDEMSoundSpeedSample shallowerSample, GDEMSoundSpeedSample deeperSample, float extendedSampleDepth)
        {
            if (deeperSample.Depth <= shallowerSample.Depth) throw new ArgumentException("shallowerSample must be at a shallower (lower) depth than deeperSample", "shallowerSample");
            if (extendedSampleDepth <= deeperSample.Depth) throw new ArgumentException("deeperSample must be at a shallower (lower) depth than extendedSampleDepth", "deeperSample");

            return new GDEMSoundSpeedSample(extendedSampleDepth, deeperSample.Temperature - (shallowerSample.Temperature - deeperSample.Temperature), deeperSample.Salinity);
        }

        public GDEMSoundSpeedSample Extend(GDEMSoundSpeedSample shallowerSample, float extendedSampleDepth) { return ExtendStatic(shallowerSample, this, extendedSampleDepth); }
    }
}
