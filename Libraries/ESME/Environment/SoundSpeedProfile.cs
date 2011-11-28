using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    public class SoundSpeedProfileAverager : EarthCoordinate<DepthValuePairs<AverageDatum>>
    {
        public SoundSpeedProfileAverager()
        {
            Data = new DepthValuePairs<AverageDatum>();
        }

        public SoundSpeedProfileAverager(SoundSpeedProfile profile) : base(profile)
        {
            Data = new DepthValuePairs<AverageDatum>();
            Add(profile);
        }

        public void Add(SoundSpeedProfile profile)
        {
            if (Data.Count == 0)
                foreach (var datum in profile.Data)
                { 
                    Data.Add(new DepthValuePair<AverageDatum>(datum.Depth, new AverageDatum(datum.Value)));
                }
            else
                foreach (var datum in profile.Data)
                {
                    var averagerAtDepth = Data[datum.Depth];
                    if (averagerAtDepth != null) averagerAtDepth.Value.Add(datum.Value);
                    else Data.Add(new DepthValuePair<AverageDatum>(datum.Depth, new AverageDatum(datum.Value)));
                }
        }

        public SoundSpeedProfile Average 
        {
            get
            {
                var result = new SoundSpeedProfile(this);
                foreach (var datum in Data)
                    result.Data.Add(new DepthValuePair<float>(datum.Depth, datum.Value.Average));
                return result;
            }
        }
    }


    public class AverageDatum
    {
        public AverageDatum()
        {
            Value = 0;
            Count = 0;
        }

        public AverageDatum(float value)
        {
            Value = value;
            Count = 1;
        }

        public float Value { get; set; }
        public int Count { get; private set; }

        public void Add(float newValue)
        {
            Value += newValue;
            Count++;
        }

        public float Average { get { return Value / Count; } }
    }

    [Serializable]
    public class SoundSpeedProfile : EarthCoordinate<DepthValuePairs<float>>
    {
        public SoundSpeedProfile()
        {
            Data = new DepthValuePairs<float>();
            Messages = new List<string>();
        }
        public SoundSpeedProfile(Geo location) : base(location)
        {
            Data = new DepthValuePairs<float>();
            Messages = new List<string>();
        }

        #region public List<string> Messages { get; set; }

        public List<string> Messages
        {
            get { return _messages; }
            set
            {
                if (_messages == value) return;
                _messages = value;
            }
        }

        [NonSerialized]
        List<string> _messages;

        #endregion

        /// <summary>
        /// Extrapolates the current sound speed profile to the given depth, in one step, using the provided temperature and salinity profile
        /// </summary>
        /// <param name="newMaxDepth"></param>
        /// <param name="temperatureProfile"></param>
        /// <param name="salinityProfile"></param>
        public void Extend(float newMaxDepth, SoundSpeedProfile temperatureProfile, SoundSpeedProfile salinityProfile)
        {
            if (newMaxDepth < Data.MaxDepth) throw new ApplicationException(string.Format("SoundSpeedProfile.Extend: Given depth {0} is less than current maximum depth {1}", newMaxDepth, Data.MaxDepth));

            //System.Diagnostics.Debug.WriteLine("Extrapolating SSP {0} from data depth of {1}m to bathymetry depth of {2}m", this, MaxDepth, newMaxDepth);
            //System.Diagnostics.Debug.WriteLine("  Initial depth vector length: {0}", Depths.Length);

            var tempDataCount = temperatureProfile.Data.Count;
            var tempD = temperatureProfile.Data[tempDataCount - 1].Value;
            var tempD1 = temperatureProfile.Data[tempDataCount - 2].Value;

            var salinity = salinityProfile.Data.Last().Value;

            var tempDiff = tempD1 - tempD;
            var newTemp = tempD - tempDiff;
            var soundSpeed = ChenMilleroLi.SoundSpeed(this, newMaxDepth, newTemp, salinity);
            Data.Add(new DepthValuePair<float>(newMaxDepth, soundSpeed));
        }

        /// <summary>
        /// Extends the current sound speed profile to the same depth as a given template, adjusting the copied template values to ensure a
        /// smooth curve in the profile
        /// </summary>
        /// <param name="templateSSP"></param>
        public void Extend(SoundSpeedProfile templateSSP)
        {
            //System.Diagnostics.Debug.WriteLine("Extending SSP {0} to new depth {1}", this, templateSSP.MaxDepth);

            if (templateSSP.Data.MaxDepth > Data.MaxDepth)
            {
                if (Data.Count == 0)
                {
                    //System.Diagnostics.Debug.WriteLine("  Original SSP is zero length, copying templateSSP.");
                    Data.AddRange(templateSSP.Data);
                }
                else
                {
                    //System.Diagnostics.Debug.WriteLine("  Original SSP depth vector length: {0} ({1}m)", Depths.Length, MaxDepth);
                    if (Data.Count > templateSSP.Data.Count) System.Diagnostics.Debugger.Break();
                    var myProfileLength = Data.Count;
                    var templateProfileLength = templateSSP.Data.Count;
                    var shallowSpeed = Data.Last().Value;
                    var deepSpeedAtSameDepth = templateSSP.Data[myProfileLength - 1].Value;
                    //System.Diagnostics.Debug.WriteLine("  Original soundspeed at {0}m: {1}", MaxDepth, shallowSpeed);
                    //System.Diagnostics.Debug.WriteLine("  Template soundspeed at {0}m: {1}", MaxDepth, deepSpeedAtSameDepth);

                    var ssDiff = deepSpeedAtSameDepth - shallowSpeed;
                    //System.Diagnostics.Debug.WriteLine("  Delta    soundspeed at {0}m: {1}", MaxDepth, ssDiff);

                    for (var speedIndex = myProfileLength; speedIndex < templateProfileLength; speedIndex++)
                    {
                        var newSpeed = templateSSP.Data[speedIndex].Value - ssDiff;
                        //System.Diagnostics.Debug.WriteLine("    Template soundspeed at {0}m: Original: {1} Adjusted: {2}", templateSSP.Depths[speedIndex], templateSSP.SoundSpeeds[speedIndex], newSpeed);
                        Data.Add(new DepthValuePair<float>(templateSSP.Data[speedIndex].Depth, newSpeed));
                    }
                }
            }
            //System.Diagnostics.Debug.WriteLine("  New SSP depth vector length: {0}", Depths.Length);
        }

        public static new SoundSpeedProfile Deserialize(BinaryReader reader)
        {
            var result = new SoundSpeedProfile(Geo.Deserialize(reader));
            var itemCount = reader.ReadInt32();
            for (var i = 0; i < itemCount; i++)
                result.Data.Add(new DepthValuePair<float>(reader.ReadSingle(), reader.ReadSingle()));
            return result;
        }

        public new void Serialize(BinaryWriter writer)
        {
            base.Serialize(writer);
            writer.Write(Data.Count);
            foreach (var item in Data)
            {
                writer.Write(item.Depth);
                writer.Write(item.Value);
            }
        }

    }

    [Serializable]
    public class DepthValuePairs<T> : List<DepthValuePair<T>>
    {
        public DepthValuePairs() { }
        
        public DepthValuePair<T> this[float depth] { get { return Find(d => d.Depth == depth); } }
        new public DepthValuePair<T> this[int depthIndex] { get { return base[depthIndex]; } }
        public IEnumerable<float> Depths { get { return this.Select(pair => pair.Depth); } }
        public IEnumerable<T> Values { get { return this.Select(pair => pair.Value); } }
        public double MaxDepth { get { return Depths.Last(); } }

        new public void Add(DepthValuePair<T> item)
        {
            base.Add(item);
            Sort();
        }

        new public void AddRange(IEnumerable<DepthValuePair<T>> collection)
        {
            base.AddRange(collection);
            Sort();
        }
    }

    [Serializable]
    public class DepthValuePair<T> : IComparable<DepthValuePair<T>>, IComparer<DepthValuePair<T>>
    {
        public DepthValuePair() { }

        public DepthValuePair(float depth, T value)
        {
            Depth = depth;
            Value = value;
        }

        public float Depth { get; set; }
        public T Value { get; set; }

        public int CompareTo(DepthValuePair<T> other)
        {
            return Depth.CompareTo(other.Depth);
        }

        public int Compare(DepthValuePair<T> x, DepthValuePair<T> y)
        {
            return x.CompareTo(y);
        }

        public static DepthValuePair<T> Deserialize(BinaryReader reader, Func<BinaryReader, T> readFunc)
        {
            return new DepthValuePair<T>(reader.ReadSingle(), readFunc(reader));
        }
    }
}