﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment.NAVO;
using HRC.Navigation;

namespace ESME.Environment
{
    public class SoundSpeedProfileAverager : EarthCoordinate<DepthValuePairs<AverageDatum>>
    {
        public SoundSpeedProfileAverager() { Data = new DepthValuePairs<AverageDatum>(); }
        public SoundSpeedProfileAverager(Geo location) : base(location) { Data = new DepthValuePairs<AverageDatum>(); }

        public void Add(SoundSpeedProfile profile)
        {
            if (Data.Count == 0)
                foreach (var datum in profile.Data)
                { 
                    if (datum.Depth > 1000)
                        Debug.WriteLine("Break!");
                    Data.Add(new DepthValuePair<AverageDatum>(datum.Depth, new AverageDatum(datum.Value)));
                }
            else
                foreach (var datum in profile.Data)
                {
                    if (datum.Depth > 1000)
                        Debug.WriteLine("Break!");
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

    public class SoundSpeedProfile : EarthCoordinate<DepthValuePairs<float>>
    {
        public SoundSpeedProfile() { Data = new DepthValuePairs<float>(); }
        public SoundSpeedProfile(Geo location) : base(location) { Data = new DepthValuePairs<float>(); }

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
    }

    public class DepthValuePairs<T> : List<DepthValuePair<T>>
    {
        public DepthValuePairs() { }
        
        [XmlIgnore]
        public DepthValuePair<T> this[float depth] { get { return Find(d => d.Depth == depth); } }
        new public DepthValuePair<T> this[int depthIndex] { get { return base[depthIndex]; } }
        [XmlIgnore]
        public IEnumerable<float> Depths { get { return this.Select(pair => pair.Depth); } }
        [XmlIgnore]
        public IEnumerable<T> Values { get { return this.Select(pair => pair.Value); } }
        [XmlIgnore]
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

    public class DepthValuePair<TValue> : IComparable<DepthValuePair<TValue>>, IComparer<DepthValuePair<TValue>>
    {
        public DepthValuePair() { }

        public DepthValuePair(float depth, TValue value)
        {
            Depth = depth;
            Value = value;
        }

        public float Depth { get; set; }
        public TValue Value { get; set; }

        public int CompareTo(DepthValuePair<TValue> other)
        {
            return Depth.CompareTo(other.Depth);
        }

        public int Compare(DepthValuePair<TValue> x, DepthValuePair<TValue> y)
        {
            return x.CompareTo(y);
        }
    }
}