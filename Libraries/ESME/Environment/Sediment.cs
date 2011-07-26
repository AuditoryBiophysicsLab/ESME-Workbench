using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Sediment
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<SedimentSample>.ReferencedTypes) { typeof (SedimentSampleBase) };

        public EnvironmentData<SedimentSample> Samples { get; private set; }

        public Sediment()
        {
            Samples = new EnvironmentData<SedimentSample>();
        }

        public static Sediment Load(string filename)
        {
            return new Sediment { Samples = XmlSerializer<EnvironmentData<SedimentSample>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<EnvironmentData<SedimentSample>> { Data = Samples };
            serializer.Save(filename, ReferencedTypes);
        }
    }

    public class SedimentSample : EarthCoordinate<SedimentSampleBase>, IComparable<SedimentSample>
    {
        public SedimentSample() { }

        public SedimentSample(double latitude, double longitude, SedimentSampleBase sample) : base(latitude, longitude, sample) { }

        public static implicit operator SedimentType(SedimentSample sedimentSample)
        {
            return SedimentTypes.Find(sedimentSample.Data.SampleValue);
        }

        public int CompareTo(SedimentSample other)
        {
            return Data.CompareTo(other.Data);
        }
    }

    public class SedimentSampleBase : IComparable<SedimentSampleBase>
    {
        public short SampleValue { get; set; }
        public int CompareTo(SedimentSampleBase other)
        {
            return SampleValue.CompareTo(other.SampleValue);
        }
    }
}
