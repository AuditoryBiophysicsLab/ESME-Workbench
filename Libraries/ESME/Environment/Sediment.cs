using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using ESME.Model;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Sediment
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<SedimentSample>.ReferencedTypes) { typeof (SedimentSampleBase) };

        public EnvironmentData<SedimentSample> Samples { get; set; }

        public Sediment()
        {
            Samples = new EnvironmentData<SedimentSample>();
        }

        public static Task<Sediment> LoadAsync(string filename)
        {
            return TaskEx.Run(() => Load(filename));
        }

        public static Sediment Load(string filename)
        {
            //return new Sediment { Samples = XmlSerializer<EnvironmentData<SedimentSample>>.Load(filename, ReferencedTypes) };
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                return new Sediment { Samples = (EnvironmentData<SedimentSample>)formatter.Deserialize(stream) };
            }
        }

        public void Save(string filename)
        {
            //var serializer = new XmlSerializer<EnvironmentData<SedimentSample>> { Data = Samples };
            //serializer.Save(filename, ReferencedTypes);
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, Samples);
            }
        }
    }

    [Serializable]
    public class SedimentSample : EarthCoordinate<SedimentSampleBase>, IComparable<SedimentSample>
    {
        public SedimentSample() { }

        public SedimentSample(Geo location, SedimentSampleBase sample) : base(location.Latitude, location.Longitude, sample) { }
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

    [Serializable]
    public class SedimentSampleBase : IComparable<SedimentSampleBase>
    {
        public short SampleValue { get; set; }
        public int CompareTo(SedimentSampleBase other)
        {
            return SampleValue.CompareTo(other.SampleValue);
        }
    }
}
