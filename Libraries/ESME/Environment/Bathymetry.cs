using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Bathymetry
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<EarthCoordinate<float>>.ReferencedTypes) { typeof(SedimentSampleBase) };

        public EnvironmentData<EarthCoordinate<float>> Samples { get; private set; }

        public Bathymetry() { Samples = new EnvironmentData<EarthCoordinate<float>>(); }

        public static Bathymetry Load(string filename)
        {
            return new Bathymetry { Samples = XmlSerializer<EnvironmentData<EarthCoordinate<float>>>.Load(filename, ReferencedTypes) };
        }

        public void Save(string filename)
        {
            var serializer = new XmlSerializer<EnvironmentData<EarthCoordinate<float>>> { Data = Samples };
            serializer.Save(filename, ReferencedTypes);
        }

        public static Bathymetry FromYXZ(string fileName, float scaleFactor)
        {
            var result = new Bathymetry();
            char[] separators = { ' ' };
            using (var stream = new StreamReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var curLine = stream.ReadLine();
                while (curLine != null)
                {
                    var fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    result.Samples.Add(new EarthCoordinate<float>(double.Parse(fields[0]), double.Parse(fields[1]), float.Parse(fields[2]) * scaleFactor));
                    curLine = stream.ReadLine();
                }
                return result;
            }
        }

        public void ToYXZ(string fileName, float scaleFactor)
        {
            using (var stream = new StreamWriter(fileName, false, System.Text.Encoding.ASCII))
            {
                foreach (var sample in Samples)
                    stream.WriteLine("{0:0.0000} {1:0.0000} {2:0.00}", sample.Latitude, sample.Longitude, sample.Data * scaleFactor);
            }
        }

        public EarthCoordinate<float> Minimum
        {
            get
            {
                var minPoint = Samples[0];
                foreach (var sample in Samples.Where(sample => sample.Data < minPoint.Data)) minPoint = sample;
                return minPoint;
            }
        }

        public EarthCoordinate<float> Maximum
        {
            get
            {
                var maxPoint = Samples[0];
                foreach (var sample in Samples.Where(sample => sample.Data > maxPoint.Data)) maxPoint = sample;
                return maxPoint;
            }
        }
    }

    public class AvailableResolution
    {
        public AvailableResolution() { IsAvailable = false; }

        /// <summary>
        /// Resolution of the described dataset, in degrees
        /// </summary>
        public float Resolution { get; set; }

        /// <summary>
        /// True if data is available at this resolution
        /// </summary>
        public bool IsAvailable { get; set; }
    }

    public class SelectedResolution : AvailableResolution
    {
        public SelectedResolution() { IsSelected = false; }

        /// <summary>
        /// True if the this resolution has been selected by the user
        /// </summary>
        public bool IsSelected { get; set; }
    }

    public class Resolutions<T> : List<T> where T: AvailableResolution, new()
    {
        public Resolutions()
        {
            base.Add(new T { Resolution = 0.05f });
            base.Add(new T { Resolution = 0.10f });
            base.Add(new T { Resolution = 0.50f });
            base.Add(new T { Resolution = 1.00f });
            base.Add(new T { Resolution = 2.00f });
        }

        public T this[float resolution] { get { return Find(t => t.Resolution == resolution); } }

        public new T this[int index] { get { return base[index]; } }
        public new void Add(T item) { throw new NotImplementedException(""); }
        public new void AddRange(IEnumerable<T> collection) { throw new NotImplementedException(""); }
        public new void Clear() { throw new NotImplementedException(""); }
        public new bool Remove(T item) { throw new NotImplementedException(""); }
        public new int RemoveAll(Predicate<T> item) { throw new NotImplementedException(""); }
        public new void RemoveAt(int index) { throw new NotImplementedException(""); }
        public new void RemoveRange(int index, int count) { throw new NotImplementedException(""); }
    }

    public class AvailableResolutions : Resolutions<AvailableResolution> { }
    public class SelectedResolutions : Resolutions<SelectedResolution> { }

    public class AvailableTimePeriod
    {
        public AvailableTimePeriod()
        {
            IsAvailable = false;
        }

        public NAVOTimePeriod TimePeriod { get; set; }
        public bool IsAvailable { get; set; }
    }

    public class SelectedTimePeriod : AvailableTimePeriod
    {
        public SelectedTimePeriod()
        {
            IsSelected = false;
        }

        public bool IsSelected { get; set; }
    }

    public class TimePeriods<T> : List<T> where T: AvailableTimePeriod, new()
    {
        public TimePeriods()
        {
            base.Add(new T { TimePeriod = 0 });
            base.Add(new T { TimePeriod = NAVOTimePeriod.January });
            base.Add(new T { TimePeriod = NAVOTimePeriod.February });
            base.Add(new T { TimePeriod = NAVOTimePeriod.March });
            base.Add(new T { TimePeriod = NAVOTimePeriod.April });
            base.Add(new T { TimePeriod = NAVOTimePeriod.May });
            base.Add(new T { TimePeriod = NAVOTimePeriod.June});
            base.Add(new T { TimePeriod = NAVOTimePeriod.July });
            base.Add(new T { TimePeriod = NAVOTimePeriod.August });
            base.Add(new T { TimePeriod = NAVOTimePeriod.September });
            base.Add(new T { TimePeriod = NAVOTimePeriod.October });
            base.Add(new T { TimePeriod = NAVOTimePeriod.November });
            base.Add(new T { TimePeriod = NAVOTimePeriod.December });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Spring });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Summer });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Fall });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Winter });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Cold });
            base.Add(new T { TimePeriod = NAVOTimePeriod.Warm });
        }

        public T this[NAVOTimePeriod timePeriod] { get { return Find(t => t.TimePeriod == timePeriod); } }

        public new T this[int index] { get { return base[index]; } }
        public new void Add(T item) { throw new NotImplementedException(""); }
        public new void AddRange(IEnumerable<T> collection) { throw new NotImplementedException(""); }
        public new void Clear() { throw new NotImplementedException(""); }
        public new bool Remove(T item) { throw new NotImplementedException(""); }
        public new int RemoveAll(Predicate<T> item) { throw new NotImplementedException(""); }
        public new void RemoveAt(int index) { throw new NotImplementedException(""); }
        public new void RemoveRange(int index, int count) { throw new NotImplementedException(""); }
    }

    public class AvailableTimePeriods : TimePeriods<AvailableTimePeriod> { }
    public class SelectedTimePeriods : TimePeriods<SelectedTimePeriod> { }
}