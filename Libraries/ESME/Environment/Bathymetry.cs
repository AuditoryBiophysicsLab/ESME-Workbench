using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment.NAVO;
using HRC.Collections;
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
            char[] separators = { ' ' };
            var samples = new List<EarthCoordinate<float>>();
            System.Diagnostics.Debug.WriteLine("{0}: FromYXZ: About to read bathymetry file {1}", DateTime.Now, Path.GetFileName(fileName));
            var lineCount = 0;
            using (var stream = new StreamReader(File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)))
            {
                var curLine = stream.ReadLine();
                while (curLine != null)
                {
                    lineCount++;
                    var fields = curLine.Split(separators, StringSplitOptions.RemoveEmptyEntries);
                    var latitude = double.Parse(fields[0]);
                    var longitude = double.Parse(fields[1]);
                    var depth = float.Parse(fields[2]) * scaleFactor;
                    samples.Add(new EarthCoordinate<float>(latitude, longitude, depth));
                    curLine = stream.ReadLine();
                }
            }
            System.Diagnostics.Debug.WriteLine("{0}: FromYXZ: Finished readying bathymetry file {1}.  Line count: {2}", DateTime.Now, Path.GetFileName(fileName), lineCount);
            var result = new Bathymetry();
            result.Samples.AddRange(samples);
            return result;
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
            if (Count != 0) return;
            Add(new T {Resolution = 0.05f});
            Add(new T {Resolution = 0.10f});
            Add(new T {Resolution = 0.50f});
            Add(new T {Resolution = 1.00f});
            Add(new T {Resolution = 2.00f});
        }

        public T this[float resolution] { get { return Find(t => t.Resolution == resolution); } }
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
            if (Count != 0) return;
            Add(new T { TimePeriod = NAVOTimePeriod.Invalid });
            Add(new T { TimePeriod = NAVOTimePeriod.January });
            Add(new T { TimePeriod = NAVOTimePeriod.February });
            Add(new T { TimePeriod = NAVOTimePeriod.March });
            Add(new T { TimePeriod = NAVOTimePeriod.April });
            Add(new T { TimePeriod = NAVOTimePeriod.May });
            Add(new T { TimePeriod = NAVOTimePeriod.June});
            Add(new T { TimePeriod = NAVOTimePeriod.July });
            Add(new T { TimePeriod = NAVOTimePeriod.August });
            Add(new T { TimePeriod = NAVOTimePeriod.September });
            Add(new T { TimePeriod = NAVOTimePeriod.October });
            Add(new T { TimePeriod = NAVOTimePeriod.November });
            Add(new T { TimePeriod = NAVOTimePeriod.December });
            Add(new T { TimePeriod = NAVOTimePeriod.Spring });
            Add(new T { TimePeriod = NAVOTimePeriod.Summer });
            Add(new T { TimePeriod = NAVOTimePeriod.Fall });
            Add(new T { TimePeriod = NAVOTimePeriod.Winter });
            Add(new T { TimePeriod = NAVOTimePeriod.Cold });
            Add(new T { TimePeriod = NAVOTimePeriod.Warm });
        }

        public T this[NAVOTimePeriod timePeriod] { get { return Find(t => t.TimePeriod == timePeriod); } }
    }

    public class AvailableTimePeriods : TimePeriods<AvailableTimePeriod> { }
    public class SelectedTimePeriods : TimePeriods<SelectedTimePeriod> { }
}