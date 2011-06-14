using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class Bathymetry
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<EarthCoordinate<float>>.ReferencedTypes) { typeof(SedimentSampleBase) };

        public EnvironmentData<EarthCoordinate<float>> Samples { get; private set; }

        public Bathymetry()
        {
            Samples = new EnvironmentData<EarthCoordinate<float>>();
        }

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
}