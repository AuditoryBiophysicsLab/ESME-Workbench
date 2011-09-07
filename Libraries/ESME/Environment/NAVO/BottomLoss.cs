using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using HRC.Navigation;
using System.Threading.Tasks;

namespace ESME.Environment.NAVO
{
    public class BottomLoss
    {
        static readonly List<Type> ReferencedTypes = new List<Type>(EnvironmentData<BottomLossSample>.ReferencedTypes) { typeof(BottomLossSample) };

        public EnvironmentData<BottomLossSample> Samples { get; private set; }

        public BottomLoss()
        {
            Samples = new EnvironmentData<BottomLossSample>();
        }

        public static BottomLoss Load(string filename)
        {
            //return new BottomLoss { Samples = XmlSerializer<EnvironmentData<BottomLossSample>>.Load(filename, ReferencedTypes) };
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                return new BottomLoss { Samples = (EnvironmentData<BottomLossSample>)formatter.Deserialize(stream) };
            }
        }

        public void Save(string filename)
        {
            //var serializer = new XmlSerializer<EnvironmentData<BottomLossSample>> { Data = Samples };
            //serializer.Save(filename, ReferencedTypes);
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, Samples);
            }
        }
    }

    [Serializable]
    public class BottomLossSample : EarthCoordinate<BottomLossData>
    {
        public BottomLossSample() { }

        public BottomLossSample(Geo location, BottomLossData sample) : base(location.Latitude, location.Longitude, sample) { }
        public BottomLossSample(double latitude, double longitude, BottomLossData sample) : base(latitude, longitude, sample) { }
    }

    // ReSharper disable InconsistentNaming
    [Serializable]
    public class BottomLossData 
    {
        public double CurveNumber { get; set; }
        public double RATIOD { get; set; }
        public double DLD { get; set; }
        public double RHOLD { get; set; }
        public double RHOSD { get; set; }
        public double GD { get; set; }
        public double BETAD { get; set; }
        public double FKZD { get; set; }
        public double FKZP { get; set; }
        public double BRFLD { get; set; }
        public double FEXP { get; set; }
        public double D2A { get; set; }
        public double ALF2A { get; set; }
        public double RHO2A { get; set; }
        public double SUBCRIT { get; set; }
        public double T2RH { get; set; }
        public double SEDTHK_M { get; set; }
        public double SEDTHK_S { get; set; }
    }
    // ReSharper restore InconsistentNaming
}