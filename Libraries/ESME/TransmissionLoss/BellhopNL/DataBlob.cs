using System;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;

namespace ESME.TransmissionLoss.BellhopNL
{
    [Serializable]
    public class DataBlob
    {
        //bellhopNL members
        public double ChargeDepth { get;set; }
        public double ChargeMass { get; set; }
        public double OutputFreq { get; set; }
        public double OutputTime { get; set; }
        public string ModelType { get; set; }

        //bellhop members
        public float WaterDepth { get; set; }
        public float CalculationRange { get; set; }
        public string EnvFilename { get; set; }
        public string BellhopConfiguration { get; set; }
        public string BottomProfile { get; set; }
        public double[,] TopReflectionCoefficients { get; set; }
        public float Bearing { get; set; }

        public static DataBlob Load(string filename)
        {
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read)) return (DataBlob)formatter.Deserialize(stream);
        }
        public void Save(string filename)
        {
            var formatter = new BinaryFormatter();
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None))
            {
                formatter.Serialize(stream, this);
            }
        }
    }
}