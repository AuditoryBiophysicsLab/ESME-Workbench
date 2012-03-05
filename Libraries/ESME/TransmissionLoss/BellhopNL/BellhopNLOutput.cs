using System;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using ESME.Environment;
using HRC.Navigation;

namespace ESME.TransmissionLoss.BellhopNL
{
    [Serializable]
    public class BellhopNLOutput
    {
        //raw output
        public double[, ,] Waveforms { get; set; }
        public double[] Ranges { get; set; }
        public double[] Depths { get; set; }


        //REFMS-mimic members.
        public Geo Location { get; set; }
        public string ModeName { get; set; }
        public TimePeriod TimePeriod { get; set; }
        public Geo SVPLocation { get; set; }
        public double ChargeDepth { get; set; }
        public double OutputTime { get; set; }
        public double PeakEnergy { get;  set; }
        public double MaxEnergy { get;  set; }
        public double[] EFD { get;  set; }
        public double[] ThirdOctaveCenterFrequencies { get;  set; }
       
        public void Save(string filename)
        {
            using (var stream = new FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.None)) new BinaryFormatter().Serialize(stream,this);
        }
        public static BellhopNLOutput Load(string filename)
        {
            var result = new BellhopNLOutput();
            using (var stream = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.None)) result = (BellhopNLOutput)new BinaryFormatter().Deserialize(stream);//result.Waveforms=(double[,,])new BinaryFormatter().Deserialize(stream);
            return result;
        }
    }
}