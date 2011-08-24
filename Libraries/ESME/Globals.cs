using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using ESME.Data;
using ESME.TransmissionLoss;

namespace ESME
{
    public static class Globals
    {
        static Globals()
        {
            Configuration = new Configuration();
        }

        public static string UserFolder
        {
            get { return Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "ESME WorkBench"); }
        }

        public static string Filter(this string s, Func<char, bool> trueIfKeep)
        {
            if (!string.IsNullOrEmpty(s))
            {
                var sb = new StringBuilder(s.Length);
                foreach (var c in s.Where(trueIfKeep)) sb.Append(c);

                return sb.ToString();
            }
            return s;
        }

        public static AppSettings AppSettings { get; set; }

        public static WorkDirectories WorkDirectories { get; set; }

        public static Configuration Configuration { get; private set; }
        
        public static List<TransmissionLossAlgorithm> ValidTransmissionLossAlgorithms = new List<TransmissionLossAlgorithm>
        {
            TransmissionLossAlgorithm.CASS,
            TransmissionLossAlgorithm.RAM,
            TransmissionLossAlgorithm.Bellhop,
            TransmissionLossAlgorithm.RAMGEO
        };
    }
}