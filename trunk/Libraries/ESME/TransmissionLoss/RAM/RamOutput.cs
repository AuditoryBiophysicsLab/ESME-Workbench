using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace ESME.TransmissionLoss.RAM
{
    public class RamOutput
    {
        public RamOutput(string ramFilename)
        {
            using (var reader = new BinaryReader(new FileStream(ramFilename, FileMode.Open, FileAccess.Read)))
            {
            } // using
        }

        public string Title { get; private set; }
        public string PlotType { get; private set; }
        public float Xs { get; private set; }
        public float Ys { get; private set; }
        public float Frequency { get; private set; }
        public float[] SourceDepths { get; private set; }
        public float[] ReceiverDepths { get; private set; }
        public float[] ReceiverRanges { get; private set; }
        public float[,] TransmissionLoss { get; private set; }
        public float DataMax { get; private set; }
        public float DataMin { get; private set; }
        public float StatMax { get; private set; }
        public float StatMin { get; private set; }
        public float Mean { get; private set; }
        public float Median { get; private set; }
        public float Variance { get; private set; }
        public float StandardDeviation { get; private set; }

    }
}