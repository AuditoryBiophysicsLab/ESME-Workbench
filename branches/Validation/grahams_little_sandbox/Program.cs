using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ESME;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.CASS;
using HRC;


namespace grahams_little_sandbox
{
    class Program
    {
        static void Main(string[] args) { 
            var cassFile = @"C:\Users\Graham Voysey\Desktop\testoutput.BIN";

            var cassresult = CASSOutput.Load(cassFile,false);

            var tlf = TransmissionLossField.FromCASS(cassresult);

            var cassOut = @"C:\Users\Graham Voysey\Desktop\gvtestoutput.BIN";
            CASSOutput.Write(cassOut,tlf);
            var outresult = CASSOutput.Load(cassOut,false);


            // var tlf = TransmissionLossField.FromCASS(CASSOutput.Load(cassFile));
        }
    }
}
